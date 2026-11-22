# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Interactive map: click a Bronx block to see which offenses happen there

source("R_Scripts/DV_Bronx_Data.R")

library(plotly)


## 1. Data: one row per complaint, with readable offense names
full_names <- c("CRIMINAL MISCHIEF & RELATED OF" = "CRIMINAL MISCHIEF & RELATED OFFENSES",
                "OFFENSES AGAINST PUBLIC ADMINI" = "OFFENSES AGAINST PUBLIC ADMINISTRATION",
                "OFF. AGNST PUB ORD SENSBLTY &"  = "OFFENSES AGAINST PUBLIC ORDER",
                "HOMICIDE-NEGLIGENT,UNCLASSIFIE" = "HOMICIDE-NEGLIGENT, UNCLASSIFIED",
                "LOITERING/GAMBLING (CARDS, DIC" = "LOITERING/GAMBLING")

bronx_offenses <- bronx_street |>
  mutate(offense = if_else(ofns_desc %in% names(full_names),
                           full_names[ofns_desc], ofns_desc),
         offense = if_else(offense == "(null)" | is.na(offense), "Unknown",
                           tools::toTitleCase(tolower(offense))))


## 2. Panel content (HTML) for each block, and for the whole Bronx
sev_colors <- c(FELONY = "#1B2A4A", MISDEMEANOR = "#2E75B6", VIOLATION = "#9AA3AB")
sev_labels <- c(FELONY = "Felony", MISDEMEANOR = "Misdemeanor", VIOLATION = "Violation")

build_panels <- function(df) {

  # Top 8 offense types, with a bar for their share
  offenses <- df |>
    count(block, offense) |>
    group_by(block) |>
    mutate(share = n / sum(n)) |>
    arrange(desc(n), .by_group = TRUE) |>
    slice_head(n = 8) |>
    summarise(rows = paste0("<tr class='orow' data-off='", offense, "'><td class='o'>", offense, "</td>",
                            "<td class='b'><div style='width:", round(100 * share / max(share)), "%'></div></td>",
                            "<td class='n'>", format(n, big.mark = ","), "</td>",
                            "<td class='p'>", round(100 * share), "%</td></tr>",
                            collapse = ""))

  # Severity: one stacked bar + legend with percentages
  severity <- df |>
    filter(law_cat_cd %in% names(sev_colors)) |>
    count(block, law_cat_cd) |>
    mutate(law_cat_cd = factor(law_cat_cd, levels = names(sev_colors))) |>
    arrange(block, law_cat_cd) |>
    group_by(block) |>
    mutate(p = round(100 * n / sum(n))) |>
    summarise(sev = paste0("<div class='sevbar'>",
                           paste0("<div style='width:", p, "%;background:", sev_colors[as.character(law_cat_cd)], "'></div>", collapse = ""),
                           "</div><div class='sevleg'>",
                           paste0("<span><i style='background:", sev_colors[as.character(law_cat_cd)], "'></i>",
                                  sev_labels[as.character(law_cat_cd)], " ", p, "%</span>", collapse = ""),
                           "</div>"))

  # Busiest hour (time the offense happened)
  # Note: unknown times are sometimes recorded as 0h or 12h (see DV_Bronx_Data.R)
  peak <- df |>
    filter(!is.na(hour)) |>
    count(block, hour) |>
    group_by(block) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(block, peak = paste0(sprintf("%02d", hour), ":00-", sprintf("%02d", hour + 1), ":00 (",
                                   format(n, big.mark = ","), " complaints)"))

  df |>
    count(block, name = "total") |>
    left_join(offenses, by = "block") |>
    left_join(severity, by = "block") |>
    left_join(peak, by = "block") |>
    mutate(panel = paste0("<div class='big'>", format(total, big.mark = ","),
                          "<span> street complaints, Jan 2024 - Jun 2026</span></div>",
                          "<div class='sec'>Severity</div>", sev,
                          "<div class='sec'>Busiest hour</div><div>", peak, "</div>",
                          "<div class='sec'>Top offense types <span class='tip'>(click one to map it)</span></div>",
                          "<table>", rows, "</table>")) |>
    select(block, panel)
}

block_panels <- build_panels(bronx_offenses)

# Whole Bronx: same panel, treating the borough as one "block" (shown at the start)
bronx_panel <- build_panels(mutate(bronx_offenses, block = "BRONX"))$panel


## 3. One row per block 
# Precinct of each block = the most common precinct of its complaints
block_precinct <- bronx_offenses |>
  count(block, addr_pct_cd) |>
  group_by(block) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(block, precinct = addr_pct_cd)

# Top 3 offense types for the hover
block_top3 <- bronx_offenses |>
  count(block, offense) |>
  group_by(block) |>
  arrange(desc(n), .by_group = TRUE) |>
  summarise(top3 = paste0(head(offense, 3), " (", head(n, 3), ")", collapse = "<br>"))

# Most common offense type of each block (ties: the first in alphabetical order);
# used to map one offense type when it is clicked in the panel
block_top_offense <- bronx_offenses |>
  count(block, offense) |>
  arrange(block, desc(n), offense) |>
  group_by(block) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(block, top_offense = offense)

# Names of the 10 worst blocks (from Figures/EDA/Street_Top10_Intersections_Bronx.png,
block_names <- data.frame(
  block = c("40.816 -73.918", "40.81 -73.925", "40.862 -73.897",
            "40.838 -73.92", "40.84 -73.918", "40.831 -73.827",
            "40.849 -73.906", "40.869 -73.88", "40.862 -73.891",
            "40.887 -73.847"),
  name  = c("Willis Ave & 3rd Ave", "E 138th St & Alexander Ave", "Grand Concourse",
            "Jerome Ave & E 169th St", "Jerome Ave", "Barkley Ave",
            "Grand Concourse & Monroe Ave", "Mosholu Pkwy", "Webster Ave",
            "E 229th St"))

blocks <- bronx_offenses |>
  count(block, block_lat, block_lon) |>
  left_join(block_precinct, by = "block") |>
  left_join(block_top3, by = "block") |>
  left_join(block_top_offense, by = "block") |>
  left_join(block_panels, by = "block") |>
  left_join(block_names, by = "block") |>
  mutate(rank = min_rank(desc(n)),
         title = if_else(is.na(name), paste0("Block at ", block_lat, ", ", block_lon), name),
         hover = paste0("<b>", title, "</b><br>", n, " street complaints, precinct ", precinct,
                        "<br>", top3, "<br><i>Click for details</i>"),
         panel = paste0("<div class='ttl'>", title, "</div>",
                        "<div class='meta'>Precinct ", precinct, " &middot; rank ", rank,
                        " of ", format(n(), big.mark = ","), " blocks</div>", panel)) |>
  arrange(n)   

# Checks: one row per block, every complaint counted once
nrow(blocks) == n_distinct(blocks$block)  # TRUE
sum(blocks$n) == nrow(bronx_street)       # TRUE

# Blocks per most common offense type (shown when an offense is clicked)
count(blocks, top_offense, sort = TRUE) |> head(10)


## 4. Map
# Basemap: Esri World Light Gray Canvas (free, no API key). The street-name
# layer is drawn above the blocks so names stay readable when zooming in.
esri_base   <- "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}"
esri_labels <- "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"

color_ticks <- c(1, 5, 25, 100, 500)

fig_offense_map <- plot_ly(blocks,
                           type = "scattermapbox", mode = "markers",
                           lat = ~block_lat, lon = ~block_lon,
                           text = ~hover, hoverinfo = "text",
                           customdata = ~panel,
                           marker = list(size = 7, opacity = 0.85,
                                         color = ~log10(n),
                                         colorscale = list(c(0, "#ffeda0"), c(0.35, "#feb24c"),
                                                           c(0.65, "#e31a1c"), c(1, "#4a0013")),
                                         cmin = 0, cmax = log10(max(blocks$n)),
                                         colorbar = list(title = "Complaints<br>per block",
                                                         tickvals = log10(color_ticks),
                                                         ticktext = color_ticks,
                                                         len = 0.6))) |>
  layout(mapbox = list(style = "white-bg",
                       layers = list(list(below = "traces", sourcetype = "raster",
                                          source = list(esri_base)),
                                     list(sourcetype = "raster",
                                          source = list(esri_labels))),
                       center = list(lat = 40.845, lon = -73.875),
                       zoom = 11.4),
         margin = list(t = 10, b = 30, l = 390, r = 10),   # left: room for the panel
         annotations = list(list(text = "Data: NYPD Complaint Data, NYC Open Data. Basemap: Esri, HERE, Garmin, OpenStreetMap contributors.",
                                 x = 1, y = -0.03, xref = "paper", yref = "paper",
                                 xanchor = "right", showarrow = FALSE,
                                 font = list(size = 10, color = "grey")))) |>
  # Panel on the left + highlight of the clicked block (JavaScript)
  htmlwidgets::onRender("
    function(el, x, data) {
      var css = document.createElement('style');
      css.innerHTML =
        '.bx-panel{position:absolute;top:0;left:0;bottom:0;width:370px;overflow:auto;box-sizing:border-box;' +
        'padding:16px 18px;background:#fff;border-right:1px solid #e3e6ea;font:13px/1.4 -apple-system,Segoe UI,Helvetica,Arial,sans-serif;color:#1B2A4A;z-index:10}' +
        '.bx-panel h1{font-size:17px;margin:0 0 4px}' +
        '.bx-panel .hint{color:#6b7480;margin-bottom:14px}' +
        '.bx-panel .ttl{font-size:16px;font-weight:700}' +
        '.bx-panel .meta{color:#6b7480;margin:2px 0 10px}' +
        '.bx-panel .big{font-size:28px;font-weight:700;color:#E8792F;margin:4px 0 6px}' +
        '.bx-panel .big span{display:block;font-size:12px;font-weight:400;color:#6b7480}' +
        '.bx-panel .sec{font-size:11px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;color:#6b7480;margin:14px 0 4px}' +
        '.bx-panel .sevbar{display:flex;height:12px;border-radius:3px;overflow:hidden}' +
        '.bx-panel .sevleg{margin-top:5px;font-size:12px}' +
        '.bx-panel .sevleg span{margin-right:10px;white-space:nowrap}' +
        '.bx-panel .sevleg i{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:4px}' +
        '.bx-panel table{width:100%;border-collapse:collapse}' +
        '.bx-panel td{padding:4px 0;border-bottom:1px solid #f0f1f3;vertical-align:middle}' +
        '.bx-panel td.o{width:48%;padding-right:6px}' +
        '.bx-panel td.b div{height:9px;background:#E8792F;border-radius:2px}' +
        '.bx-panel td.n{text-align:right;width:12%;padding-left:6px}' +
        '.bx-panel td.p{text-align:right;width:12%;color:#6b7480}' +
        '.bx-panel a.reset{display:inline-block;margin-bottom:10px;color:#2E75B6;cursor:pointer;text-decoration:none}' +
        '.bx-panel .tip{text-transform:none;letter-spacing:0;font-weight:400}' +
        '.bx-panel tr.orow{cursor:pointer}' +
        '.bx-panel tr.orow:hover{background:#fdf1e8}' +
        '.bx-panel tr.orow.active{background:#fde3cf;font-weight:600}' +
        '.bx-panel .note{background:#fdf1e8;border-left:3px solid #E8792F;padding:6px 8px;margin:8px 0}';
      document.head.appendChild(css);

      var panel = document.createElement('div');
      panel.className = 'bx-panel';
      el.style.position = 'relative';
      el.appendChild(panel);

      var intro = '<h1>Which offenses happen on each Bronx block?</h1>' +
                  '<div class=\"hint\">Click a block on the map to see its offense types, severity and busiest hour. ' +
                  'Zoom in to read the street names.</div>' +
                  '<div class=\"ttl\">All Bronx street complaints</div>' + data.all;
      var n = el.data[0].lat.length;
      var top = data.top;          // most common offense of each block, same order as the points
      var shown = null;            // offense type currently mapped

      function allBlocks() {
        Plotly.restyle(el, {'marker.opacity': 0.85, 'marker.size': 7}, [0]);
      }

      // Click an offense row: show only the blocks where it is the most common offense
      function mapOffense(off, row) {
        var note = panel.querySelector('.note');
        if (note) note.remove();
        panel.querySelectorAll('tr.orow').forEach(function(r) { r.classList.remove('active'); });
        if (shown === off) { shown = null; allBlocks(); return; }   // click again = clear
        shown = off;
        var op = [], sz = [], k = 0;
        for (var i = 0; i < n; i++) {
          var hit = top[i] === off;
          op.push(hit ? 0.95 : 0.06); sz.push(hit ? 9 : 5); if (hit) k++;
        }
        Plotly.restyle(el, {'marker.opacity': [op], 'marker.size': [sz]}, [0]);
        row.classList.add('active');
        var box = document.createElement('div');
        box.className = 'note';
        box.innerHTML = '<b>' + off + '</b> is the most common offense on <b>' + k.toLocaleString() +
                        '</b> of ' + n.toLocaleString() + ' blocks (shown on the map). Click it again to show all blocks.';
        row.closest('table').parentNode.insertBefore(box, row.closest('table'));
      }

      function activateRows() {
        panel.querySelectorAll('tr.orow').forEach(function(r) {
          r.onclick = function() { mapOffense(r.dataset.off, r); };
        });
      }

      function reset() {
        shown = null;
        allBlocks();
        panel.innerHTML = intro;
        activateRows();
      }

      panel.innerHTML = intro;
      activateRows();

      el.on('plotly_click', function(d) {
        shown = null;
        var i = d.points[0].pointNumber;
        var op = new Array(n).fill(0.12); op[i] = 1;
        var sz = new Array(n).fill(6);    sz[i] = 18;
        Plotly.restyle(el, {'marker.opacity': [op], 'marker.size': [sz]}, [0]);
        panel.innerHTML = '<a class=\"reset\">&larr; Back to the whole Bronx</a>' + d.points[0].customdata;
        panel.querySelector('.reset').onclick = reset;
        activateRows();
      });
    }", data = list(all = bronx_panel, top = blocks$top_offense))

fig_offense_map

dir.create("Figures/Nov22_ProgressCheck/Interactive", showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(fig_offense_map, "Figures/Nov22_ProgressCheck/Interactive/Bronx_Street_Offense_Map.html",
                        selfcontained = TRUE, title = "Bronx street offenses by block")
unlink("Figures/Nov22_ProgressCheck/Interactive/Bronx_Street_Offense_Map_files", recursive = TRUE)  # helper folder saveWidget leaves behind
