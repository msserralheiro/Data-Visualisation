# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Interactive density map of Bronx street complaints (zoom + hover)


source("R_Scripts/DV_Bronx_Data.R")

library(plotly)

dv_orange <- "#E8792F"


## 1. Complaints per block

# Precinct of each block = the most common precinct of its complaints
block_precinct <- bronx_street |>
  count(block, addr_pct_cd) |>
  group_by(block) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(block, precinct = addr_pct_cd)

# Complaints per block: whole period, 2024 and 2025
blocks <- bronx_street |>
  group_by(block, block_lat, block_lon) |>
  summarise(n = n(),
            n2024 = sum(report_year == 2024),
            n2025 = sum(report_year == 2025),
            .groups = "drop") |>
  left_join(block_precinct, by = "block")

# Check: every Bronx street complaint is in one block
sum(blocks$n) == nrow(bronx_street)  # TRUE
nrow(blocks)                         # 6 638 blocks


## 2. Names for the 10 worst blocks
# From Figures/EDA/Street_Top10_Intersections_Bronx.png 
block_names <- data.frame(
  block = c("40.816 -73.918", "40.81 -73.925", "40.862 -73.897",
            "40.838 -73.92", "40.84 -73.918", "40.831 -73.827",
            "40.849 -73.906", "40.869 -73.88", "40.862 -73.891",
            "40.887 -73.847"),
  name  = c("Willis Ave & 3rd Ave", "E 138th St & Alexander Ave", "Grand Concourse",
            "Jerome Ave & E 169th St", "Jerome Ave", "Barkley Ave",
            "Grand Concourse & Monroe Ave", "Mosholu Pkwy", "Webster Ave",
            "E 229th St"))

blocks <- blocks |>
  left_join(block_names, by = "block") |>
  mutate(rank = min_rank(desc(n)),
         hover = paste0(if_else(is.na(name), "", paste0("<b>", name, "</b><br>")),
                        "<b>", n, " street complaints</b> (rank ", rank, " of ", nrow(blocks), ")",
                        "<br>2024: ", n2024, "  |  2025: ", n2025,
                        "<br>Precinct ", precinct,
                        "<br>", block_lat, ", ", block_lon)) |>
  arrange(n)   # busiest blocks drawn last, on top


## 3. Map
color_ticks <- c(1, 5, 25, 100, 500)

fig_density <- plot_ly(blocks,
                       type = "scattermapbox", mode = "markers",
                       lat = ~block_lat, lon = ~block_lon,
                       text = ~hover, hoverinfo = "text",
                       marker = list(size = 7, opacity = 0.85,
                                     color = ~log10(n),
                                     colorscale = list(c(0, "#ffeda0"), c(0.35, "#feb24c"),
                                                       c(0.65, "#e31a1c"), c(1, "#4a0013")),
                                     cmin = 0, cmax = log10(max(blocks$n)),
                                     colorbar = list(title = "Complaints<br>per block",
                                                     tickvals = log10(color_ticks),
                                                     ticktext = color_ticks))) |>
  layout(title = list(text = paste0("<b>Bronx street complaints by block (about 100 m)</b>",
                                    "<br><sup>January 2024 - June 2026. Zoom in to read the streets; hover over a block for its counts.</sup>"),
                      x = 0),
         mapbox = list(style = "white-bg",
                       layers = list(list(below = "traces", sourcetype = "raster",
                                          source = list("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")),
                                     list(sourcetype = "raster",   # street names above the blocks
                                          source = list("https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"))),
                       center = list(lat = 40.845, lon = -73.88),
                       zoom = 11.3),
         margin = list(t = 70, b = 40, l = 0, r = 0),
         annotations = list(list(text = "Data: NYPD Complaint Data, NYC Open Data. Basemap: Esri, HERE, Garmin, OpenStreetMap contributors.",
                                 x = 1, y = -0.04, xref = "paper", yref = "paper",
                                 xanchor = "right", showarrow = FALSE,
                                 font = list(size = 10, color = "grey"))))

fig_density

dir.create("Figures/Nov22_ProgressCheck/Interactive", showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(fig_density, "Figures/Nov22_ProgressCheck/Interactive/Bronx_Street_Density_Map.html",
                        selfcontained = TRUE, title = "Bronx street complaints by block")
unlink("Figures/Nov22_ProgressCheck/Interactive/Bronx_Street_Density_Map_files", recursive = TRUE)  # helper folder saveWidget leaves behind
