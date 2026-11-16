# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Street-level spatial analysis — Bronx deep-dive

if (!exists("nypd")) source("R_Scripts/DV_Data_Loading.R")

# Install once if needed:
# install.packages(c("tigris", "sf", "stringi", "hexbin"))

library(dplyr)
library(ggplot2)
library(scales)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)

# Parameters
MIN_CLUSTER_COUNT <- 5     # minimum count per block to qualify as a hotspot
BRONX_TOP_N       <- 10    # for the intersections bar chart
INTERSECT_RADIUS  <- 30    # metres — second-nearest road search radius
BRONX_FIPS        <- "005"

# Colormap
RAMP_LOW  <- "#feb24c"
RAMP_HIGH <- "#800026"
DV_ORANGE <- "#E8792F"
GREY_MUTE <- "#9AA3AB"

caption_map <- paste(
  "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)",
  "Map boundaries: U.S. Census Bureau TIGER/Line via tigris",
  sep = "\n")

caption_bar <- paste(
  "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)",
  "Street names: U.S. Census Bureau TIGER/Line via tigris",
  sep = "\n")

theme_map <- theme_minimal(base_size = 12) +
  theme(panel.grid   = element_blank(),
        axis.text    = element_blank(),
        axis.ticks   = element_blank(),
        plot.caption = element_text(hjust = 0),  # left-aligned to avoid right-side overflow clipping the long first line
        plot.title   = element_text(face = "bold"))

# 1. Bronx Geography — tract polygons and road polylines

bronx_tracts <- tracts(state = "NY", county = BRONX_FIPS,
                       cb = TRUE, class = "sf") |>
  st_transform(crs = 4326)

bronx_land <- st_union(bronx_tracts)  # single multipolygon used to clip points

bronx_roads <- roads(state = "NY", county = BRONX_FIPS) |>
  st_transform(crs = 4326) |>
  filter(!is.na(FULLNAME))


# 2. Bronx Street Complaints — clipped to Bronx land
bronx_street <- nypd |>
  filter(prem_typ_desc == "STREET",
         report_year >= 2024,
         boro_nm == "BRONX",
         !is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

bronx_street <- bronx_street[
  lengths(st_intersects(bronx_street, bronx_land)) > 0,]


# 3. Hotspot Clusters round to block level (~100 m)

bronx_hotspots <- bronx_street |>
  st_drop_geometry() |>
  mutate(lat_r = round(latitude, 3), lon_r = round(longitude, 3)) |>
  count(lat_r, lon_r, sort = TRUE) |>
  filter(n >= MIN_CLUSTER_COUNT) |>
  st_as_sf(coords = c("lon_r", "lat_r"), crs = 4326, remove = FALSE)

# Rounding can push a point just off the shoreline — reclip to be safe.
bronx_hotspots <- bronx_hotspots[
  lengths(st_intersects(bronx_hotspots, bronx_land)) > 0,]


# 4. Street Names — nearest two roads per top hotspot

bronx_roads_m <- st_transform(bronx_roads, 32618)  # UTM 18N (metres)

label_intersection <- function(pt_m, roads_m, maxdist_m = INTERSECT_RADIUS) {
  buf <- st_buffer(pt_m, maxdist_m)
  cand_idx <- unlist(st_intersects(buf, roads_m))
  if (length(cand_idx) == 0) {
    idx <- st_nearest_feature(pt_m, roads_m)
    return(as.character(roads_m$FULLNAME[idx]))
  }
  cand <- roads_m[cand_idx, ]
  d <- as.numeric(st_distance(pt_m, cand))
  nm <- unique(as.character(cand$FULLNAME[order(d)]))
  nm <- nm[nzchar(nm)]
  if (length(nm) >= 2) paste(nm[1], "&", nm[2]) else nm[1]}

bronx_top   <- bronx_hotspots |>
  slice_max(n, n = BRONX_TOP_N, with_ties = FALSE)
bronx_top_m <- st_transform(bronx_top, 32618)

bronx_top$label <- vapply(seq_len(nrow(bronx_top_m)), function(i) {
  label_intersection(bronx_top_m[i, ], bronx_roads_m)
}, character(1))


# 5. Bronx Hex-Density Map 

bronx_bbox <- st_bbox(bronx_land)

p_bronx_hex <- ggplot() +
  geom_sf(data = bronx_tracts, fill = "#f2f2f2", colour = "#ffffff", linewidth = 0.2) +
  stat_bin_hex(data = bronx_street,
               aes(x = longitude, y = latitude, fill = after_stat(count)),
               bins = 60, alpha = 0.75) +
  scale_fill_gradient(low = RAMP_LOW, high = RAMP_HIGH,
                      name = "Complaints", labels = comma) +
  coord_sf(xlim = c(bronx_bbox["xmin"], bronx_bbox["xmax"]),
           ylim = c(bronx_bbox["ymin"], bronx_bbox["ymax"]),
           expand = FALSE) +
  labs(
    title    = "Street offense density in the Bronx (2024–2026)",
    subtitle = "Hexagonal bins; darker = more complaints",
    x = NULL, y = NULL, caption = caption_map) +
  theme_map

print(p_bronx_hex)
ggsave("Figures/Street_Hex_Density_Bronx.png",
       p_bronx_hex, width = 10, height = 11, dpi = 300)


# 6. Top 10 Intersections — bar chart with street names

bronx_top_df <- bronx_top |>
  st_drop_geometry() |>
  arrange(desc(n))

p_bronx_bar <- bronx_top_df |>
  mutate(label  = factor(label, levels = rev(label)),
         is_top = n == max(n)) |>
  ggplot(aes(x = label, y = n, fill = is_top)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.5) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(`TRUE` = DV_ORANGE, `FALSE` = GREY_MUTE),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
  labs(
    title    = "Top 10 street-complaint intersections in the Bronx (2024–2026)",
    subtitle = "Counts at ~100 m block clusters; top intersection highlighted",
    x = NULL, y = "Complaints", caption = caption_bar) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title   = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1))

print(p_bronx_bar)
ggsave("Figures/Street_Top10_Intersections_Bronx.png",
       p_bronx_bar, width = 10, height = 6, dpi = 300)
