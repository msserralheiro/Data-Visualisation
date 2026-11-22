# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Which offense types make up Bronx street complaints, and which are more common than in the rest of NYC?
#
# Bars = share of Bronx street complaints (top 12 offense types).
# Black mark = the same share in the rest of NYC (the comparison baseline).
# Orange = at least 20% more common on Bronx streets than in the rest of NYC.

source("R_Scripts/DV_Bronx_Data.R")

library(ggplot2)
library(tidyr)

dv_orange <- "#E8792F"
dv_grey   <- "#9AA3AB"
dv_dark   <- "#1B2A4A"

# complete name
full_names <- c("CRIMINAL MISCHIEF & RELATED OF" = "CRIMINAL MISCHIEF & RELATED OFFENSES")


## 1. Share of street complaints by offense type, Bronx vs rest of NYC
offense_share <- nyc_street |>
  count(area, ofns_desc) |>
  group_by(area) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  select(area, ofns_desc, share) |>
  pivot_wider(names_from = area, values_from = share, values_fill = 0) |>
  rename(bronx = Bronx, rest = `Rest of NYC`)

# Top 12 offense types in the Bronx
top_offenses <- bronx_street |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 12) |>
  left_join(offense_share, by = "ofns_desc") |>
  mutate(ratio = bronx / rest,
         higher = ratio >= 1.2,
         offense = if_else(ofns_desc %in% names(full_names), full_names[ofns_desc], ofns_desc),
         offense = tools::toTitleCase(tolower(offense)),
         offense = reorder(offense, bronx),
         label = if_else(higher,
                         paste0(sprintf("%.1f", bronx * 100), "%  (", sprintf("%.1f", ratio), "x)"),
                         paste0(sprintf("%.1f", bronx * 100), "%")))

top_offenses
# The top 12 types are 87% of Bronx street complaints.
# Robbery 5.1% vs 3.6% (1.4x), Grand Larceny of Motor Vehicle 8.0% vs 6.1% (1.3x),
# Felony Assault 5.2% vs 4.2% (1.2x); Harassment 2 6.4% vs 8.5% (0.7x)
sum(top_offenses$n) / nrow(bronx_street)


## 2. Figure 
p_offense_types <- ggplot(top_offenses, aes(y = offense)) +
  geom_col(aes(x = bronx, fill = higher), width = 0.7) +
  geom_errorbar(aes(xmin = rest, xmax = rest), colour = dv_dark,
                width = 0.85, linewidth = 0.9) +
  geom_text(aes(x = pmax(bronx, rest), label = label, colour = higher),
            hjust = -0.15, size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = dv_orange, `FALSE` = dv_grey), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = dv_orange, `FALSE` = "grey40"), guide = "none") +
  scale_x_continuous(labels = scales::label_percent(),
                     limits = c(0, max(top_offenses$bronx, top_offenses$rest) * 1.25),
                     expand = c(0, 0)) +
  labs(title = "Robbery, car theft and felony assault are more common on Bronx streets",
       subtitle = paste("Share of street complaints by offense type (top 12 in the Bronx).",
                        "Bar = Bronx; black mark = rest of NYC; orange = at least 1.2 times the rest of NYC",
                        sep = "\n"),
       x = "Share of street complaints", y = NULL,
       caption = "Data: NYPD Complaint Data, NYC Open Data (street complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

p_offense_types

ggsave("Figures/Nov22_ProgressCheck/Street_Offense_Types_Bronx.png",
       plot = p_offense_types, width = 10, height = 6, dpi = 300, bg = "white")
