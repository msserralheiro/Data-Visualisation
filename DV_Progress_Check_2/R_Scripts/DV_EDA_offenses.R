# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Offense Categories — which types are most common and how their distribution varies

library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(gridExtra)

if (!exists("nypd")) source("R_Scripts/DV_Data_Loading.R")

dv_blue   <- "#2E75B6"
dv_orange <- "#E8792F"

# Fix typo in raw data: "VLATION" to "VIOLATION" (affects Offense: OTHER STATE LAWS)
nypd <- nypd |>
  mutate(law_cat_cd = ifelse(law_cat_cd == "VLATION", "VIOLATION", law_cat_cd))

# 1. Severity Breakdown: Level of offense
offense_by_severity <- nypd |>
  filter(report_year >= 2024, ofns_desc != "(null)", !is.na(law_cat_cd)) |>
  count(law_cat_cd, ofns_desc, sort = TRUE) |>
  group_by(law_cat_cd) |>
  slice_head(n = 10) |>
  ungroup() |>
  mutate(
    law_cat_cd = factor(law_cat_cd, levels = c("FELONY", "MISDEMEANOR", "VIOLATION")),
    n = scales::comma(n)) |>
  arrange(law_cat_cd) |>
  rename(Offense = ofns_desc, Complaints = n)

offense_tbl_formatted <- offense_by_severity |>
  mutate(
    law_cat_cd = as.character(law_cat_cd),
    Severity   = ifelse(duplicated(law_cat_cd), "", law_cat_cd)) |>
  select(Severity, Offense, Complaints)

tbl_grob <- tableGrob(offense_tbl_formatted, rows = NULL,
                      theme = ttheme_minimal(base_size = 9))

ggsave("Figures/Offense_Table.png", tbl_grob, width = 10, height = 6, dpi = 300)

severity <- nypd |>
  filter(report_year >= 2024, !is.na(law_cat_cd)) |>
  count(law_cat_cd) |>
  mutate(
    pct = round(n / sum(n) * 100, 1),
    law_cat_cd = factor(law_cat_cd, levels = c("FELONY", "MISDEMEANOR", "VIOLATION"))) |>
  arrange(law_cat_cd)

plot_severity <- ggplot(severity, aes(x = law_cat_cd, y = n)) +
  geom_col(fill = dv_blue) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Misdemeanours dominate reported complaints in NYC",
    x = NULL, y = NULL,
    caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 1),
        plot.title = element_text(face = "bold"))

plot_severity
ggsave("Figures/Offense_Severity_Breakdown.png", plot_severity, width = 10, height = 6, dpi = 300)

# 2. Top offense types
top_offenses <- nypd |>
  filter(report_year >= 2024, ofns_desc != "(null)") |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 15) |>
  mutate(ofns_desc = fct_reorder(ofns_desc, n))

plot_top_offenses <- ggplot(top_offenses, aes(x = n, y = ofns_desc,
                                              fill = n == max(n))) +
  geom_col() +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c(`TRUE` = dv_orange, `FALSE` = "#9AA3AB"),
                    guide = "none") +
  labs(
    title = "Top 15 Offense types",
    subtitle = "Petit larceny accounts for the most reported offenses",
    x = NULL, y = NULL,
    caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text.x = element_blank(),
        plot.caption = element_text(hjust = 1),
        plot.title = element_text(face = "bold"))

plot_top_offenses
ggsave("Figures/Offense_Types.png", plot_top_offenses, width = 10, height = 6, dpi = 300)


# 3.Severity trend over time

severity_time <- nypd |>
  filter(!is.na(law_cat_cd), report_year < 2026) |>
  count(report_year, law_cat_cd) |>
  mutate(law_cat_cd = factor(law_cat_cd, levels = c("FELONY", "MISDEMEANOR", "VIOLATION")))

plot_severity_time <- ggplot(severity_time, aes(x = report_year, y = n)) +
  geom_line(colour = dv_blue, linewidth = 0.9) +
  geom_point(colour = dv_blue, size = 2) +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~ law_cat_cd, scales = "free_y", ncol = 3) +
  labs(
    title = "How complaint severity has changed since 2024",
    subtitle = "2026 excluded (partial year)",
    x = "Year", y = "Number of complaints",
    caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1),
        plot.title = element_text(face = "bold"))

plot_severity_time
ggsave("Figures/Offense_Time.png", plot_severity_time, width = 10, height = 6, dpi = 300)


# 4. Top offense types by hour of day

offense_hour <- nypd |>
  filter(report_year >= 2024, ofns_desc %in% top6_off, !is.na(hour)) |>
  count(ofns_desc, hour)

plot_offense_hour <- ggplot(offense_hour, aes(x = hour, y = n)) +
  geom_line(colour = dv_blue, linewidth = 0.8) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23)) +
  scale_y_continuous(labels = label_comma()) +
  facet_wrap(~ ofns_desc, scales = "free_y", ncol = 2) +
  labs(
    title = "When do different offense types happen during the day?",
    subtitle = "Peaks at 00:00 and 12:00 may include unknown times recorded as round hours",
    x = "Hour of occurrence", y = "Number of complaints",
    caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1),
        plot.title = element_text(face = "bold"))

plot_offense_hour
ggsave("Figures/Offense_By_Hour.png", plot_offense_hour, width = 10, height = 6, dpi = 300)
