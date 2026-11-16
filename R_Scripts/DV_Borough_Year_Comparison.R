# Data Visualization - Group Project
# Weekly Progress Check - Borough Analysis

# Question:
# How did reported complaints across NYC boroughs
# change between 2024 and 2025?
# Only two full years are compared here (2024 and 2025),
# even though the group decided to analyze the broader
# period from 2024 to June 2026.
# This allows us to compare two complete years.

# Compare two filter option approaches:
# 1. Year when the incident occurred
# 2. Year when the complaint was reported

# AI assistance: ChatGPT helped develop and explain
# portions of this R code.

library(dplyr)
library(ggplot2)

# Run DV_Data_Loading.R first

### OPTION 1 - OCCURRENCE DATE

# Select incidents occurring in 2024 and 2025
borough_occurrence <- nypd |>
  filter(occurred_on >= as.Date("2024-01-01") &
           occurred_on < as.Date("2026-01-01"),
         !is.na(boro_nm)) |>
  mutate(year = as.integer(format(occurred_on, "%Y"))) |>
  count(boro_nm, year)

# Check complaint counts
borough_occurrence

# Compare the two years
plot_occurrence <- borough_occurrence |>
  ggplot(aes(x = n, y = boro_nm)) +
  geom_col(fill = "#595959") +
  geom_text(aes(label = scales::comma(n)),
            hjust = -0.1, size = 3.5) +
  facet_wrap(~ year, ncol = 2) +
  scale_x_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "NYPD complaints by borough: 2024 vs 2025",
    subtitle = "Based on incident occurrence date",
    x = "Number of complaints",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

plot_occurrence

ggsave(
  "Figures/Borough_Year_Comparison_Occurrence.png",
  plot_occurrence, width = 10, height = 5, dpi = 300
)


### OPTION 2 - REPORT DATE

# Select complaints reported in 2024 and 2025
borough_report <- nypd |>
  filter(report_year %in% c(2024, 2025),
         !is.na(boro_nm)) |>
  count(boro_nm, report_year)

# Check complaint counts
borough_report

# Compare the two years
plot_report <- borough_report |>
  ggplot(aes(x = n, y = boro_nm)) +
  geom_col(fill = "#595959") +
  geom_text(aes(label = scales::comma(n)),
            hjust = -0.1, size = 3.5) +
  facet_wrap(~ report_year, ncol = 2) +
  scale_x_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "NYPD complaints by borough: 2024 vs 2025",
    subtitle = "Based on complaint report date",
    x = "Number of complaints",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

plot_report

ggsave(
  "Figures/Borough_Year_Comparison_Report.png",
  plot_report, width = 10, height = 5, dpi = 300
)

