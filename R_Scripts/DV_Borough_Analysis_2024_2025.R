# Data Visualization - Group Project
# NYPD Complaint Data (2024-2025)
# Exploratory Analysis: NYC Boroughs
#
# Research question:
# How do reported offense patterns vary across
# New York City's boroughs in 2024 and 2025?
#
# Data source: NYPD Complaint Data
# AI assistance: ChatGPT helped explain the
# filtering and exploratory analysis steps.

library(dplyr)
library(ggplot2)

# Load the original NYPD dataset
# Run DV_Data_Loading.R before this script.

# Select complaints occurring in 2024 and 2025
nypd_2024_2025 <- nypd |>
  filter(occurred_on >= as.Date("2024-01-01") &
           occurred_on < as.Date("2026-01-01"))

# --------------------------------------------------
# Chart 1 - BEFORE: Total complaints by borough
# --------------------------------------------------
# Initial exploratory visualization (2024-2025)
# Excludes records with missing borough information.

nypd_2024_2025 |>
  filter(!is.na(boro_nm)) |>
  count(boro_nm) |>
  ggplot(aes(n, reorder(boro_nm, n))) +
  geom_col()

# --------------------------------------------------
# Population Data and Calculations
# --------------------------------------------------
# Population source: 2020 US Census (NYC boroughs)
# Calculate 2024-2025 complaints per 1,000 residents.

pop_2020 <- data.frame(
  boro_nm = c("BRONX", "BROOKLYN", "MANHATTAN",
              "QUEENS", "STATEN ISLAND"),
  pop = c(1472654, 2736074, 1694251,
          2405464, 495747)
)

# Combine borough complaint counts with 2020 Census population
# Calculate two-year complaints per 1,000 residents

by_boro <- nypd_2024_2025 |>
  filter(!is.na(boro_nm)) |>
  count(boro_nm) |>
  left_join(pop_2020, by = "boro_nm") |>
  mutate(per_1000 = n / pop * 1000)

# Compare borough rankings

# Ranking by total complaints
by_boro |>
  arrange(desc(n))

# Ranking by complaints per 1,000 residents
by_boro |>
  arrange(desc(per_1000))

# --------------------------------------------------
# Chart 2 - BEFORE: Population-adjusted complaints
# --------------------------------------------------
# Initial exploratory visualization (2024-2025)
# Population denominator: 2020 US Census.

by_boro |>
  ggplot(aes(per_1000, reorder(boro_nm, per_1000))) +
  geom_col()

# --------------------------------------------------
# Chart 1 - AFTER: Improved total complaints by borough
# --------------------------------------------------

nypd_2024_2025 |>
  filter(!is.na(boro_nm)) |>
  count(boro_nm) |>
  ggplot(aes(n, reorder(boro_nm, n),
             fill = boro_nm == "BROOKLYN")) +
  geom_col() +
  scale_fill_manual(
    values = c("TRUE" = "#E8792F",
               "FALSE" = "#595959"),
    guide = "none"
  ) +
  geom_text(aes(label = scales::comma(n)),
            hjust = 1.1, colour = "white", size = 4) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Brooklyn recorded the most NYPD complaints in 2024–2025",
    x = "Number of reported complaints",
    y = NULL,
    caption = "Source: NYPD Complaint Data | Occurrence dates: 2024–2025"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

# --------------------------------------------------
# Chart 2 - AFTER: Population-adjusted complaints
# --------------------------------------------------
# 2024-2025 complaints per 1,000 residents
# Population denominator: 2020 US Census

by_boro |>
  ggplot(aes(per_1000, reorder(boro_nm, per_1000),
             fill = boro_nm == "BRONX")) +
  geom_col() +
  scale_fill_manual(
    values = c("TRUE" = "#E8792F",
               "FALSE" = "#595959"),
    guide = "none"
  ) +
  geom_text(
    aes(label = round(per_1000, 1)),
    hjust = 1.1,
    colour = "white",
    size = 4
  ) +
  labs(
    title = "The Bronx had the most complaints per 1,000 residents",
    x = "2024–2025 complaints per 1,000 residents",
    y = NULL,
    caption = "Sources: NYPD Complaint Data (2024–2025) | 2020 US Census population"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )

# Preliminary findings
#
# Chart 1: Brooklyn recorded the highest total number
# of NYPD complaints in 2024-2025 (319,779).
#
# Chart 2: After adjusting for borough population,
# the Bronx had the highest number of complaints
# (172.9 per 1,000 residents), followed by Manhattan
# (161.7 per 1,000 residents).
#
# Population-adjusted values represent cumulative
# complaints over two years, using 2020 Census
# population figures as the denominator.
#
# These descriptive results do not establish
# individual crime risk or explain why boroughs differ.



