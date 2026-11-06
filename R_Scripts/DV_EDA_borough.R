# Data Visualization - Group Project
# Data: NYPD Complaint Data (2020-2026)
# Analysis period: 2024-2025 
# The group has decided to analyze two full years of data
#
# Five Borough Analysis
#
# Question:
# How do reported offenses differ across NYC boroughs
# in 2024 and 2026?
#
# AI assistance: ChatGPT used to help develop and
# explain parts of the R code, charts, and comments.
# The analysis uses methods discussed in class.


library(dplyr)
library(ggplot2)

# Load the original NYPD dataset
# Run DV_Data_Loading.R before this script.

# Select complaints occurring in 2024 and 2026
nypd_2024_2026 <- nypd |>
  filter(occurred_on >= as.Date("2024-01-01") &
           occurred_on < as.Date("2026-12-01"))

### Chart 1 - BEFORE: Total complaints by borough

# First version of the chart using 2024-2026 data
# Remove records where the borough is missing

Borough_Complaints_2024_2026_Exploratory <- 
  nypd_2024_2026 |>
  filter(!is.na(boro_nm)) |>
  count(boro_nm) |>
  ggplot(aes(n, reorder(boro_nm, n))) +
  geom_col()
Borough_Complaints_2024_2026_Exploratory

ggsave("Figures/Borough_Complaints_2024_2026_Exploratory.png", 
              plot = Borough_Complaints_2024_2026_Exploratory, width = 10, height = 6, dpi = 300)

### Population Data and Calculations

# Population source: 2020 US Census - NYC boroughs
# Calculate 2024-2026 complaints per 1,000 residents.

pop_2020 <- data.frame(
  boro_nm = c("BRONX", "BROOKLYN", "MANHATTAN",
              "QUEENS", "STATEN ISLAND"),
  pop = c(1472654, 2736074, 1694251,
          2405464, 495747))

# Combine borough complaint counts with 2020 Census population
# Calculate two-year complaints per 1,000 residents

by_boro <- nypd_2024_2025 |>
  filter(!is.na(boro_nm)) |>
  count(boro_nm) |>
  left_join(pop_2020, by = "boro_nm") |>
  mutate(per_1000 = n / pop * 1000)

## Compare borough rankings

# Ranking by total complaints
by_boro |>
  arrange(desc(n))

# Ranking by complaints per 1,000 residents
by_boro |>
  arrange(desc(per_1000))

### Chart 2 - BEFORE: Population-adjusted complaints

# First version of the chart using 2024-2026 data
# Population figures from the 2020 US Census

Borough_Complaints_Per_1000_Exploratory <-
  by_boro |>
  ggplot(aes(per_1000, reorder(boro_nm, per_1000))) +
  geom_col()
Borough_Complaints_Per_1000_Exploratory

ggsave("Figures/Borough_Complaints_Per_1000_Exploratory.png", 
       plot = Borough_Complaints_Per_1000_Exploratory, width = 10, height = 6, dpi = 300)

### Chart 1 - AFTER: Improved total complaints by borough

Borough_Complaints_2024_2026_Improved <-
  nypd_2024_2026 |>
  filter(!is.na(boro_nm)) |>
  count(boro_nm) |>
  ggplot(aes(n, reorder(boro_nm, n),
             fill = boro_nm == "BROOKLYN")) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#E8792F","FALSE" = "#595959"), guide = "none") +
  geom_text(aes(label = scales::comma(n)), hjust = 1.1, colour = "white", size = 4) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Brooklyn recorded the most NYPD complaints between 2024 and 2026",
       x = "Number of reported complaints",
       y = NULL,
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(face = "bold"))
Borough_Complaints_2024_2026_Improved

ggsave("Figures/Borough_Complaints_2024_2026_Improved.png", 
       plot = Borough_Complaints_2024_2026_Improved, width = 10, height = 6, dpi = 300)


### Chart 2 - AFTER: Population-adjusted complaints

# 2024-2026 complaints per 1,000 residents
# Population denominator: 2020 US Census

Borough_Complaints_Per_1000_Improved <-
  by_boro |>
  ggplot(aes(per_1000, reorder(boro_nm, per_1000),
             fill = boro_nm == "BRONX")) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#E8792F","FALSE" = "#595959"),
    guide = "none") +
  geom_text(aes(label = round(per_1000, 1)), hjust = 1.1, 
            colour = "white", size = 4) +
  labs(title = "The Bronx reported the most NYPD complaints per 1,000 residents",
    x = "Complaints per 1,000 residents",
    y = NULL,
    caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(face = "bold"))
Borough_Complaints_Per_1000_Improved

ggsave("Figures/Borough_Complaints_Per_1000_Improved.png", 
       plot = Borough_Complaints_Per_1000_Improved, width = 10, height = 6, dpi = 300)

### Preliminary Findings

# Chart 1: Brooklyn had the highest total number of
# complaints in 2024-2025 - 319,779.

# Chart 2: After adjusting for population, the Bronx
# had the highest number of complaints per 1,000
# residents - 172.9, followed by Manhattan - 161.7.

# Population figures are from the 2020 US Census.
# The results cover two years instead of one year.

# Complaint counts don't necessarily indicate
# individual crime risk.