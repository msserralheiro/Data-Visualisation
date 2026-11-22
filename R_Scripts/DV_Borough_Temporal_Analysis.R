# Data Visualization - Borough Temporal Analysis
# NYPD Complaint Data: January 2024 - June 2026
#
# Research focus -
# Explore how complaint patterns vary across NYC boroughs
# over time, including monthly, hourly, and weekday patterns.
#
# Date filter definition -
# Use occurrence dates rather than report dates.
#
# Comparison periods -
# 1. Full years: 2024 vs 2025
# 2. January-June: 2024, 2025, and 2026
# 3. Monthly trends: January 2024 - June 2026
#
# Data loading:
# Run DV_Data_Loading.R before running this script.

### LIBRARIES

library(dplyr)
library(ggplot2)
library(plotly)

# Step 1 - filter data by occurrence date
nypd_temporal <- nypd |>
  filter(
    occurred_on >= as.Date("2024-01-01"),
    occurred_on <= as.Date("2026-06-30")
  )
nrow(nypd_temporal)

# Step 2 - check borough distribution
nypd_temporal |>
  count(boro_nm, sort = TRUE)

# Step 3 - create a separate dataset for borough analysis
# Exclude 2,972 records (0.21%) with missing borough information.
# The new dataset called nypd_borough contains 1,417,263 records.
# The original nypd_temporal dataset remains unchanged.

nypd_borough <- nypd_temporal |>
  filter(!is.na(boro_nm))

nrow(nypd_borough)

### RESEARCH QUESTION 1 - Monthly Complaint Trends by Borough
# How have recorded complaints changed over time across
# NYC's five boroughs from January 2024 to June 2026?

# Analysis - monthly complaint counts by borough
# Visualization - Interactive line chart (ggplot2 + Plotly)

# Step 4 - aggregate complaints by month and borough

monthly_borough <- nypd_borough |>
  mutate(month = as.Date(format(occurred_on, "%Y-%m-01"))) |>
  count(month, boro_nm, name = "complaints")

head(monthly_borough, 10)

# Hypothesis - monthly complaint counts show recurring seasonal
# patterns across the five boroughs, although complaint volumes may differ.

# Step 5 - visualize monthly complaint trends by borough

p_monthly <- ggplot(
  monthly_borough,
  aes(
    x = month,
    y = complaints,
    colour = boro_nm,
    group = boro_nm,
    text = paste0(
      "Borough: ", boro_nm,
      "<br>Month: ", format(month, "%B %Y"),
      "<br>Complaints: ", format(complaints, big.mark = ",")
    )
  )
) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Monthly NYPD Complaints by Borough",
    subtitle = "Occurrence dates: January 2024 - June 2026",
    x = "Month",
    y = "Number of complaints",
    colour = "Borough"
  ) +
  theme_minimal()

p_monthly

# Step 6 - save monthly complaint chart

ggsave(
  filename = "Figures/Nov20_ProgressCheck/Monthly_Borough_Complaints.png",
  plot = p_monthly,
  width = 12,
  height = 6,
  dpi = 300
)

# Step 7 - add borough population data
# Population source - 2020 US Census
# Use the same population figures as in the previous borough 
# analysis - DV_Borough_Analysis_2024_2025.R.
# Population is held constant across the analysis period.

pop_2020 <- data.frame(
  boro_nm = c("BRONX", "BROOKLYN", "MANHATTAN",
              "QUEENS", "STATEN ISLAND"),
  pop = c(1472654, 2736074, 1694251,
          2405464, 495747)
)

# Step 8 - join monthly complaints with borough population

monthly_borough_pop <- monthly_borough |>
  left_join(pop_2020, by = "boro_nm")

# Step 9 - calculate monthly complaints per 1,000 residents

monthly_borough_pop <- monthly_borough_pop |>
  mutate(per_1000 = complaints / pop * 1000)

# Step 10 - visualize monthly complaints per 1,000 residents

p_monthly_pop <- ggplot(
  monthly_borough_pop,
  aes(
    x = month,
    y = per_1000,
    colour = boro_nm,
    group = boro_nm,
    text = paste0(
      "Borough: ", boro_nm,
      "<br>Month: ", format(month, "%B %Y"),
      "<br>Complaints per 1,000: ", round(per_1000, 2)
    )
  )
) +
  geom_line(linewidth = 1) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "Monthly NYPD Complaints per 1,000 Residents by Borough",
    subtitle = "Occurrence dates: January 2024 - June 2026",
    x = "Month",
    y = "Complaints per 1,000 residents",
    colour = "Borough",
    caption = "Population: 2020 US Census (fixed denominator)"
  ) +
  theme_minimal()

p_monthly_pop

# Step 11 - save monthly population-adjusted chart

ggsave(
  filename = "Figures/Nov20_ProgressCheck/Monthly_Borough_Complaints_Per_1000.png",
  plot = p_monthly_pop,
  width = 12,
  height = 6,
  dpi = 300
)

### Preliminary Findings - Research Question 1

# Brooklyn has the highest monthly complaint counts
# in absolute numbers.

# When adjusted for population, the Bronx has the highest monthly 
# complaints per 1,000 residents, followed by Manhattan.  

# Monthly counts fluctuate across the chosen period, with some
# similar peaks and declines across boroughs.

# The patterns suggest possible seasonality, but the
# Jan-Jun comparison shows differences across years.
# The results don't confirm a consistent seasonal pattern.

# Population-adjusted comparisons use fixed 2020 Census
# numbers and don't represent individual crime risk.


# Step 12 - convert monthly complaint chart to interactive Plotly

p_monthly_interactive <- ggplotly(p_monthly, tooltip = "text")

p_monthly_interactive

# Step 13 - convert population-adjusted chart to interactive Plotly

p_monthly_pop_interactive <- ggplotly(p_monthly_pop, tooltip = "text")

p_monthly_pop_interactive

# Step 14 - save interactive monthly complaint chart

htmlwidgets::saveWidget(
  p_monthly_interactive,
  file = "Figures/Nov20_ProgressCheck/Monthly_Borough_Complaints.html",
  selfcontained = TRUE
)

# Step 15 - save interactive population-adjusted chart

htmlwidgets::saveWidget(
  p_monthly_pop_interactive,
  file = "Figures/Nov20_ProgressCheck/Monthly_Borough_Complaints_Per_1000.html",
  selfcontained = TRUE
)

# Step 16 - prepare January-June comparison for 2024, 2025 and 2026
# Use monthly complaint counts already aggregated by borough.
# Compare the same six months across all three years.

monthly_comparison <- monthly_borough |>
  mutate(
    year = as.integer(format(month, "%Y")),
    month_num = as.integer(format(month, "%m"))
  ) |>
  filter(month_num <= 6)

# Step 17 - visualize Jan-Jun complaint trends across three years

p_comparison <- ggplot(
  monthly_comparison,
  aes(
    x = month_num,
    y = complaints,
    colour = factor(year),
    group = year
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ boro_nm, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:6,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
  ) +
  labs(
    title = "NYPD Complaints: Jan-Jun Comparison",
    subtitle = "2024, 2025 and 2026 | Based on occurrence dates",
    x = "Month",
    y = "Number of complaints",
    colour = "Year"
  ) +
  theme_minimal()

p_comparison

# Step 18 - calculate the number of days in each month
# This allows us to compare average daily complaints across months.

monthly_comparison <- monthly_comparison |>
  mutate(
    days_in_month = as.integer(
      as.Date(format(month + 32, "%Y-%m-01")) - month
    )
  )

# Step 19 - calculate average daily complaints by borough
# Adjust monthly counts for differences in the number of days.

monthly_comparison <- monthly_comparison |>
  mutate(avg_daily = complaints / days_in_month)

# Step 20 - visualize average daily complaints across three years

p_daily_comparison <- ggplot(
  monthly_comparison,
  aes(
    x = month_num,
    y = avg_daily,
    colour = factor(year),
    group = year
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ boro_nm, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:6,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
  ) +
  labs(
    title = "Average Daily NYPD Complaints: Jan-Jun Comparison",
    subtitle = "2024, 2025 and 2026 | Based on occurrence dates",
    x = "Month",
    y = "Average complaints per day",
    colour = "Year"
  ) +
  theme_minimal()

p_daily_comparison

# Step 21 - save average daily complaint comparison chart

ggsave(
  filename = "Figures/Nov20_ProgressCheck/Average_Daily_Complaints_Jan_Jun.png",
  plot = p_daily_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

### Additional Findings - Research Question 1

# Monthly complaint totals appear lower in February
# across the five boroughs.

# However, the February decline becomes less consistent
# after adjusting for the number of days in each month
# and calculating average daily complaints.

# Average daily complaints generally increase during spring,
# but the patterns vary across boroughs and years.

# The results provide some support for the seasonality hypothesis,
# but the patterns are not consistent enough to confirm it.

# The Jan-Jun comparison covers three years and doesn't
# establish seasonal patterns across the full calendar year.


### RESEARCH QUESTION 2 - Time-of-Day Complaint Patterns by Borough
# How do complaint patterns vary by time of day across
# NYC's five boroughs?

# Hypothesis - complaints are more frequent during afternoon
# and evening hours, but patterns may differ across boroughs.

# Analysis - complaint counts by hour and borough
# Visualization - hourly complaint trends (ggplot2 + Plotly)

# Step 22 - count complaints by hour and borough
# Use occurrence hour to identify when complaints are concentrated.

hourly_borough <- nypd_borough |>
  count(boro_nm, hour, name = "complaints")

# Step 23 - visualize hourly complaint patterns by borough

p_hourly <- ggplot(
  hourly_borough,
  aes(
    x = hour,
    y = complaints,
    colour = boro_nm,
    group = boro_nm,
    text = paste0(
      "Borough: ", boro_nm,
      "<br>Hour: ", sprintf("%02d:00", hour),
      "<br>Complaints: ", format(complaints, big.mark = ",")
    )
  )
) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    title = "NYPD Complaints by Hour and Borough",
    subtitle = "Occurrence dates: January 2024 - June 2026",
    x = "Hour of day (24-hour clock)",
    y = "Number of complaints",
    colour = "Borough"
  ) +
  theme_minimal()

p_hourly

### Preliminary Findings - Research Question 2

# Complaint counts are generally lowest in the early morning
# and increase during afternoon and evening hours.

# The patterns are similar across the five boroughs,
# but the hours with the highest number of complaints may differ.

# There is also a noticeable increase in complaints at midnight.
# Of the 60,335 complaints recorded between 00:00 and 00:59,
# 15,333 (25.4%) were recorded at exactly 00:00.

# Some occurrence times may be approximate, but this cannot
# be confirmed from the data. For this analysis, all records
# were retained.

# Step 24 - save hourly complaint chart

ggsave(
  filename = "Figures/Nov20_ProgressCheck/Hourly_Borough_Complaints.png",
  plot = p_hourly,
  width = 12,
  height = 6,
  dpi = 300
)

# Step 25 - convert hourly complaint chart to interactive Plotly

p_hourly_interactive <- ggplotly(p_hourly, tooltip = "text")

p_hourly_interactive

# Step 26 - save interactive hourly complaint chart

htmlwidgets::saveWidget(
  p_hourly_interactive,
  file = "Figures/Nov20_ProgressCheck/Hourly_Borough_Complaints.html",
  selfcontained = TRUE
)
