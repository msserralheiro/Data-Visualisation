# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Exploratory Data Analysis

library(ggplot2)

## EDA ABOUT "WHAT?" COLUMNS
# Reference table with one row per offense code and its description
ky_table <- nypd |> count(ky_cd, ofns_desc) |> arrange(ky_cd)

# Checks if each code has only one description 
# (shows codes with more than one description!)
ky_table |> count(ky_cd) |> filter(n > 1)
"NOTE: Join counts by equal codes with different description?"

# Add the severity level to see how codes relate to level of offense
ky_law <- nypd |> count(ky_cd, ofns_desc, law_cat_cd) |> arrange(ky_cd)

# Codes that appear with more than one severity level
ky_law |> count(ky_cd) |> filter(n > 1)

# Count complaints by completion status, with percentages
nypd |> count(crm_atpt_cptd_cd) |> mutate(pct = round(n / sum(n) * 100, 2))
"NOTE: the percentage of ATTEMPTED CRIMES is not significantly enough."

# Remove the 161 rows with "(null)" and create table with combinations year+status 
status_year <- nypd |> filter(crm_atpt_cptd_cd != "(null)") |>
  count(report_year, crm_atpt_cptd_cd)
ggplot(status_year, aes(x = report_year, y = n, fill = crm_atpt_cptd_cd)) +
          geom_col(position = "stack")


## EDA ABOUT "WHEN?" COLUMNS
# Count old occurrences by year to see what are the plausible dates 
nypd |> filter(old_occurrence) |>
  count(year = as.integer(format(occurred_on, "%Y"))) |> arrange(year)
"NOTE: Cut by which year? Substitute by NA?"

# Check the opposite problem: offences "occurring" after being reported
nypd |> filter(occurred_on > reported_on) |> nrow() # 17 occurrences

occur_after_report <- nypd |> filter(occurred_on > reported_on) |>
  select(occurred_on, reported_on, hour, boro_nm, ofns_desc) |>
  mutate(days_after = as.integer(occurred_on - reported_on)) |>
  arrange(desc(days_after)) # table with those 17 rows 
"NOTE: Cut every row? Substitute occured/reported_on columns with NA?"


# EDA ABOUT "WHAT?" COLUMNS



