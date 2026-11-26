# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Initial exploration and cleaning checks (data quality, gaps and oddities)

library(dplyr)
library(tidyr)

# Load the data only if it is not already in memory
if (!exists("nypd")) {source("R_Scripts/DV_Data_Loading.R")}


# Borough (the dictionary reports 5 835 NA)
count(nypd, boro_nm, sort = TRUE)

# Coordinates (the dictionary reports 28 NA)
sum(is.na(nypd$latitude))
sum(is.na(nypd$longitude))

# Completed vs attempted, with percentages
nypd |>
  count(crm_atpt_cptd_cd) |>
  mutate(pct = round(n / sum(n) * 100, 2))


# OFFENSE CODE: is there one description per code (ky_cd -> ofns_desc)?
# Step 1: reference table with one row per offence code and its description
ky_table <- nypd |>
  count(ky_cd, ofns_desc) |>
  arrange(ky_cd)

# Step 2: codes with more than one description (an empty result means all good)
ky_table |>
  count(ky_cd) |>
  filter(n > 1)

# Step 3: add the severity level (law_cat_cd)
ky_law <- nypd |>
  count(ky_cd, ofns_desc, law_cat_cd) |>
  arrange(ky_cd)

# Codes that appear with more than one severity level
ky_law |>
  count(ky_cd) |>
  filter(n > 1)

# Inspect the codes with more than one description
# (12 codes in our data: 116, 120, 124, 125, 126, 343, 345, 361, 363, 364, 675, 677)
problem_codes <- ky_table |>
  count(ky_cd) |>
  filter(n > 1) |>
  pull(ky_cd)

ky_table |>
  filter(ky_cd %in% problem_codes) |>
  arrange(ky_cd)
# Look for: same text written differently, NA/"(null)", or really different offences.
# DECISION: one rule per type of problem (not one per code), written in the cleaning script.


# Occurrence dates: impossible and inconsistent values
# old_occurrence = occurred_on before 2020-01-01 (16 630 rows). 
# It mixes genuine late reports with impossible years (e.g. 1015, 1900).

# Years of the old occurrences, to see where plausible dates end
nypd |>
  filter(old_occurrence) |>
  count(year = as.integer(format(occurred_on, "%Y"))) |>
  arrange(year)

# Offences recorded as occurring after being reported
after_report <- nypd |>
  filter(occurred_on > reported_on) |>
  select(cmplnt_num, occurred_on, reported_on, hour, boro_nm, ofns_desc) |>
  mutate(days_after = as.integer(occurred_on - reported_on)) |>
  arrange(desc(days_after))

nrow(after_report)
head(after_report, 20)

# Are the suspicious rows worse in other columns? (cutoff is a placeholder)
nypd |>
  mutate(suspicious = occurred_on < as.Date("2015-01-01")) |>
  group_by(suspicious) |>
  summarise(
    n = n(),
    pct_borough_na = mean(is.na(boro_nm)) * 100,
    pct_coords_na = mean(is.na(latitude)) * 100,
    pct_hour_na = mean(is.na(hour)) * 100
  )

# DECISION: cutoff chosen after inspecting the years above (adjust it).
# We set occurred_on to NA instead of dropping the row, so the complaint still
# counts for questions that do not depend on the date (borough, premises, ...).
cutoff <- as.Date("2015-01-01")

nypd_clean <- nypd |>
  mutate(
    occurred_on = if_else(occurred_on < cutoff, as.Date(NA), occurred_on),
    occurred_at = if_else(is.na(occurred_on), as.POSIXct(NA), occurred_at)
  )

sum(is.na(nypd_clean$occurred_on))
min(nypd_clean$occurred_on, na.rm = TRUE)


# ---------------------------------------------------------------------------
# 5. Reporting delay (reported_on - occurred_on) by completion status
# ---------------------------------------------------------------------------
delay_data <- nypd |>
  filter(!is.na(occurred_on), occurred_on >= cutoff) |>
  mutate(delay_days = as.integer(reported_on - occurred_on))

delay_data |>
  group_by(crm_atpt_cptd_cd) |>
  summarise(
    n = n(),
    median = median(delay_days),
    p90 = quantile(delay_days, 0.9),
    max = max(delay_days),
    n_negative = sum(delay_days < 0)
  )


# ---------------------------------------------------------------------------
# 6. Attempted vs completed by report year
# ---------------------------------------------------------------------------
# report_year is used (not occurrence year): it has no impossible dates.
# 2026 only goes up to June, so compare proportions, not raw counts.

# DECISION: drop the 161 rows with "(null)" completion status (about 0.005%)
status_year <- nypd |>
  filter(crm_atpt_cptd_cd != "(null)") |>
  count(report_year, crm_atpt_cptd_cd)

# Share of attempted offences per year
status_year |>
  group_by(report_year) |>
  mutate(pct = round(n / sum(n) * 100, 2)) |>
  filter(crm_atpt_cptd_cd == "ATTEMPTED")


# ---------------------------------------------------------------------------
# 7. Most frequent offence types per report year
# ---------------------------------------------------------------------------
# Check the offence column for missing values first
count(nypd, ofns_desc, sort = TRUE) |> slice_head(n = 15)

# Pick the most frequent offence types (adjust n to the group's decision)
top_offences <- nypd |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 5) |>
  pull(ofns_desc)

# Complaints per year for those offences
# (2026 is incomplete: exclude it, flag it in the figure, or use another measure)
offence_year <- nypd |>
  filter(ofns_desc %in% top_offences) |>
  count(report_year, ofns_desc)
