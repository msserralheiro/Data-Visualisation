# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Shared data for the Bronx analysis
#
# Every new script will start with: source("R_Scripts/DV_Bronx_Data.R")

if (!exists("nypd")) source("R_Scripts/DV_Data_Loading.R")

library(dplyr)


## 1. Period: complaints reported from January 2024 to June 2026
nypd_period <- nypd |>
  filter(report_year >= 2024)

nrow(nypd_period)  # 1 436 180


## 2. Remove complaints without a borough
nrow(filter(nypd_period, is.na(boro_nm)))  # 2 980 rows removed

nypd_period <- nypd_period |>
  filter(!is.na(boro_nm))

nrow(nypd_period)  # 1 433 200


## 3. New columns
age_groups <- c("<18", "18-24", "25-44", "45-64", "65+")

nypd_period <- nypd_period |>
  mutate(area = if_else(boro_nm == "BRONX", "Bronx", "Rest of NYC"),
         susp_age = if_else(susp_age_group %in% age_groups,
                            susp_age_group, "UNKNOWN"))

count(nypd_period, area)
# Bronx 319 193, Rest of NYC 1 114 007


## 4. Bronx and the rest of NYC
bronx    <- nypd_period |> filter(area == "Bronx")
rest_nyc <- nypd_period |> filter(area == "Rest of NYC")

# Check: the two parts add up to the whole
nrow(bronx) + nrow(rest_nyc) == nrow(nypd_period)  # TRUE

count(bronx, susp_age, sort = TRUE)

## 5. Bronx precincts
# The Bronx has 12 precincts
bronx_precincts <- c("40", "41", "42", "43", "44", "45",
                     "46", "47", "48", "49", "50", "52")

nrow(filter(bronx, !addr_pct_cd %in% bronx_precincts))  # 62


## 6. Street complaints with ~100 m blocks
nrow(filter(nypd_period, prem_typ_desc == "STREET", latitude == 0))  # 7 (4 of them in the Bronx)
nrow(filter(nypd_period, prem_typ_desc == "STREET", area == "Bronx",
            !addr_pct_cd %in% bronx_precincts))  # 22

nyc_street <- nypd_period |>
  filter(prem_typ_desc == "STREET",
         !is.na(latitude), !is.na(longitude),
         latitude != 0, longitude != 0,
         area == "Rest of NYC" | addr_pct_cd %in% bronx_precincts) |>
  mutate(block_lat = round(latitude, 3),
         block_lon = round(longitude, 3),
         block = paste(block_lat, block_lon))

nrow(nyc_street)  # 427 551 (all boroughs)

bronx_street <- nyc_street |> filter(area == "Bronx")

nrow(bronx_street)  # 108 220
range(bronx_street$latitude)   # all inside the Bronx
range(bronx_street$longitude)


## 7. Subway complaints in the Bronx
bronx_subway <- bronx |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY")

nrow(bronx_subway)  # 15 152
sum(bronx_subway$station_name == "(null)")  # 0: every Bronx subway row has a station


## 8. Round-hour check (unknown times may be recorded as 00:00 or 12:00)
bronx |> count(hour) |> filter(hour %in% c(0, 1, 11, 12, 13))
