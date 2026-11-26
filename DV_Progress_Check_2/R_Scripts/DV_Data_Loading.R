# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Data loading

library(dplyr)

nypd <- readRDS("Data/nypd_2020_2026.rds")
nypd <- as.data.frame(nypd)

str(nypd)


count(nypd, report_year)
count(nypd, boro_nm, sort = TRUE)
count(nypd, law_cat_cd)
count(nypd, ofns_desc, sort = TRUE) |> slice_head(n = 20)
count(nypd, prem_typ_desc, sort = TRUE) |> slice_head(n = 20)
count(nypd, hour)
nypd |> filter(!old_occurrence) |> count(wday)
