# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Comparison of offence types across NYC boroughs

library(dplyr)
library(tidyr)
library(ggplot2)


dv_blue   <- "#2E75B6"
dv_orange <- "#E8792F"


# Count complaints by offence type and borough.
# Display each borough in a separate column.
# Replace missing offence-borough combinations with zero.
# Calculate total complaints for each offence type.
# Sort offence types by total complaints, highest first.
# Display the 5 most frequent offence types.
#
# Note: Totals include complaints with an unknown borough (NA).
# AI assistance: ChatGPT helped with data aggregation,
# reshaping, sorting, and explaining the R functions.

nypd |>
  count(ofns_desc, boro_nm) |>
  pivot_wider(
    names_from = boro_nm,
    values_from = n,
    values_fill = 0
  ) |>
  mutate(total = rowSums(across(where(is.numeric)))) |>
  arrange(desc(total)) |>
  slice_head(n = 10)


# Verify the most frequently reported offence types in the Bronx

nypd |>
  filter(boro_nm == "BRONX") |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 5)

off_bronx <- nypd |>
  filter(boro_nm == "BRONX") |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 5) |>
  ggplot(aes(x= reorder(ofns_desc, -n), y = n)) +
  geom_col(fill = dv_blue, position = position_dodge(width = 0.8), width = 0.75) + 
  labs(title = "Most frequent reported offence types in the Bronx", x=NULL, y = "Number of reports") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

off_bronx

ggsave("Bronx_Most_Frequent_Reported_Offence_Types.png", off_bronx, width = 7, height = 5, dpi = 300)


# Verify the most frequently reported offence types in Staten Island

nypd |>
  filter(boro_nm == "STATEN ISLAND") |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 5)

off_staten_island <- nypd |>
  filter(boro_nm == "STATEN ISLAND") |>
  count(ofns_desc, sort = TRUE) |>
  slice_head(n = 5) |>
  ggplot(aes(x= reorder(ofns_desc, -n), y = n)) +
  geom_col(fill = dv_orange, position = position_dodge(width = 0.8), width = 0.75) + 
  labs(title = "Most frequent reported offence types in Staten Island", x=NULL, y = "Number of reports") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

off_staten_island

ggsave("Staten_Island_Most_Frequent_Reported_Offence_Types.png", off_staten_island, width = 7, height = 5, dpi = 300)
