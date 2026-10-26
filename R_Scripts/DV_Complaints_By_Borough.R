# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Comparison of offence types across NYC boroughs

library(dplyr)
library(tidyr)

# Count complaints by offence type and borough.
# Display each borough in a separate column.
# Replace missing offence-borough combinations with zero.
# Calculate total complaints for each offence type.
# Sort offence types by total complaints, highest first.
# Display the 10 most frequent offence types.
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

