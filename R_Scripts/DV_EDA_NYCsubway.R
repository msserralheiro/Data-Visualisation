# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Exploratory Data Analysis on NYC Subway

# Load the data only if it is not already in memory
if (!exists("nypd")) {source("R_Scripts/DV_Data_Loading.R")}

library(dplyr)
library(ggplot2)

## TOP PLACES - WHY NYC SUBWAY 
# Names of the 6 most frequent places types
top_places <- nypd |> count(prem_typ_desc, sort = TRUE) |>
  slice_head(n = 6)

# Reports per year for each place
place_year <- nypd |>
  filter(prem_typ_desc %in% top_places$prem_typ_desc) |>
  count(report_year, prem_typ_desc)

# Subway complaints per year
subway_year <- place_year |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY")

library(forcats)

# Obtain order of places and apply to the chart
legend_order <- place_year |> group_by(prem_typ_desc) |>
  slice_max(report_year, n = 1) |> arrange(desc(n)) |>
  pull(prem_typ_desc)

place_year$prem_typ_desc <- 
  factor(place_year$prem_typ_desc, levels = legend_order)

# Table for this figure only: 2026 is excluded because its data ends in June
place_year_plot <- place_year |>
  filter(report_year < 2026)

p1 <- ggplot(place_year_plot, aes(x = report_year, y = n, color = prem_typ_desc)) +
  geom_line() +
  geom_line(data = place_year_plot |> filter(prem_typ_desc %in% c("STREET", "TRANSIT - NYC SUBWAY")),
    linewidth = 2) +
  labs(x = "Year reported", y = "Number of complaints", color = "Places",
       title = "Top 6 places with the most complaints between 2020 and 2025",
       subtitle = "Street and NYC Subway highlighted: the two premises types analysed in this project
2026 is excluded because its data ends in June",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)") +
  scale_x_continuous(breaks = 2020:2025) + 
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_size = 13) +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(face = "bold"))

ggsave("Figures/Top6Places_Streets_Subway.png", 
       plot = p1, width = 10, height = 6, dpi = 300)


## EDA ON NYC SUBWAY OCCURENCES PER MONTH
# Table with offenses occurrenced in the subway per month per year 
subway_month <- nypd |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY") |>
  mutate(month = as.integer(format(reported_on, "%m"))) |>
  count(report_year, month)
# Convert month numbers into an ordered factor with names (Jan, Feb, ...)
subway_month <- subway_month |>
  mutate(month = factor(month, levels = 1:12, labels = month.abb))

# Heatmap of number of complaints in the NYC Subway per month over the years
p2 <- ggplot(subway_month, aes(factor(report_year), factor(month), fill = n)) + 
  geom_tile(colour = "white") +
  scale_y_discrete(limits = rev) +
  labs(title = "Subway complaints increased sharply since 2024",
       subtitle = "Complaints per month, by report year (2026 data ends in June)", 
       x = "Year reported", y = "Month", fill = "Number of Complaints",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)") +
  scale_fill_gradient(low = "#9ecae1", high = "#08306b") + 
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(face = "bold"))
p2

ggsave("Figures/HeatMap_Subway_Complaints.png", 
       plot = p2, width = 10, height = 6, dpi = 300)

# Legend of the Figure 2: 
# Monthly subway complaints, 2020 to June 2026. 

# The heatmap suggested two jumps between 2023, 2024 and between 2024 and 2025 
# and raised the question of seasonality.


## EDA ON NYC SUBWAY OCCURENCES PER WEEKDAY AND HOUR
## Question: "On which days and at what hours are subway complaints concentrated (between 2024 and 2026)?"
subway_data <- nypd |> filter(prem_typ_desc == "TRANSIT - NYC SUBWAY",
                              report_year >= 2024) |>
  mutate(wday = factor(wday, levels = levels(wday), 
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
subway_data |> count(wday)
subway_data |> count(hour)

# Check NA values for hour column
nypd |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY", report_year >= 2024) |>
  summarise(n_na_hour = sum(is.na(hour)), total = n())

# How many hour-0 rows are exactly 00:00:00 ro 12:00:00?
nypd |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY", report_year >= 2024, hour == 0) |>
  count(exact_midnight = cmplnt_fr_tm == "00:00:00")
## NOTE: “We checked whether unknown times are recorded as 00:00:00 or 12:00:00 
##        in subway complaints: exact-hour records vary about 
##        200 and 500 per hour in every hour tested, so we kept all rows.”

p3 <- subway_data |> count(wday, hour) |>
  ggplot(aes(hour, wday, fill = n)) + 
  geom_tile() +
  scale_y_discrete(limits = rev) + scale_x_continuous(breaks = 0:23, expand = c(0, 0)) + 
  scale_fill_gradient(low = "#9ecae1", high = "#08306b") +
  labs(title = "One in five complaints falls in under 10% of the total weekly hours.",
       subtitle = "Tuesday to Friday, 14:00–17:59. Subway complaints by weekday and hour of occurrence",
       x = "Hour of the day", y = NULL, fill = "Number of Complaints",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 13) + 
  theme(panel.grid = element_blank(), plot.caption = element_text(hjust = 0), 
        plot.title = element_text(face = "bold"))
p3

ggsave("Figures/HeatMap_Subway_Complaints_hour_wday.png", 
       plot = p3, width = 10, height = 6, dpi = 300)

# Percentage of all subway complaints that fall in Tue-Fri, between 14h and 18h (exclusive)
# (compare it with the percentage of the week that block represents: 4 days x 4 hours = 16 of 168 cells, about 9.5%)
subway_data |>
  summarise(share_block = mean(wday %in% c("Tue", "Wed", "Thu", "Fri") & hour %in% 14:17))
# Results: About 21.1% of complaints fall on Tuesday to Friday afternoons.


## EDA ON STATION NAMES AND BOROUGHS
subway_data |> count(station_name, sort = TRUE) # 3 "(null)" rows 
subway_data |> count(boro_nm, sort = TRUE) # 24 NA rows -> remove these lines

# Complaints per station and borough
subway_station <- subway_data |> 
  filter(!is.na(boro_nm), station_name != "(null)") |> 
  count(station_name, boro_nm, sort = TRUE)

# Top 5 Stations per Borough
top5_stations <- subway_station |> 
  group_by(boro_nm) |> 
  slice_max(n, n = 5) 

p4 <- ggplot(top5_stations, aes(x = n, y = reorder(station_name, n))) +
  geom_col(fill = "grey", width = 0.65) +
  geom_col(data = top5_stations |> group_by(boro_nm) |> slice_max(n, n = 1),
           fill = "#E8792F", width = 0.65) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 4) +
  facet_wrap(~factor(boro_nm, 
                     levels = (subway_data |> count(boro_nm, sort = TRUE))$boro_nm), 
             scales = "free_y", ncol = 1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title = "LALALALALA",
       x = NULL, y = NULL,
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 13),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0))
p4

ggsave("Figures/Top5_Stations_per_Borough.png", 
       plot = p4, width = 10, height = 14, dpi = 300)
  