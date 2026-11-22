# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Exploratory Data Analysis on NYC Subway

# Load the data only if it is not already in memory
if (!exists("nypd")) {source("R_Scripts/DV_Data_Loading.R")}

library(dplyr)
library(ggplot2)

## TOP PLACES - WHY NYC SUBWAY 
# Names of the 6 most frequent places types
top_places <- nypd |> count(prem_typ_desc, sort = TRUE) |> slice_head(n = 6)

# Reports per year for each place
place_year <- nypd |>
  filter(prem_typ_desc %in% top_places$prem_typ_desc) |>
  count(report_year, prem_typ_desc)

# Subway complaints per year
subway_year <- place_year |> filter(prem_typ_desc == "TRANSIT - NYC SUBWAY")

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
       subtitle = "2026 excluded (its data ends in June)",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to December 2025)") +
  scale_x_continuous(breaks = 2020:2025) + 
  scale_y_continuous(labels = scales::label_comma()) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(face = "bold"))
p1
ggsave("Figures/EDA/Top6Places_Streets_Subway.png", 
       plot = p1, width = 10, height = 6, dpi = 300)


## EDA ON NYC SUBWAY OCCURRENCES PER MONTH OF OCCURRENCE
# Table with offenses occurred in the subway per month per year 
subway_month <- nypd |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY",
         as.integer(format(occurred_on, "%Y")) == report_year) |>
  mutate(month = as.integer(format(occurred_on, "%m"))) |>
  count(report_year, month)
# Convert month numbers into an ordered factor with names (Jan, Feb, ...)
subway_month <- subway_month |>
  mutate(month = factor(month, levels = 1:12, labels = month.abb))

nypd |>
  filter(prem_typ_desc == "TRANSIT - NYC SUBWAY") |>
  summarise(share_dropped = mean(as.integer(format(occurred_on, "%Y")) != report_year))

# Heatmap of number of complaints in the NYC Subway per month over the years
p2 <- ggplot(subway_month, aes(factor(report_year), factor(month), fill = n)) + 
  geom_tile() +
  scale_y_discrete(limits = rev) +
  labs(title = "Subway complaints increased sharply since 2024",
       subtitle = "Complaints per month of occurrence, by report year (2026 data ends in June)", 
       x = "Year reported", y = "Month of occurrence", fill = "Number of Complaints",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2020 to June 2026)") +
  scale_fill_gradient(low = "#9ecae1", high = "#08306b") + 
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(face = "bold"))
p2

ggsave("Figures/EDA/HeatMap_Subway_Complaints.png", 
       plot = p2, width = 10, height = 6, dpi = 300)


## EDA ON NYC SUBWAY OCCURENCES PER WEEKDAY AND HOUR
## Question: "On which days and at what hours are subway complaints 
##           concentrated (between 2024 and 2026)?"
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
  labs(title = "1 in 5 complaints occurs Tuesday to Friday, 14:00–17:59",
       subtitle = "Subway complaints by weekday and hour of occurrence",
       x = "Hour of the day", y = NULL, fill = "Number of Complaints",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank(), 
        plot.caption = element_text(hjust = 0), 
        plot.title = element_text(face = "bold"))
p3

ggsave("Figures/EDA/HeatMap_Subway_Complaints_hour_wday.png", 
       plot = p3, width = 10, height = 6, dpi = 300)

## Question: "At what hours are subway complaints concentrated?"
p3_hour <- subway_data |> count(hour) |>
  ggplot(aes(hour, n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  labs(title = "Subway complaints peak in the afternoon, 14h-17h",
       subtitle = "Subway complaints by hour of occurrence",
       x = "Hour of the day", y = "Number of complaints",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"))
p3_hour
ggsave("Figures/EDA/Subway_Complaints_hour.png", 
       plot = p3_hour, width = 10, height = 6, dpi = 300)

p3_hour_percentage <- subway_data |> 
  count(hour) |> mutate(share = n / sum(n)) |>
  ggplot(aes(hour, share)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  labs(title = "Subway complaints peak in the afternoon, 14h-17h",
       subtitle = "Share of all subway complaints by hour of occurrence",
       x = "Hour of the day", y = "Share of subway complaints",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(hjust = 0),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"))
p3_hour_percentage

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
  slice_max(n, n = 5) |>
  ungroup()

p4 <- ggplot(top5_stations, aes(x = n, y = reorder(station_name, n))) +
  geom_col(fill = "grey", width = 0.65) +
  geom_col(data = top5_stations |> group_by(boro_nm) |> slice_max(n, n = 1),
           fill = "#E8792F", width = 0.65) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 4) +
  facet_wrap(~factor(boro_nm, 
                     levels = (subway_data |> count(boro_nm, sort = TRUE))$boro_nm), 
             scales = "free_y", ncol = 1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title = "Top 5 Subway Stations per Borough",
       x = NULL, y = NULL,
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        strip.text = element_text(face = "bold", size = 12),
        axis.text.y = element_text(size = 10),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0))
p4

ggsave("Figures/EDA/Top5_Stations_per_Borough.png", 
       plot = p4, width = 10, height = 14, dpi = 300)


## TRANSFORM FIGURES TO INTERACTIVE
library(tidyr)
library(plotly)
library(crosstalk)

# Subway complaints at the top 5 stations per borough, by hour of occurrence
station_hour <- subway_data |>
  semi_join(top5_stations, by = c("boro_nm", "station_name")) |>
  count(boro_nm, station_name, hour) |>
  complete(nesting(boro_nm, station_name), hour = 0:23,
           fill = list(n = 0)) |> # hours with no complaints get n = 0
  group_by(boro_nm, station_name) |>
  mutate(share = n / sum(n)) |> # percentage of complaints per hour of that station
  ungroup()

# Share of all subway complaints per hour (reference)
subway_hour <- subway_data |>
  count(hour) |>
  mutate(share = n / sum(n))

# SharedData links both plots: rows with the same key (borough + station) are 
# highlighted together across plots in the same group
shared_bars  <- SharedData$new(top5_stations, key = ~paste(boro_nm, station_name), group = "stations")
shared_lines <- SharedData$new(station_hour,  key = ~paste(boro_nm, station_name), group = "stations")

p4_shared <- ggplot(shared_bars, aes(x = n, y = reorder(station_name, n))) +
  geom_col(fill = "grey", width = 0.65) +
  geom_text(aes(label = scales::comma(n)), nudge_x = 150, size = 3) +
  facet_wrap(~factor(boro_nm, 
                     levels = (subway_data |> count(boro_nm, sort = TRUE))$boro_nm), 
             scales = "free_y", ncol = 1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title = "Top 5 Subway Stations per Borough",
       x = NULL, y = NULL,
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(size = 11, face = "bold"),
        strip.text = element_text(face = "bold", size = 9),
        axis.text.y = element_text(size = 7),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0))

# Hourly profile of the top 5 stations per borough vs all subway
p5 <- ggplot(shared_lines, aes(x = hour, y = share)) +
  # one grey line per station (a station = borough + name)
  geom_line(aes(group = station_name,
                text = paste0(station_name, " (", boro_nm, ")",
                          "\n", hour, ":00 - ", scales::percent(share, accuracy = 0.1),
                          " (", n, " complaints)")),
            colour = "grey", linewidth = 0.4) +
  # reference: share of all subway complaints per hour
  geom_line(data = subway_hour, colour = "black") +
  geom_point(data = subway_hour, colour = "black", size = 1) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = 0:23, expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  labs(title = "Share of each station’s complaints by hour of occurrence",
       subtitle = "Share of each station’s complaints by hour of occurrence",
       x = "Hour of the day", y = "Percentage of subway complaints per station",
       caption = "Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor.x = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 11, face = "bold"))

# Interactive barplot
bars_inter <- ggplotly(p4_shared, height = 700) |> highlight(color = "#E8792F") |>
  style(hoverinfo = "none")

# Interactive line plot
lines_inter <- ggplotly(p5, tooltip = "text", height = 700) |>
  highlight(color = "#E8792F", selected = attrs_selected(line = list(width = 3)))

# Put both interactive charts together
final_inter <- htmltools::tagList(htmltools::tags$h3("Clicking a station compares its hourly profile 
                                                    with all subway complaints (black line), 
                                                    revealing that many of the busiest stations 
                                                    have their own peaks.", 
                                                     style = "font-family: sans-serif;"),
                                  htmltools::tags$p("Instructions: Click on a bar or a line to highlight
                                                    a station and double-click to reset.", 
                                                    style = "font-family: sans-serif;"),
                                  bscols(widths = c(5, 7), bars_inter, lines_inter),
                                  htmltools::tags$p("Data: NYPD Complaint Data, NYC Open Data 
                                                    (complaints reported from January 2024 to June 2026)",
                                                    style = "font-family: sans-serif; font-size: 13px; color: grey;"))

htmltools::save_html(final_inter, "Figures/Nov22_ProgressCheck/Interactive/interactive_substations_hourly.html")
