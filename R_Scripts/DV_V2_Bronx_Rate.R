# Data Visualization - Group Project
# NYPD Complaint Data (2020-2026)
# Why the Bronx: the highest complaint rate per resident

# Population: 2020 U.S. Census 

source("R_Scripts/DV_Bronx_Data.R")

library(ggplot2)

dv_orange <- "#E8792F"
dv_grey   <- "#9AA3AB"


## 1. Monthly complaints per 1,000 residents, by borough 
pop_2020 <- data.frame(
  boro_nm = c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"),
  pop     = c(1472654, 2736074, 1694251, 2405464, 495747))

monthly_rate <- nypd_period |>
  mutate(month = as.Date(format(reported_on, "%Y-%m-01"))) |>
  count(boro_nm, month) |>
  left_join(pop_2020, by = "boro_nm") |>
  mutate(per_1000 = n / pop * 1000,
         bronx = boro_nm == "BRONX",
         borough = tools::toTitleCase(tolower(boro_nm)))

# Check: 30 months x 5 boroughs, and the counts add up to the period
nrow(monthly_rate)                          # 150
sum(monthly_rate$n) == nrow(nypd_period)    # TRUE

# Which borough has the highest rate each month?
monthly_rate |>
  group_by(month) |>
  slice_max(per_1000, n = 1) |>
  ungroup() |>
  count(boro_nm)
# BRONX 30: the Bronx is highest in all 30 months 

months_bronx_top <- monthly_rate |>
  group_by(month) |>
  slice_max(per_1000, n = 1) |>
  ungroup() |>
  summarise(bronx = sum(boro_nm == "BRONX"), months = n())

# Average monthly rate per borough
monthly_rate |>
  group_by(borough) |>
  summarise(avg_per_1000 = mean(per_1000))
# Bronx 7.2, Manhattan 6.7, Brooklyn 4.9, Queens 4.3, Staten Island 4.0


## 2. Figure 
end_labels <- monthly_rate |>
  filter(month == max(month))

p_h0a <- ggplot(monthly_rate,
                aes(x = month, y = per_1000, group = borough,
                    colour = bronx, linewidth = bronx)) +
  geom_line() +
  geom_text(data = end_labels,
            aes(label = borough), hjust = -0.1, size = 3.6,
            show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = dv_orange, `FALSE` = dv_grey), guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.7), guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.12))) +
  scale_y_continuous(breaks = 3:8) +   
  labs(title = paste0("The Bronx has the highest complaint rate of any borough in all ",
                      months_bronx_top$months, " months"),
       subtitle = "Complaints per 1,000 residents, by month reported (January 2024 - June 2026)",
       x = NULL, y = "Complaints per 1,000 residents",
       caption = paste("Data: NYPD Complaint Data, NYC Open Data (complaints reported from January 2024 to June 2026)",
                       "Population: U.S. Census Bureau, 2020 Census (fixed), via NYC Department of City Planning",
                       sep = "\n")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1),
        panel.grid.minor = element_blank())

p_h0a

ggsave("Figures/Nov22_ProgressCheck/Temporal_Patterns/V2_Monthly_Borough_Complaints_Per_1000.png",
       plot = p_h0a, width = 10, height = 6, dpi = 300, bg = "white")
