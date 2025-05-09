# Load required packages
library(tidyverse)
library(lubridate)
library(janitor)
library(readr)

# List all CSV files
files <- list.files("data/", pattern = "*.csv", full.names = TRUE)

# Load and combine
bike_data <- files %>%
  lapply(read_csv) %>%
  bind_rows()

# Clean column names
bike_data <- bike_data %>% clean_names()

# Check structure
glimpse(bike_data)

# Parse datetime
bike_data <- bike_data %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE),
    month = month(started_at, label = TRUE),
    hour = hour(started_at)
  )

# Remove bad data
bike_data <- bike_data %>%
  filter(
    ride_length > 1,
    ride_length < 1440,  # Less than 24 hours
    !is.na(member_casual)
  )

# Summary by user type
summary_stats <- bike_data %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride = mean(ride_length),
    median_ride = median(ride_length),
    max_ride = max(ride_length),
    total_rides = n()
  )

print(summary_stats)

# Average ride duration by day of week
bike_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(avg_duration = mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Avg Ride Duration by Day", y = "Minutes")


ggsave(
  filename = file.path("charts", "avg_ride_duration_day.png"),
  width = 25, height = 15, units = "cm", dpi = 300
)
  
#Most Popular Days (Total Ride Count per Day of Week)
#(The general values are very large, and shows exponential values, thus formatting them as regular integers)

#install.packages("scales")  # only once
library(scales)

bike_data %>%
  group_by(member_casual, day_of_week) %>%
  summarise(total_rides = n()) %>%
  ggplot(aes(x = day_of_week, y = total_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +  # 👈 this line formats the numbers
  labs(title = "Total Rides by Day of Week", y = "Total Rides")

ggsave(
  filename = file.path("charts", "total_rides_by_week.png"),
  width = 25, height = 15, units = "cm", dpi = 300
)

#Ride count by hour

bike_data %>%
  group_by(member_casual, hour) %>%
  summarise(ride_count = n()) %>%
  ggplot(aes(x = hour, y = ride_count, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +  # 👈 this line formats the numbers
  labs(title = "Ride Count by Hour", x = "Hour of Day", y = "Number of Rides")

ggsave(
  filename = file.path("charts", "ride_count_by_hour.png"),
  width = 25, height = 15, units = "cm", dpi = 300
)


#Ride start locations on MAP (TRY)

#install.packages("leaflet")
library(leaflet)
library(dplyr)

# Sample only 1000 points to avoid overloading the map
sample_map_data <- bike_data %>% 
  filter(!is.na(start_lat) & !is.na(start_lng)) %>%
  slice_sample(n = 1000) #sample_n() is from the dplyr package, so need to declare to access

leaflet(sample_map_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~start_lng,
    lat = ~start_lat,
    color = ~ifelse(member_casual == "member", "blue", "red"),
    radius = 2,
    popup = ~paste("Station:", start_station_name, "<br>Type:", member_casual)
  ) %>%
  addLegend("bottomright", 
            colors = c("blue", "red"),
            labels = c("Member", "Casual"),
            title = "User Type")


#Top 10 stations by usage

library(ggplot2)
library(forcats)

bike_data %>%
  group_by(start_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(start_station_name, total_rides), y = total_rides)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +  # 👈 fixing x axis
  labs(title = "Top 10 Most Used Start Stations", x = "Station", y = "Rides")

ggsave(
  filename = file.path("charts", "most_used_start_stations_NA.png"),
  width = 25, height = 15, units = "cm", dpi = 300
)


#selecting the top stations excluding the NA

bike_data %>%
  filter(!is.na(start_station_name)) %>%
  group_by(start_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(start_station_name, total_rides), y = total_rides)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +  # 👈 fixing x axis
  labs(title = "Top 10 Most Used Start Stations", x = "Station", y = "Rides")

ggsave(
  filename = file.path("charts", "most_used_start_stations_excluding_NA.png"),
  width = 25, height = 15, units = "cm", dpi = 300
)


#replacing NA with the value Unknown since the ride start places are not known

bike_data %>%
  mutate(start_station_name = ifelse(is.na(start_station_name), "Unknown", start_station_name)) %>%
  group_by(start_station_name) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(start_station_name, total_rides), y = total_rides)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +  # 👈 fixing x axis
  labs(title = "Top 10 Most Used Start Stations", x = "Station", y = "Rides")

ggsave(
  filename = file.path("charts", "most_used_start_stations.png"),
  width = 25, height = 15, units = "cm", dpi = 300
)
