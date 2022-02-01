#### R version 4.1.1
####
#### R Workshop - Statistical Analysis
####
#### Statistical analysis using the Capital Bikeshare 2014 quarter data set

## load libs
library(tidyverse)
library(lubridate)
library(geosphere)

## Load dataframes as tibble
trips <- read_csv("volume/data/raw/bikeshare_trips.csv")
stations <- read_csv("volume/data/raw/bikeshare_stations.csv")

## look at tibbles
head(trips)
head(stations)

## Plot start time of day of casual vs registered users

ggplot(trips, aes(x = client, y = hour(sdate), color = client)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

ggplot(trips, aes(x = client, y = hour(sdate), fill = client)) +
  geom_violin() +
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

## take a stratified sample of the trips data set
trips_subset <- trips %>%
  group_by(client) %>%
  mutate(num_rows = n()) %>%
  sample_frac(0.001, weight = num_rows) %>%
  ungroup()

## Perform t-test to compare start time of day of casual vs registered users
(test <- t.test(hour(sdate) ~ client, data = trips_subset))


## Merge trips and stations data sets
station_colnames <- names(stations)
names(stations) <- paste0("s", station_colnames)
bikes <- left_join(trips, stations, by = c("sstation" = "sname"))
names(stations) <- paste0("e", station_colnames)
bikes <- left_join(bikes, stations, by = c("estation" = "ename"))

head(bikes)

## Calculate distances of the trips
bikes$distance <- distHaversine(bikes[, c("slong", "slat")], bikes[, c("elong", "elat")])

## Plot distances by client
ggplot(bikes, aes(x = distance, fill = client)) +
  geom_density(alpha = 0.3) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

## plot densities by time of day (hour min sec)
ggplot(bikes, aes(x = hour(sdate) + minute(sdate)/60, fill = client)) +
  geom_density(alpha = 0.3) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()
