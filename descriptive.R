library(tidyverse)
library(lutz)
library(sf)
library(raster)
library(viridis)

load("../data/full_data_counties.RData")

full_data$timezone <- tz_lookup(full_data, method = "accurate")
full_data$local_time <- with_tz(full_data$timestamp, tzone = full_data$timezone)
full_data$local_timebin <- with_tz(full_data$time_bin, tzone = full_data$timezone)

# extract longitute/latitude from geometry
full_data <- full_data %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

# cut pm25 into bins
full_data <- full_data %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 150, by = 10), Inf), include.lowest = TRUE)
  )

# how many tweets per each pollution bin? 
full_data %>%
  ggplot(aes(x=pm25_cat)) + 
  geom_histogram(stat="count") +
  theme_bw(base_family = "Times")

# relationship between #tweets and pollution
full_data %>% 
  mutate(pm25_int = floor(pm25)) %>% 
  group_by(pm25_int) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=pm25_int, y=count)) +
  geom_line()

# how many users per each pollution bin?
full_data %>%
  group_by(username, pm25_cat) %>% 
  summarize(user_count = n())
  ggplot(aes(x=pm25_cat)) + 
  geom_histogram(stat="count") +
  theme_bw(base_family = "Times")

usa_counties <- st_read("../data/USA_shapefiles/counties/") |>
  dplyr::select(NAME, STATE_NAME, STATE_FIPS, CNTY_FIPS, POP2010) |>
  filter(STATE_NAME != "Alaska") |>
  filter(STATE_NAME != "Hawaii")
  
full_data_polygons <- full_data %>% 
  st_drop_geometry() %>% 
  left_join(usa_counties, ., by=c("STATE_FIPS" = "STATE_FIPS", "CNTY_FIPS" = "CNTY_FIPS", "NAME" = "NAME", "STATE_NAME" = "STATE_NAME"))

# use ecmwf data instead of this -- not every county is in full_data
# mean pollution by county
full_data_polygons %>% 
  st_drop_geometry() %>% 
  group_by(STATE_FIPS, CNTY_FIPS) %>% 
  summarise(mean_pollution = mean(pm25, na.rm = TRUE)) %>% 
  left_join(usa_counties, ., by=c("STATE_FIPS" = "STATE_FIPS", "CNTY_FIPS" =
                                    "CNTY_FIPS")) %>% 
  ggplot(aes(fill=mean_pollution)) + 
  geom_sf() + 
  scale_fill_viridis(
    option="magma",
    direction=-1
  )  

# mean sentiment (vader) by county
full_data_polygons %>% 
  st_drop_geometry() %>% 
  group_by(STATE_FIPS, CNTY_FIPS) %>% 
  summarise(mean_vader = mean(vader_compound, na.rm = TRUE)) %>% 
  left_join(usa_counties, ., by=c("STATE_FIPS" = "STATE_FIPS", "CNTY_FIPS" =
                                    "CNTY_FIPS")) %>% 
  ggplot(aes(fill=mean_vader)) + 
  geom_sf() + 
  scale_fill_viridis(
    option="magma",
    direction=-1
  )  

# number of tweets by county (per capita)
full_data_polygons %>% 
  st_drop_geometry() %>% 
  group_by(STATE_FIPS, CNTY_FIPS) %>% 
  summarise(num_tweets = n()/POP2010.x) %>% 
  left_join(usa_counties, ., by=c("STATE_FIPS" = "STATE_FIPS", "CNTY_FIPS" =
                                    "CNTY_FIPS")) %>% 
  ggplot(aes(fill=num_tweets)) + 
  geom_sf() + 
  scale_fill_viridis(
    option="magma",
    direction=-1
  )
  
# tweet distribution -- need to keep the grid information in table
grid <- st_make_grid(st_as_sfc(st_bbox(full_data)), cellsize = c(0.75, 0.75))
grid <- st_as_sf(grid)

# Count points in each grid cell
a <- st_join(grid, full_data)
points_count <- st_intersects(full_data, grid) %>% lengths()

# Create a new column in the grid with the count of points in each cell
grid$points_count <- points_count

# Plot the grid
plot(grid)