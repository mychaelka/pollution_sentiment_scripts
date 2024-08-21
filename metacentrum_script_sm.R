library(tidyverse)
library(sf)
library(stars)
library(zoo)
library(parallel)
library(terra)

## LOAD DATA
vader <- read_delim('../data/tweets/all/july2015_vader_sentiment.csv', delim = '|')
roberta <- read_delim('../data/tweets/all/july2015_roberta_sentiment.csv', delim = '|') %>% 
  dplyr::select(`...1`, roberta_positive, roberta_neutral, roberta_negative)
bertweet <- read_delim('../data/tweets/all/july2015_bertweet_sentiment.csv', delim = '|') %>%
  dplyr::select(`...1`, bertweet_positive, bertweet_neutral, bertweet_negative)
topics <- read_delim('../data/tweets/all/july2015_topics.csv', delim = '|') %>%
  dplyr::select(`...1`, `arts_&_culture`:`label`)

## join sentiment measures and topics
data <- merge(vader, roberta, by = '...1') %>% 
  merge(bertweet, by = '...1') %>% 
  merge(topics, by = '...1')

rm(vader, roberta, bertweet, topics)
gc()

data <- data %>% mutate(id = `...1`) %>% 
  dplyr::select(-geo_available,-`...1`) %>%
  separate_wider_delim(coordinates, names = c("lon", "lat"), delim = ' ') %>% 
  mutate(lon = as.double(lon), lat = as.double(lat)) %>% 
  sf::st_as_sf(
    coords = c("lat", "lon"), 
    crs = 4326)

usa_counties <- st_read("../data/USA_shapefiles/counties/") %>% 
  dplyr::select(NAME, STATE_NAME, POP2010) %>% 
  filter(STATE_NAME != "Alaska") %>% 
  filter(STATE_NAME != "Hawaii")

sf_use_s2(FALSE)
data <- st_intersection(data, usa_counties)

parse_nums <- function(df) {
  res <- df %>% mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, as.character)) %>% 
    mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, parse_number))
  res
}

## weather has columns ordered differently than pollution
parse_nums_weather <- function(df) {
  res <- df %>% mutate(across(`2015-07-01 00:00:00`:`2015-07-31 09:00:00`, as.character)) %>% 
    mutate(across(`2015-07-01 00:00:00`:`2015-07-31 09:00:00`, parse_number))
  res
}

weather <- read_stars("../data/ECMWF/weather_all.nc")
pollution <- read_stars("../data/ECMWF/ecmwf_july.nc")

wind_x <- st_as_sf(weather[1]) %>% parse_nums_weather() %>%  # component of wind along y axis
  dplyr::mutate(
    id = row_number()
  )
wind_y <- st_as_sf(weather[2]) %>% parse_nums_weather() %>%  # component of wind along y axis
  dplyr::mutate(
    id = row_number()
  )
dewpoint <- st_as_sf(weather[3]) %>% parse_nums_weather() %>% 
  mutate(across(`2015-07-01 00:00:00`:`2015-07-31 09:00:00`, ~.-272.15)) %>%
  dplyr::mutate(
    id = row_number()
  )
temperature <- st_as_sf(weather[4]) %>% parse_nums_weather() %>%
  mutate(across(`2015-07-01 00:00:00`:`2015-07-31 09:00:00`, ~.-272.15)) %>%
  dplyr::mutate(
    id = row_number()
  )
clouds <- st_as_sf(weather[5]) %>% parse_nums_weather() %>%
  dplyr::mutate(
    id = row_number()
  )
precipitation <- st_as_sf(weather[6]) %>% parse_nums_weather() %>%
  mutate(across(`2015-07-01 00:00:00`:`2015-07-31 09:00:00`, ~.*1000)) %>%
  dplyr::mutate(
    id = row_number()
  )
visibility <- st_as_sf(weather[7]) %>% parse_nums_weather() %>%
  dplyr::mutate(
    id = row_number()
  )
pm25 <- st_as_sf(pollution[4]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::mutate(
    id = row_number()
  )
aod550 <- st_as_sf(pollution[5]) %>% parse_nums() %>%
  dplyr::mutate(
    id = row_number()
  )


rm(weather, pollution)
gc()

# Function to adjust column names
rename_midnight <- function(names) {
  sapply(names, function(name) {
    if (lengths(strsplit(name, " ")) == 1) {
      paste(name, "00:00:00")
    } else {
      name
    }
  })
}

colnames <- setdiff(names(pm25), c("geometry", "id"))

names(pm25)[names(pm25) %in% colnames] <- 
  rename_midnight(names(pm25)[names(pm25) %in% colnames])

names(aod550)[names(aod550) %in% colnames] <- 
  rename_midnight(names(aod550)[names(aod550) %in% colnames])

wind_xgeo <- wind_x
wind_ygeo <- wind_y
dewpointgeo <- dewpoint
temperaturegeo <- temperature
cloudsgeo <- clouds
precipitationgeo <- precipitation
visibilitygeo <- visibility
pm25geo <- pm25
aod550geo <- aod550

wind_x <- wind_x %>% st_drop_geometry() %>% as_tibble()
wind_y <- wind_y %>% st_drop_geometry() %>% as_tibble()
dewpoint <- dewpoint %>% st_drop_geometry() %>% as_tibble()
temperature <- temperature %>% st_drop_geometry() %>% as_tibble()
clouds <- clouds %>% st_drop_geometry() %>% as_tibble()
precipitation <- precipitation %>% st_drop_geometry() %>% as_tibble()
visibility <- visibility %>% st_drop_geometry() %>% as_tibble()
pm25 <- pm25 %>% st_drop_geometry() %>% as_tibble()
aod550 <- aod550 %>% st_drop_geometry() %>% as_tibble()


create_long <- function(df, col_name) {
  df <- df %>% 
    pivot_longer(-id, names_to = "timestamp", values_to = col_name) %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", 
                                  tz = "UTC"))
  
  df
}

wind_x <- create_long(wind_x, "wind_x")
wind_y <- create_long(wind_y, "wind_y")
dewpoint <- create_long(dewpoint, "dewpoint")
temperature <- create_long(temperature, "temperature")
clouds <- create_long(clouds, "clouds")
precipitation <- create_long(precipitation, "precipitation")
visibility <- create_long(visibility, "visibility")
pm25 <- create_long(pm25, "pm25")
aod550 <- create_long(aod550, "aod550")

no_cores <- detectCores() - 1

# in the following function, change variable to what you currently need (dewpoint, temperature, pm25,...)
process_row <- function(xd) {
   ID <- xd %>% pull(id) %>% first()
   subset_df <- xd %>% dplyr::select(-id) %>% arrange(timestamp)
   
   timestamps <- seq(as.POSIXct("2015-07-01 00:00:00"), as.POSIXct("2015-07-31 21:00:00"), by = "3 hours")
   finer_timestamps <- seq(as.POSIXct("2015-07-01 00:00:00"), as.POSIXct("2015-07-31 21:00:00"), by = "hour")
   ts_data <- zoo(subset_df$clouds, order.by = timestamps)
   interpolated <- na.approx(ts_data, xout = finer_timestamps)
   
   tibble(timestamp = index(interpolated), clouds = coredata(interpolated)) %>% 
     mutate(id = ID)
 }

library(furrr)

plan(multisession, workers = no_cores)

wind_x_interpolated <- 
  wind_x %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

wind_y_interpolated <- 
  wind_y %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

dewpoint_interpolated <- 
  dewpoint %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

temperature_interpolated <- 
  temperature %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

clouds_interpolated <- 
  clouds %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

precipitation_interpolated <- 
  precipitation %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

visibility_interpolated <- 
  visibility %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

pm25_interpolated <- 
  pm25 %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

aod550_interpolated <- 
  aod550 %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)


data <- data %>% 
  mutate(
    time_bin = timestamp %>% floor_date(unit = "hour")
  )


# join with pm25
full_data <- 
  pm25geo %>% 
  dplyr::select(id) %>% 
  st_set_crs(st_crs(data)) %>% 
  rename(id_geo = id) %>% 
  st_join(data %>% 
            mutate(time_bin = as.character(time_bin)),.) %>% 
  left_join(
    .,
    rename(pm25_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
      time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()  # change to character is necessary due to joining
      )
  )

# join with aod550
a <- full_data %>% 
  left_join(
    ., 
    rename(aod550_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
              )
  ) 

# join with wind_x
full_data <- a %>% 
  left_join(
    ., 
    rename(wind_x_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

# join with wind_y
a <- full_data %>% 
  left_join(
    ., 
    rename(wind_y_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

# join with dewpoint
full_data <- a %>% 
  left_join(
    ., 
    rename(dewpoint_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

# join with temperature
a <- full_data %>% 
  left_join(
    ., 
    rename(temperature_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

# join with cloud cover
full_data <- a %>% 
  left_join(
    ., 
    rename(clouds_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

# join with precipitation
a <- full_data %>% 
  left_join(
    ., 
    rename(precipitation_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

# join with visibility
full_data <- a %>% 
  left_join(
    ., 
    rename(visibility_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 

full_data <- full_data %>% mutate(time_bin = floor_date(timestamp, unit = "hour"))
save(full_data, file = "../data/full_data_counties.RData")
