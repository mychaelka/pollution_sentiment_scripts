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

## filter out bots (do not delete the bots data!! Need them for placebo tests)
#bots <- read_csv("../data/bot_usernames.csv")
#data <- data |> filter(!(username %in% bots$username))

data <- data %>% mutate(id = `...1`) %>% 
  dplyr::select(-geo_available,-`...1`) %>%
  separate_wider_delim(coordinates, names = c("lon", "lat"), delim = ' ') %>% 
  mutate(lon = as.double(lon), lat = as.double(lat)) %>% 
  sf::st_as_sf(
    coords = c("lat", "lon"), 
    crs = 4326)

usa_counties <- st_read("../data/USA_shapefiles/counties/") %>% 
  dplyr::select(NAME, STATE_NAME, STATE_FIPS, CNTY_FIPS, POP2010) %>% 
  filter(STATE_NAME != "Alaska") %>% 
  filter(STATE_NAME != "Hawaii")

sf_use_s2(FALSE)
data <- st_intersection(data, usa_counties)

parse_nums <- function(df) {
  res <- df %>% mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, as.character)) %>% 
    mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, parse_number))
  res
}

ecmwf <- read_stars("../data/ECMWF/july2015_new.nc")
#weather <- read_stars("../data/ECMWF/weather.nc")


temperature <- st_as_sf(weather[1]) %>% parse_nums() %>%  # temperature and dewpoint have really coarse resolution -- add grid from pm25?
  dplyr::mutate(
    id = row_number()
  )
dewpoint <- st_as_sf(weather[2]) %>% parse_nums() %>% 
  dplyr::mutate(
    id = row_number()
  )

pm25 <- st_as_sf(ecmwf[6]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::mutate(
    id = row_number()
  )
aod550 <- st_as_sf(ecmwf[8]) %>% parse_nums() %>%
  dplyr::mutate(
    id = row_number()
  )
so2 <- st_as_sf(ecmwf[22]) %>% parse_nums() %>%
  dplyr::mutate(
    id = row_number()
  )
wind <- st_as_sf(ecmwf[1]) %>% parse_nums() %>%
  dplyr::mutate(
    id = row_number()
  )


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

names(so2)[names(so2) %in% colnames] <- 
  rename_midnight(names(so2)[names(so2) %in% colnames])

names(wind)[names(wind) %in% colnames] <- 
  rename_midnight(names(wind)[names(wind) %in% colnames])

pm25geo <- pm25
aod550geo <- aod550
so2geo <- so2
temperaturegeo <- temperature
dewpointgeo <- dewpoint
windgeo <- wind

pm25 <- pm25 %>% st_drop_geometry() %>% as_tibble()
aod550 <- aod550 %>% st_drop_geometry() %>% as_tibble()
so2 <- so2 %>% st_drop_geometry() %>% as_tibble()
temperature <- temperature %>% st_drop_geometry() %>% as_tibble()
dewpoint <- dewpoint %>% st_drop_geometry() %>% as_tibble()
wind <- wind %>% st_drop_geometry() %>% as_tibble()

create_long <- function(df, col_name) {
  df <- df %>% 
    pivot_longer(-id, names_to = "timestamp", values_to = col_name) %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", 
                                  tz = "UTC"))
  
  df
}

pm25 <- create_long(pm25, "pm25")
aod550 <- create_long(aod550, "aod550")
so2 <- create_long(so2, "so2")
temperature <- create_long(temperature, "temperature")
dewpoint <- create_long(dewpoint, "dewpoint")
wind <- create_long(wind, "wind")

no_cores <- detectCores() - 1

process_row <- function(xd) {
   ID <- xd %>% pull(id) %>% first()
   subset_df <- xd %>% dplyr::select(-id) %>% arrange(timestamp)
   
   timestamps <- seq(as.POSIXct("2015-07-01 00:00:00"), as.POSIXct("2015-07-31 21:00:00"), by = "3 hours")
   finer_timestamps <- seq(as.POSIXct("2015-07-01 00:00:00"), as.POSIXct("2015-07-31 21:00:00"), by = "hour")
   ts_data <- zoo(subset_df$pm25, order.by = timestamps)
   interpolated <- na.approx(ts_data, xout = finer_timestamps)
   
   tibble(timestamp = index(interpolated), pm25 = coredata(interpolated)) %>% 
     mutate(id = ID)
 }

library(furrr)

plan(multisession, workers = no_cores)

pm25_interpolated <- 
  pm25 %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

aod550_interpolated <- 
  aod550 %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

so2_interpolated <- 
  so2 %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

temperature_interpolated <- 
  temperature %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

dewpoint_interpolated <- 
  dewpoint %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

wind_interpolated <- 
  wind %>% 
  split(.$id) %>% 
  future_map_dfr(process_row)

data <- data %>% 
  mutate(
    time_bin = timestamp %>% floor_date(unit = "hour")
  )


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

a <- full_data %>% 
  left_join(
    ., 
    rename(wind_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
              )
  ) 

full_data <- a %>% 
  left_join(
    ., 
    rename(so2_interpolated, id_geo = id, time_bin = timestamp) %>% 
      mutate(
        time_bin = time_bin %>% floor_date(unit = "hour") %>% as.character()
      )
  ) 
  

full_data <- full_data %>% mutate(time_bin = floor_date(timestamp, unit = "hour"))
save(full_data, file = "../data/full_data_counties.RData")
