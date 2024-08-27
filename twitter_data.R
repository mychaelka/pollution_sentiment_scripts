library(tidyverse)
library(sf)
library(ggspatial)
library(viridis)
library(stars)
library(zoo)
library(parallel)
library(terra)
library(ggplot2)
library(plm)

# data from GESIS
#data <- read_delim('../data/tweets/05-07_07_2015_geocoded.csv', delim = '|')
#geocoded <- data[data$geo_available == 1,]  # 13% of tweets have gps coordinates
#write_delim(geocoded, file = '../data/tweets/05-07_07_2015_geocoded.csv', delim = '|')
#data <- read_delim('../data/tweets/2015-07-06_geocoded.csv', delim = '|')

##### calculate sentiment in python before proceeding #####
# load geocoded data with already calculated sentiment
data <- read_delim('../data/05-07_07_2015_vader_sentiment.csv', delim = '|')
data <- data[,-1]

# shapefiles
#world <- st_read("../data/USA_shapefiles/World_Countries_Generalized/World_Countries_Generalized.shp")
usa_states <- st_read("../data/USA_shapefiles/States_shapefile/States_shapefile.shp")
#usa_states <- vect("../data/USA_shapefiles/States_shapefile/States_shapefile.shp")
#usa_full <- vect("../data/USA_shapefiles/gadm41_USA_shp/gadm41_USA_0.shp")

# split coordinates and transform to spatial data frame
data <- data %>% 
  separate(coordinates, c("lon", "lat"), sep = ' ') %>% 
  mutate(lon = as.double(lon), lat = as.double(lat))

#data_spatial <- vect(data, geom = c("lat", "lon"), crs = "EPSG:4326")
data <- st_as_sf(data, coords = c("lat", "lon"), crs = 4326)

# tweets and US shapefile intersection -- takes long to compute!
sf_use_s2(FALSE)
# drop Alaska and Hawaii
#usa_states <- usa_states[!(usa_states$State_Code == "AK"),]
#usa_states <- usa_states[!(usa_states$State_Code == "HI"),]
#usa_tweets <- terra::intersect(data_spatial, usa_states)
usa_states <- usa_states[-c(2,12),]
data <- st_intersection(data, usa_states)

######
## merge twitter data with pollution
######
parse_nums <- function(df) {
  res <- df %>% mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, as.character)) %>% 
    mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, parse_number))
  res
}

#ecmwf <- terra::rast("../data/ECMWF/july2015_new.nc", drivers = "NETCDF")
#pm25 <- subset(ecmwf, c(1273:1296))

#ext <- terra::extract(pm25, data_spatial)
#data_spatial$pm25 <- ext

ecmwf <- read_stars("../data/ECMWF/july2015_new.nc")
u10 <- st_as_sf(ecmwf[1]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
v10 <- st_as_sf(ecmwf[2]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
sea_pressure <- st_as_sf(ecmwf[3]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
pm10 <- st_as_sf(ecmwf[4]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
pm01 <- st_as_sf(ecmwf[5]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
pm25 <- st_as_sf(ecmwf[6]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`) %>% 
  st_set_crs(st_crs(usa_states))
surface_pressure <- st_as_sf(ecmwf[7]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
aod550 <- st_as_sf(ecmwf[8]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`) %>% 
  st_set_crs(st_crs(usa_states))
co <- st_as_sf(ecmwf[9]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
c2h6 <- st_as_sf(ecmwf[10]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
hcho <- st_as_sf(ecmwf[11]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
h2o2 <- st_as_sf(ecmwf[12]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
oh <- st_as_sf(ecmwf[13]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
c5h8 <- st_as_sf(ecmwf[14]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
ch4 <- st_as_sf(ecmwf[15]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
hno3 <- st_as_sf(ecmwf[16]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
no2 <- st_as_sf(ecmwf[17]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
no <- st_as_sf(ecmwf[18]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
co3 <- st_as_sf(ecmwf[19]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
pan <- st_as_sf(ecmwf[20]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
c3h8 <- st_as_sf(ecmwf[21]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`)
so2 <- st_as_sf(ecmwf[22]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`) %>% 
  st_set_crs(st_crs(usa_states))

# pm25july <- pm25 %>%
  # parse numeric values from character vectors
#  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, as.character)) %>% 
#  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, parse_number)) %>% 
  # change units to ppm from kg/m^3
  # 1 Kilogram Per Cubic Meter = 1 000 000 000 ug per cubic meter
#  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>% 
#  st_set_crs(st_crs(usa_states))

# rename first column to also include time
rename_midnight <- function(df) {
  names(df)[1] <- "2015-07-05 00:00:00"
  names(df)[9] <- "2015-07-06 00:00:00"
  names(df)[17] <- "2015-07-07 00:00:00"
  
  df
}

aod550 <- rename_midnight(aod550)
ch4 <- rename_midnight(ch4)
co <- rename_midnight(co)
hcho <- rename_midnight(hcho)
no2 <- rename_midnight(no2)
pm01 <- rename_midnight(pm01)
pm10 <- rename_midnight(pm10)
pm25 <- rename_midnight(pm25)
so2 <- rename_midnight(so2)
co3 <- rename_midnight(co3)

pm25 <- st_intersection(pm25, usa_states)
aod550 <- st_intersection(aod550, usa_states)
so2 <- st_intersection(so2, usa_states)


# create long tables before joining
create_long <- function(df, col_name) {
  df <- df %>% 
    pivot_longer(!geometry, names_to = "timestamp", values_to = col_name) %>% 
    mutate(timestamp = as.POSIXct(timestamp, format = "X%Y.%m.%d.%H.%M.%S", tz = "UTC"))
  
  df
}

aod550 <- create_long(aod550[,1:24], "aod550")
ch4 <- create_long(ch4, "ch4")
co <- create_long(co, "co")
hcho <- create_long(hcho, "hcho")
no2 <- create_long(no2, "no2")
pm01 <- create_long(pm01, "pm01")
pm10 <- create_long(pm10, "pm10")
pm25 <- create_long(pm25[,1:24], "pm25")
so2 <- create_long(so2[,1:24], "so2")
co3 <- create_long(co3, "co3")

# join pollution tables
#TODO

# interpolate pm values to one hour time intervals
no_cores <- detectCores() - 2

process_row <- function(row) {
  subset_df <- pm25[row,] %>% st_drop_geometry() %>% as.double()
  
  timestamps <- seq(as.POSIXct("2015-07-05 00:00:00"), as.POSIXct("2015-07-07 21:00:00"), by = "3 hours")
  finer_timestamps <- seq(as.POSIXct("2015-07-05 00:00:00"), as.POSIXct("2015-07-07 21:00:00"), by = "hour")
  ts_data <- zoo(subset_df, order.by = timestamps)
  interpolated <- na.approx(ts_data, xout = finer_timestamps)
  
  df <- data.frame(timestamp = index(interpolated), pm25 = coredata(interpolated))
  df$geometry <- pm25[row,]$geometry
  df <- df %>% st_as_sf()
  
  df
}

result_list <- mclapply(seq(1, nrow(pm25)), process_row, mc.cores = no_cores)

pm25_interpolated <- bind_rows(result_list)

breaks <- seq(from = as.POSIXct("2015-07-05 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 
              to = as.POSIXct("2015-07-07 21:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), 
              by = "hour",
              labels = labels)
labels <- format(breaks, format = "%Y-%m-%d %H:%M:%S")

# time bins from timestamps
data$time_bin <- cut(data$timestamp, breaks, labels = FALSE)
data$time_bin <- breaks[data$time_bin]

#### MERGE DATASETS
full_data <- st_join(data, pm25_interpolated) %>% 
  filter(time_bin == timestamp.y) %>% 
  mutate(timestamp = timestamp.x) %>% 
  select(id, username, timestamp, time_bin, State_Code, text, vader_compound, pm25) %>% 
  mutate(timestamp = format(timestamp, format = "%Y-%m-%d %H:%M:%S"))

filtered <- full_data %>% 
  filter(vader_compound != 0)

write_sf(full_data, "../data/cleaned/US_tweets_05-07_07_2015.shp", driver = "ESRI Shapefile")

summary(lm(vader_compound ~ pm25 + timestamp, data = filtered))

ggplot(full_data, aes(x = timestamp, y = vader_compound, fill = pm25)) +
  geom_bar(stat = "summary", fun = "mean", position = position_dodge(width = 0.8), width = 0.7) +
  theme_minimal() +
  labs(title = "Difference-in-Differences Plot",
       x = "Time Period",
       y = "Outcome",
       fill = "Group") +
  geom_errorbar(aes(ymin = ..y.. - sd(outcome), ymax = ..y.. + sd(outcome)),
                stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.8), width = 0.25)
