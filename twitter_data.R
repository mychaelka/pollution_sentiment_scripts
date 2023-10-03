library(tidyverse)

data <- read_delim('./2015-07-01.csv', delim = '|')
geocoded <- data[data$geo_available == 1,]  # 13% of tweets have gps coordinates

