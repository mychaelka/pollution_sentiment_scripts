library(tidyverse)
library(fixest)
library(lutz)
library(sf)
library(readr)
library(lubridate)
library(data.table)

load("../data/full_data_counties.RData")

# create local timestamps (the ones from data are in UTC)
full_data$timezone <- tz_lookup(full_data, method = "accurate")
full_data$local_time <- with_tz(full_data$timestamp, tzone = full_data$timezone)
full_data$local_timebin <- with_tz(full_data$time_bin, tzone = full_data$timezone)
full_data$day <- as.POSIXlt(full_data$local_time)$wday
full_data$date <- as.Date(full_data$local_time)

# extract longitute/latitude from geometry
full_data <- full_data %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

full_data <- full_data |> dplyr::select(!(text)) %>% st_drop_geometry()

# cut pm25 into bins
full_data <- full_data %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 150, by = 30),Inf), include.lowest = TRUE)
  )

# standardize data - for nonparametric regression, do not standardize pm2.5!
scaled <- data %>% dplyr::select(date, NAME, day, day_hour, username, pm25_cat, pm25,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.))) %>% 
  drop_na() 

# remove bots from data
bot_usernames <- read_csv("../data/bot_usernames.csv")
day_bot_usernames <- read_csv("../data/day_bot_usernames.csv")
bots <- full_data %>% filter(username %in% bot_usernames$username)
day_bots <- full_data %>% filter(username %in% day_bot_usernames$username)
all_bots <- rbindlist(list(bots, day_bots))
data <- full_data %>% filter(!(username %in% all_bots$username))

# day of week indicator
data$day <- lubridate::wday(data$local_time)
data$day_hour <- lubridate::hour(data$local_time)

# night data -- too few tweets during the night
data$night <- if_else(lubridate::hour(data$local_time) >= 22 | 
                             lubridate::hour(data$local_time) <= 6, 1, 0)
night <- data %>% filter(lubridate::hour(local_time) >= 22 | 
                                lubridate::hour(local_time) <= 6)

### CORRELATION BETWEEN NUMBER OF TWEETS AND PM25
grouped <- data %>% st_drop_geometry() %>% 
  group_by(local_timebin, STATE_NAME) %>% 
  summarize(count = n(), pm25 = mean(pm25, na.rm = TRUE))

cor(grouped$count, grouped$pm25, use = "complete.obs")


### CORRELATION BETWEEN SENTIMENT MEASURES - do this on data with bots 
cor(full_data[,c("vader_compound", "roberta_positive", 
                 "roberta_negative", "bertweet_positive", "bertweet_negative")])

# bertweet binarizes the data much more than roberta 
full_data[sample(nrow(full_data), 100000), ] %>% ggplot(aes(x=roberta_positive, y=bertweet_positive)) +
  geom_point()

full_data[sample(nrow(full_data), 500000), ] %>% ggplot(aes(x=roberta_negative, y=bertweet_negative)) +
  geom_point()

# cut roberta and bertweet measures into categories
full_data <- full_data %>% 
  mutate(
    roberta_positive_cat = cut(roberta_positive, breaks = c(seq(from = 0, to = 1, by = 0.1)), include.lowest = TRUE),
    bertweet_positive_cat = cut(bertweet_positive, breaks = c(seq(from = 0, to = 1, by = 0.1)), include.lowest = TRUE),
    roberta_negative_cat = cut(roberta_negative, breaks = c(seq(from = 0, to = 1, by = 0.1)), include.lowest = TRUE),
    bertweet_negative_cat = cut(bertweet_negative, breaks = c(seq(from = 0, to = 1, by = 0.1)), include.lowest = TRUE)
  )

counts_positive <- full_data %>% st_drop_geometry() %>% 
  group_by(roberta_positive_cat, bertweet_positive_cat) %>% 
  summarise(count = n())

counts_negative <- full_data %>% st_drop_geometry() %>% 
  group_by(roberta_negative_cat, bertweet_negative_cat) %>% 
  summarise(count = n())

ggplot(counts_positive, aes(roberta_positive_cat, bertweet_positive_cat, fill = count)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Roberta positive", y = "Bertweet positive", fill = "Value")

ggplot(counts_negative, aes(roberta_negative_cat, bertweet_negative_cat, fill = count)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Roberta negative", y = "Bertweet negative", fill = "Value")


## Create binary sentiment measures from roberta and bertweet


### MODELS
#save(full_data, data, all_bots, night, file="../data/models.RData")
load("../data/models.RData")

## County + date + username, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data,
  cluster = "NAME"
) |>
  etable()

# All possible FE, cluster by state
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | STATE_NAME + NAME + date + day + day_hour + username,
  data = full_data,
  cluster = "STATE_NAME"
) |>
  etable()

# All possible FE, cluster by username
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | STATE_NAME + NAME + date + day + day_hour + username,
  data = full_data,
  cluster = "username"
) |>
  etable()

# All possible FE, Conley SE 
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | STATE_NAME + NAME + date + day + day_hour + username,
  data = full_data,
  vcov = conley(100)
) |>
  etable()

## County + date + hour + username, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour + username,
  data = full_data,
  cluster = "NAME"
) |>
  etable()

## County + date + hour + username + topic, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour + username + main_label,
  data = full_data,
  cluster = "NAME"
) |>
  etable()

# sentiment during night -- robustness check to filter out the visibility problem
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date,
  data = night,
  cluster = "NAME"
) |>
  etable()

# bots -- placebo test
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = all_bots,
  cluster = "NAME"
) |>
  etable()

# nonlinearity -- pm25 categories
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat | NAME + date + day_hour + username,
  data = scaled,
  cluster = "NAME"
) |>
  etable()

# coarser categories
full_data <- full_data %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 150, by = 50),Inf), include.lowest = TRUE)
  )

feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat | NAME + date + day_hour + username,
  data = full_data,
  cluster = "NAME"
) |>
  etable()

# interaction with category
# assign each tweet only one topic
full_data$main_label <- lapply(full_data$label, function(x) {
  aux <- gsub('\\[', '', x) %>% 
    gsub('\\]', '', .) %>% 
    strsplit(., ",")
  ifelse(is.na(aux[[1]][1]), "'no_topic'", aux[[1]][1])
}) %>% unlist()

feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 * main_label | NAME + date + day_hour + username,
  data = full_data
) |>
  etable()


# individual categories
# sports
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(sports > 0.5),
  cluster = "NAME"
) |>
  etable()

# celebrities
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(celebrity_._pop_culture > 0.5),
  cluster = "NAME"
) |>
  etable()

# arts and culture
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(arts_._culture > 0.5),
  cluster = "NAME"
) |>
  etable()

# food
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(food_._dining > 0.5),
  cluster = "NAME"
) |>
  etable()

# news and social concern
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(news_._social_concern > 0.5),
  cluster = "NAME"
) |>
  etable()

# travel
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(travel_._adventure > 0.5),
  cluster = "NAME"
) |>
  etable()

# diaries and daily life
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(diaries_._daily_life > 0.5),
  cluster = "NAME"
) |>
  etable()

# family
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(family > 0.5),
  cluster = "NAME"
) |>
  etable()

# relationships
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour,
  data = full_data %>% filter(relationships > 0.5),
  cluster = "NAME"
) |>
  etable()


## DEVIATION FROM MEAN POLLUTION
countymeans <- humans %>% 
  st_drop_geometry() %>% 
  group_by(NAME) %>% 
  summarise(county_pollution_mean = mean(pm25, na.rm = TRUE),
            vader_mean = mean(vader_compound, na.rm = TRUE),
            roberta_positive_mean = mean(roberta_positive, na.rm = TRUE),
            roberta_negative_mean = mean(roberta_negative, na.rm = TRUE)) %>% 
  ungroup()

deviations <- humans %>% 
  dplyr::select(vader_compound, roberta_positive, roberta_negative, 
                bertweet_positive, bertweet_negative,
                pm25, pm25_cat, NAME, STATE_NAME, local_timebin) %>% 
  left_join(countymeans, by = "NAME") %>% 
  mutate(pm25_deviation = pm25 - county_pollution_mean,
         vader_deviation = vader_compound - vader_mean,
         roberta_positive_deviation = roberta_positive - roberta_positive_mean,
         roberta_negative_deviation = roberta_negative - roberta_negative_mean)

feols(
  c(vader_compound, 
    roberta_positive, roberta_negative,
    bertweet_positive, bertweet_negative) ~ pm25_deviation | local_timebin + STATE_NAME,
  data = deviations
) |>
  etable()



##### PLOTS
# correlation between pm25 and aod550
full_data %>% ggplot(aes(x = pm25, y = aod550)) +
  geom_point() + 
  theme_bw()


full_data %>% 
  st_drop_geometry() %>% 
  group_by(local_timebin) %>% 
  summarize(count = n(), pm25 = mean(pm25)) %>% 
  ggplot() +
  geom_line(aes(x = local_timebin, y = count)) +
  geom_line(aes(x = local_timebin, y = pm25*1000), col = "blue") +
  theme_bw(base_family = "Times") 

sports %>% 
  st_drop_geometry() %>% 
  group_by(local_timebin) %>% 
  summarize(count = n(), pm25 = max(pm25), sentiment = mean(vader_compound)) %>% 
  ggplot() +
  geom_line(aes(x = local_timebin, y = count)) +
  geom_line(aes(x = local_timebin, y = pm25*100), col = "blue") +
  geom_line(aes(x = local_timebin, y = sentiment*10000), col = "red") +
  theme_bw(base_family = "Times") 

celebrities %>% 
  st_drop_geometry() %>% 
  group_by(local_timebin) %>% 
  summarize(count = n(), pm25 = mean(pm25)) %>% 
  ggplot() +
  geom_line(aes(x = local_timebin, y = count)) +
  geom_line(aes(x = local_timebin, y = pm25*100), col = "blue") +
  theme_bw(base_family = "Times") 

# fixed effects plots
# all categories except for sports
a <- feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + day_hour + username,
  data = full_data
) 

coefplot(a)

b <- without_sport %>% 
  group_by(pm25_cat) %>% 
  summarise(sentiment=mean(vader_compound),
            n_obs = n())

ggplot(full_data %>% 
         group_by(pm25_cat) %>% 
         summarise(sentiment=mean(vader_compound),
                   n_obs = n()), 
       mapping = aes(x = pm25_cat, y = sentiment)) +
  geom_point() +
  labs(x = 'pm2.5', y = 'sentiment') +
  theme_minimal()



## SPLIT BY LABEL
qq <- 
  humans %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  dplyr::select(id,arts_._culture:youth_._student_life) %>% 
  pivot_longer(-id) %>% 
  group_by(id) %>% 
  slice_max(
    value, n=1L
  ) %>% 
  slice(1L) %>% 
  ungroup() %>% 
  dplyr::select(id, labelmax = name)

humans_qq <- left_join(humans, qq, by = 'id')


## TREATMENT FOR DID
humans_did <- humans %>% 
  group_by(lon, lat) %>% 
  mutate(treat = ifelse(max(pm25) > 100, 1, 0)) %>% 
  ungroup() %>% 
  group_by(lon, lat, local_timebin) %>% 
  mutate(time = ifelse(max(pm25) > 100, 1, 0)) %>% 
  ungroup()

without_sport_did <- humans_did %>% 
  group_by(username, timestamp) %>% filter(sports < 0.5) %>% ungroup()
