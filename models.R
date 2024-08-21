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

full_data <- full_data |> dplyr::select(!(text)) |> st_drop_geometry() |> drop_na()

# cut pm25 into bins
full_data <- full_data %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)
  )

# assign each tweet only one topic
full_data$main_label <- lapply(full_data$label, function(x) {
  aux <- gsub('\\[', '', x) %>% 
    gsub('\\]', '', .) %>% 
    strsplit(., ",")
  ifelse(is.na(aux[[1]][1]), "'no_topic'", aux[[1]][1])
}) %>% unlist()

# hour of day indicator
full_data$day_hour <- lubridate::hour(full_data$local_time)

# county unique indicator
full_data <- full_data %>% mutate(CNTY_UNIQUE=paste(full_data$NAME, full_data$STATE_NAME))

# remove bots from data
bot_usernames <- read_csv("../data/bot_usernames.csv")
day_bot_usernames <- read_csv("../data/day_bot_usernames.csv")
bots <- full_data %>% filter(username %in% bot_usernames$username)
day_bots <- full_data %>% filter(username %in% day_bot_usernames$username)

# join bots data and standardize 
all_bots <- rbindlist(list(bots, day_bots)) %>% 
  dplyr::select(date, CNTY_UNIQUE, day, day_hour, username, main_label, lon, lat,
                pm25_cat, pm25, aod550, wind_x, wind_y, temperature, dewpoint, clouds, 
                precipitation, visibility,
                vader_compound, roberta_positive,
                roberta_negative, bertweet_positive,
                bertweet_negative) %>%
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

data <- full_data %>% filter(!(username %in% all_bots$username))

rm(bot_usernames, day_bot_usernames, bots, day_bots)

# night data
night <- data %>% filter(lubridate::hour(local_time) >= 22 | 
                                lubridate::hour(local_time) <= 6) %>% 
  dplyr::select(date, CNTY_UNIQUE, day, day_hour, username, main_label, 
                pm25_cat, pm25, aod550, wind_x, wind_y, temperature, dewpoint, clouds, 
                precipitation, visibility,
                vader_compound, roberta_positive,
                roberta_negative, bertweet_positive,
                bertweet_negative) %>%
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

# standardize data
scaled <- data %>% dplyr::select(date, CNTY_UNIQUE, day, day_hour, username, main_label, 
                                 lon, lat, pm25_cat, pm25, aod550, wind_x, wind_y, 
                                 temperature, dewpoint, clouds, precipitation, visibility,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

### CORRELATION BETWEEN NUMBER OF TWEETS AND PM25
grouped <- data %>% st_drop_geometry() %>% 
  group_by(local_timebin, STATE_NAME) %>% 
  summarize(count = n(), pm25 = mean(pm25, na.rm = TRUE))

cor(grouped$count, grouped$pm25, use = "complete.obs")


### CORRELATION BETWEEN SENTIMENT MEASURES
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
#save(full_data, data, all_bots, night, scaled, file="../data/models.RData")
load("../data/models.RData")

#### GROUPED BY COUNTY
grouped <- data %>% group_by(CNTY_UNIQUE, date) %>% 
  summarize(num_tweets=n(), pm25=median(pm25), temp=median(temperature), 
            wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
            aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
            visibility=median(visibility),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

grouped$day <- as.POSIXlt(grouped$date)$wday


### lagged pm??
#library(plm)
#grouped_plm <- pdata.frame(grouped, index=c("CNTY_UNIQUE", "date"))
#grouped_plm <- transform(grouped_plm, lagged_pm=lag(pm25, 1))
#grouped_plm$day <- as.POSIXlt(grouped_plm$date)$wday

## baseline, weight by number of tweets / population
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp + precip + visibility | CNTY_UNIQUE + date + day,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()


# nonlinear 
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat + temp + precip | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

#### INDIVIDUAL LEVEL
## County + date, cluster by county and date
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + precipitation + visibility | CNTY_UNIQUE + date,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()


## County + date + username, cluster by county and date
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + precipitation | CNTY_UNIQUE + date + username,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# County + date + username + hour, cluster by county and date
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature | CNTY_UNIQUE + date + day_hour + username,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# County + date + username, Conley SE 
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature | CNTY_UNIQUE + date + username,
  data = scaled,
  vcov = conley(100)
) |>
  etable()

## County + date + hour + username + topic, cluster by county and date
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature | CNTY_UNIQUE + date + day_hour + username + main_label,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()


##### ROBUSTNESS CHECKS
# sentiment during night -- robustness check to filter out the visibility problem
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature | CNTY_UNIQUE + date,
  data = night,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# night in grouped data - does not work
grouped_night <- data %>% filter(lubridate::hour(local_time) >= 22 | 
                             lubridate::hour(local_time) <= 6) %>% 
  group_by(CNTY_UNIQUE, date) %>% 
  summarize(num_tweets=n(), pm25=median(pm25), temp=median(temperature),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

grouped_night$day <- as.POSIXlt(grouped_night$date)$wday

feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp | CNTY_UNIQUE + date + day,
  data = grouped_night,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped_night$num_tweets
) |>
  etable()


#### VISIBILITY - residuals from regression of aod550 on pm25?
aod_model <- lm(aod550 ~ pm25, data = grouped)
aod_res <- resid(aod_model)
grouped <- grouped %>% drop_na()
grouped$resid <- aod_res

feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp + resid | CNTY_UNIQUE + date + day,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()

########


# bots -- placebo test
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + precipitation | CNTY_UNIQUE + date,
  data = all_bots,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | CNTY_UNIQUE + date + username + main_label,
  data = all_bots,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# nonlinearity -- pm25 categories
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat | CNTY_UNIQUE + date,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# interaction with category
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat * main_label | CNTY_UNIQUE + date + username,
  data = scaled
) |>
  etable()


# remove wildfire counties
data_without_california <- data %>% filter(STATE_NAME != "California") %>% 
  group_by(CNTY_UNIQUE, date) %>% 
  summarize(num_tweets=n(), pm25=median(pm25), temp=median(temperature), 
            wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
            aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
            visibility=median(visibility),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

data_without_california$day <- as.POSIXlt(data_without_california$date)$wday

## baseline, weight by number of tweets / population
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp + precip + visibility | CNTY_UNIQUE + date + day,
  data = data_without_california,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = data_without_california$num_tweets
) |>
  etable()

# individual categories
# business and entrepreneurs
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(business_._entrepreneurs > 0.5),
  cluster = "NAME"
) |>
  etable()

# learning and educational
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(learning_._educational > 0.5),
  cluster = "NAME"
) |>
  etable()

# fitness and health
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(fitness_._health > 0.5),
  cluster = "NAME"
) |>
  etable()

# sports
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(sports > 0.5),
  cluster = "NAME"
) |>
  etable()

# celebrities
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(celebrity_._pop_culture > 0.5),
  cluster = "NAME"
) |>
  etable()

# arts and culture
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(arts_._culture > 0.5),
  cluster = "NAME"
) |>
  etable()

# food
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(food_._dining > 0.5),
  cluster = "NAME"
) |>
  etable()

# news and social concern
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(news_._social_concern > 0.5),
  cluster = "NAME"
) |>
  etable()

# travel
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(travel_._adventure > 0.5),
  cluster = "NAME"
) |>
  etable()

# diaries and daily life
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(diaries_._daily_life > 0.5),
  cluster = "NAME"
) |>
  etable()

# family
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(family > 0.5),
  cluster = "NAME"
) |>
  etable()

# relationships
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = data %>% filter(relationships > 0.5),
  cluster = "NAME"
) |>
  etable()

##### SO2 AND WIND DIRECTION
## County + date, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ so2 | NAME + date,
  data = scaled,
  cluster = c("NAME")
) |>
  etable()

## County + date + username, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ so2 | NAME + date + username,
  data = scaled,
  cluster = c("NAME")
) |>
  etable()

##### 2SLS
library(ivreg)
summary(ivreg(vader_compound ~ pm25 + aod550 | wind + aod550, 
      data = data))

summary(ivreg(roberta_positive ~ temperature | pm25 | wind, 
              data = data))

summary(ivreg(roberta_negative ~ temperature | pm25 | wind, 
              data = data))

summary(ivreg(bertweet_positive ~ temperature | pm25 | wind, 
              data = data))

summary(ivreg(bertweet_negative ~ temperature | pm25 | wind, 
              data = data))




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
a <- feols(vader_compound ~ pm25_cat | NAME + date + day_hour + username,
  data = scaled
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
data_did <- data %>% 
  group_by(lon, lat) %>% 
  mutate(treat = ifelse(max(pm25) > 100, 1, 0)) %>% 
  ungroup() %>% 
  group_by(lon, lat, local_timebin) %>% 
  mutate(time = ifelse(max(pm25) > 100, 1, 0)) %>% 
  ungroup()

summary(lm(roberta_positive ~ treat*time, data=data_did))
