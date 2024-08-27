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

# standardize data
scaled <- data %>% dplyr::select(date, NAME, STATE_NAME, 
                                 CNTY_UNIQUE, day, day_hour, username, main_label, 
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


### MODELS
#save(full_data, data, all_bots, scaled, file="../data/models.RData")
load("../data/models.RData")

#### GROUPED BY COUNTY --- aggregation function is median, use mean for sensitivity check?
grouped <- data %>%  
  group_by(CNTY_UNIQUE, date) %>% 
  dplyr::summarize(num_tweets=n(), pm25=median(pm25), temp_max=max(temperature), 
                   temp_min=min(temperature), temp_med=median(temperature),
            wind_x=median(wind_x), wind_y=median(wind_y), dewpoint=median(dewpoint), 
            aod550=median(aod550), precip=median(precipitation), clouds=median(clouds),
            visibility=median(visibility),
            vader_compound=median(vader_compound),
            roberta_positive=median(roberta_positive),
            roberta_negative=median(roberta_negative),
            bertweet_positive=median(bertweet_positive),
            bertweet_negative=median(bertweet_negative)) %>% 
  mutate(pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5), Inf), include.lowest = TRUE)) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (. -mean(.)) / sd(.))) ### counties with only one tweet will result in NA's due to zero variance

grouped$day <- as.POSIXlt(grouped$date)$wday

## baseline, weight by number of tweets / population
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility | CNTY_UNIQUE + date + day,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()


# nonlinear 
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat + temp_med + I(temp_med^2) + precip + visibility | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) |>
  etable()

#### INDIVIDUAL LEVEL
## County + date + username, cluster by county and date
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | CNTY_UNIQUE + date + day + username,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

## County + date + username + topic, cluster by county and date
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | CNTY_UNIQUE + date + day + username + main_label,
  data = scaled,
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# bots -- placebo test, exclude posts related to weather (category news and social concern)
grouped_bots <- all_bots %>% group_by(CNTY_UNIQUE, date) %>% 
  summarize(num_tweets=n(), pm25=median(pm25), temp_med=median(temperature), 
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

grouped_bots$day <- as.POSIXlt(grouped_bots$date)$wday

feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility | CNTY_UNIQUE + date + day,
  data = grouped_bots,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped_bots$num_tweets
) |>
  etable()

# remove wildfire counties
data_without_california <- data %>% filter(STATE_NAME != "California") %>% filter(STATE_NAME != "Washington") %>% 
  group_by(CNTY_UNIQUE, date) %>% 
  dplyr::summarize(num_tweets=n(), pm25=median(pm25), temp_med=median(temperature), 
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
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 + temp_med + I(temp_med^2) + precip + visibility | CNTY_UNIQUE + date + day,
  data = data_without_california,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = data_without_california$num_tweets
) |>
  etable()

# individual categories
# celebrities
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | CNTY_UNIQUE + date,
  data = data %>% filter(celebrity_._pop_culture > 0.5),
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# food
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~  pm25 + temperature + I(temperature^2) + precipitation + visibility | CNTY_UNIQUE + date,
  data = data %>% filter(food_._dining > 0.5),
  cluster = c("CNTY_UNIQUE", "date")
) |>
  etable()

# news and social concern
feols(
  c(vader_compound, 
    roberta_positive, roberta_negative, 
    bertweet_positive, bertweet_negative) ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | CNTY_UNIQUE + date,
  data = data %>% filter(news_._social_concern > 0.5),
  cluster = c("CNTY_UNIQUE", "date")
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


### sizes in context
# 2015 FIFA Women's World Cup
soccer <- scaled %>% 
  filter(date >= "2015-07-03") %>% 
  filter(date <= "2015-07-11") %>% 
  filter(main_label == "'sports'") %>% 
  group_by(date) %>% 
  summarize(vader = mean(vader_compound),
            roberta_pos = mean(roberta_positive),
            bertweet_pos = mean(bertweet_positive),
            roberta_neg = mean(roberta_negative),
            bertweet_neg = mean(bertweet_negative)) %>% 
  mutate(Positive = (roberta_pos + bertweet_pos) / 2,
         Negative = (roberta_neg + bertweet_neg) / 2) %>% 
  dplyr::select(date, Positive, Negative) %>% 
  melt(id.vars = 'date') 

soccer %>% 
  ggplot(aes(x=date, y=value, col=variable, group=variable)) + 
  geom_line() +
  theme_minimal() +
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=16, angle = 45),
        axis.text.y = element_text(size=16),
        legend.position = c(0.10, 0.85)) +
  geom_line(linewidth=1) +
  geom_point(size=3) +
  scale_color_manual(name='Sentiment polarity',
                     breaks=c('Positive', 'Negative'),
                     values=c('Negative' = 'darkred', 'Positive'='chartreuse4')) +
  geom_vline(xintercept = as.Date("2015-07-05"), linetype='dashed') +
  xlab(NULL) +
  scale_x_date(breaks = seq(as.Date("2015-07-01"), as.Date("2015-07-18"), by = "1 day")) +
  ylab("Mean standardized positive sentiment") 
  

# Kansas tornado
tornado <- scaled %>% 
  filter(date >= "2015-07-10") %>% 
  filter(date <= "2015-07-20") %>% 
  filter(str_detect(CNTY_UNIQUE, 'Kansas')) %>% 
  group_by(date) %>% 
  summarize(vader = mean(vader_compound),
            roberta_pos = mean(roberta_positive),
            bertweet_pos = mean(bertweet_positive),
            roberta_neg = mean(roberta_negative),
            bertweet_neg = mean(bertweet_negative)) %>% 
  mutate(Positive = (roberta_pos + bertweet_pos) / 2,
         Negative = (roberta_neg + bertweet_neg) / 2) %>% 
  dplyr::select(date, Positive, Negative) %>% 
  melt(id.vars = 'date')


tornado %>% 
  ggplot(aes(x=date, y=value, col=variable, group=variable)) + 
  geom_line() +
  theme_minimal() +
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=15, angle = 45),
        axis.text.y = element_text(size=16),
        legend.position = c(0.15, 0.85)) +
  geom_line(linewidth=1) +
  geom_point(size=3) +
  scale_color_manual(name='Sentiment polarity',
                     breaks=c('Positive', 'Negative'),
                     values=c('Negative' = 'darkred', 'Positive'='chartreuse4')) +
  geom_vline(xintercept = as.Date("2015-07-13"), linetype='dashed') +
  xlab(NULL) +
  scale_x_date(breaks = seq(as.Date("2015-07-01"), as.Date("2015-07-31"), by = "1 day")) +
  ylab("Mean standardized positive sentiment") 

##### 2SLS
library(ivreg)
summary(ivreg(vader_compound ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | temperature + I(temperature^2) + precipitation + visibility + wind_x + wind_y, 
      data = data))

summary(ivreg(roberta_positive ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | temperature + I(temperature^2) + precipitation + visibility + wind_x + wind_y, 
              data = data))

summary(ivreg(roberta_negative ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | temperature + I(temperature^2) + precipitation + visibility + wind_x + wind_y, 
              data = data))

summary(ivreg(bertweet_positive ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | temperature + I(temperature^2) + precipitation + visibility + wind_x + wind_y, 
              data = data))

summary(ivreg(bertweet_negative ~ pm25 + temperature + I(temperature^2) + precipitation + visibility | temperature + I(temperature^2) + precipitation + visibility + wind_x + wind_y, 
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
