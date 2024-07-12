library(tidyverse)
library(lutz)
library(sf)
library(raster)
library(viridis)
library(reshape2)
library(fixest)
library(gridExtra)

load("../data/models.RData")

# sample characteristics
summary(full_data$pm25)
summary(full_data$vader_compound)
summary(full_data$roberta_positive)
summary(full_data$roberta_negative)
summary(full_data$bertweet_positive)
summary(full_data$bertweet_negative)

# tweets per person
tweets_user <- full_data %>% 
  group_by(username) %>% 
  summarize(count = n())
summary(tweets_user$count)

# tweets per county
tweets_county <- full_data %>% 
  group_by(NAME) %>% 
  summarize(count = n())
summary(tweets_county$count)

# how many tweets per each pollution bin? 
data %>%
  ggplot(aes(x=pm25_cat)) + 
  geom_histogram(stat="count") +
  theme_bw(base_family = "Times")

# relationship between #tweets and pollution
data %>% 
  mutate(pm25_int = floor(pm25)) %>% 
  group_by(pm25_int) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=pm25_int, y=count)) +
  geom_line()

# how many users per each pollution bin?
data %>%
  group_by(username, pm25_cat) %>% 
  summarize(user_count = n()) %>% 
  ggplot(aes(x=pm25_cat)) + 
  geom_histogram(stat="count") +
  theme_bw(base_family = "Times")

# sentiment and day of week (1 = Sunday)
# cut pm2.5 into finer bins
data <- data %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 100, by = 10),Inf), include.lowest = TRUE)
  )


# standardize 
scaled <- data %>% dplyr::select(date, NAME, day, day_hour, username, pm25_cat, pm25,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))


##### REGRESSION TEST
## County + date + username, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = scaled,
  cluster = "NAME"
) |>
  etable()
#####

# positive 
pos <- scaled %>%
       group_by(day) %>% 
       summarize(vader = mean(vader_compound),
                 roberta_pos = mean(roberta_positive),
                 bertweet_pos = mean(bertweet_positive)) %>%
       melt(id.vars = "day") %>% 
       mutate(lag=lag(value)) %>% 
       mutate(diff=value-lag)

pos_plot <- scaled %>%
  group_by(day) %>% 
  summarize(vader = mean(vader_compound),
            roberta_pos = mean(roberta_positive),
            bertweet_pos = mean(bertweet_positive)) %>%
  melt(id.vars = "day") %>% 
  ggplot(aes(x=day, y=value, col=variable)) + 
  geom_line(linewidth=1) +
  geom_point(linewidth=2) +
  geom_point() +
  theme_bw(base_family = "Times")

# negative
neg <- scaled %>%
  group_by(day) %>% 
  summarize(roberta_neg = mean(roberta_negative),
            bertweet_neg = mean(bertweet_negative)) %>%
  melt(id.vars = "day") %>% 
  mutate(lag=lag(value)) %>% 
  mutate(diff=value-lag)

neg_plot <- neg %>% 
  ggplot(aes(x=day, y=value, col=variable)) + 
  geom_line(linewidth=1) +
  geom_point(linewidth=2) +
  geom_point() +
  theme_bw(base_family = "Times")

grid.arrange(pos_plot, neg_plot, nrow = 1)


# cut pm25 into finer bins
scaled <- scaled %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 75, by = 5),Inf), include.lowest = TRUE)
  )

a <- feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat | NAME + date + day_hour + username,
  data = scaled,
  cluster = c("NAME", "date")
)


# regression coefficients and CI per pollution bin 
# TODO: include coefficients for alternative specifications of FE
rename_intervals <- function(df) {
  df$name <- gsub("pm25_cat", "", df$name)
  df <- df %>% 
    mutate(start = as.numeric(gsub("[^0-9,-]", "", sub(",.*", "", name))),
           end = as.numeric(gsub("[^0-9,-]", "", sub(".*,", "", name)))) %>%
    arrange(start, end) %>%
    mutate(intervals = factor(name, levels = unique(name), ordered = TRUE)) %>% 
    dplyr::select(-start, -end)
  
  return(df)
}

# vader
vader_coefs <- coef(a$`lhs: vader_compound`) %>% enframe()
vader_confint <- confint(a$`lhs: vader_compound`) %>% rownames_to_column(., "name")

vader_coefs <- vader_coefs %>% 
  rename_intervals() %>% 
  inner_join(vader_confint %>% rename_intervals(), by = c("name", "intervals"))

line_plot <- vader_coefs %>% ggplot(aes(x=intervals, y=value, group=1)) +
  geom_line() +
  geom_line(aes(x=intervals, y=`2.5 %`), alpha=0.5)+
  geom_line(aes(x=intervals, y=`97.5 %`), alpha=0.5) +
  theme_bw(base_family = "Times")

hist_plot <- scaled %>% 
  ggplot(aes(x=pm25_cat)) + 
  geom_bar() +
  theme_bw(base_family = "Times")

grid.arrange(line_plot, hist_plot, ncol = 1)

# roberta positive
roberta_pos_coefs <- coef(a$`lhs: roberta_positive`) %>% enframe()
roberta_pos_confint <- confint(a$`lhs: roberta_positive`) %>% rownames_to_column(., "name")

roberta_pos_coefs <- roberta_pos_coefs %>% 
  rename_intervals() %>% 
  inner_join(roberta_pos_confint %>% rename_intervals(), by = c("name", "intervals"))

line_plot <- 
  roberta_pos_coefs %>% ggplot(aes(x=intervals, y=value, group=1)) +
  geom_line() +
  geom_line(aes(x=intervals, y=`2.5 %`), alpha=0.5) +
  geom_line(aes(x=intervals, y=`97.5 %`), alpha=0.5) +
  theme_bw(base_family = "Times")

grid.arrange(line_plot, hist_plot, ncol = 1)

# roberta negative
roberta_neg_coefs <- coef(a$`lhs: roberta_negative`) %>% enframe()
roberta_neg_confint <- confint(a$`lhs: roberta_negative`) %>% rownames_to_column(., "name")

roberta_neg_coefs <- roberta_neg_coefs %>% 
  rename_intervals() %>% 
  inner_join(roberta_neg_confint %>% rename_intervals(), by = c("name", "intervals"))

line_plot <- 
  roberta_neg_coefs %>% ggplot(aes(x=intervals, y=value, group=1)) +
  geom_line() +
  geom_line(aes(x=intervals, y=`2.5 %`), alpha=0.5) +
  geom_line(aes(x=intervals, y=`97.5 %`), alpha=0.5) +
  theme_bw(base_family = "Times")

grid.arrange(line_plot, hist_plot, ncol = 1)

# bertweet positive
bertweet_pos_coefs <- coef(a$`lhs: bertweet_positive`) %>% enframe()
bertweet_pos_confint <- confint(a$`lhs: bertweet_positive`) %>% rownames_to_column(., "name")

bertweet_pos_coefs <- bertweet_pos_coefs %>% 
  rename_intervals() %>% 
  inner_join(bertweet_pos_confint %>% rename_intervals(), by = c("name", "intervals"))

line_plot <- 
  bertweet_pos_coefs %>% ggplot(aes(x=intervals, y=value, group=1)) +
  geom_line() +
  geom_line(aes(x=intervals, y=`2.5 %`), alpha=0.5) +
  geom_line(aes(x=intervals, y=`97.5 %`), alpha=0.5) +
  theme_bw(base_family = "Times")

grid.arrange(line_plot, hist_plot, ncol = 1)

# bertweet negative
bertweet_neg_coefs <- coef(a$`lhs: bertweet_negative`) %>% enframe()
bertweet_neg_confint <- confint(a$`lhs: bertweet_negative`) %>% rownames_to_column(., "name")

bertweet_neg_coefs <- bertweet_neg_coefs %>% 
  rename_intervals() %>% 
  inner_join(bertweet_neg_confint %>% rename_intervals(), by = c("name", "intervals"))

line_plot <- 
  bertweet_neg_coefs %>% ggplot(aes(x=intervals, y=value, group=1)) +
  geom_line() +
  geom_line(aes(x=intervals, y=`2.5 %`), alpha=0.5) +
  geom_line(aes(x=intervals, y=`97.5 %`), alpha=0.5) +
  theme_bw(base_family = "Times")

grid.arrange(line_plot, hist_plot, ncol = 1)

# histogram of pollution bins
full_data %>%
  ggplot(aes(x=pm25_cat)) + 
  geom_histogram(stat="count") +
  theme_bw(base_family = "Times")

# mean sentiment by pollution bin 
full_data %>%
  group_by(pm25_cat) %>%
  summarize(mean_sentiment=mean(vader_compound)) %>% 
  ggplot(aes(x=pm25_cat, y=mean_sentiment)) + 
  geom_point() +
  theme_bw(base_family = "Times")

# pm25 vs. sentiment
full_data %>% 
  drop_na() %>% 
  filter(pm25 < 75) %>% 
  mutate(int_pm=round(pm25)) %>% 
  group_by(int_pm) %>% 
  summarize(vader = mean(vader_compound),
            roberta_pos = mean(roberta_positive),
            bertweet_pos = mean(bertweet_positive)) %>% 
  ggplot(aes(x=int_pm, y=roberta_pos)) +
  geom_line()

# same but during the night
night %>% 
  drop_na() %>% 
  filter(pm25 < 75) %>% 
  mutate(int_pm=round(pm25)) %>% 
  group_by(int_pm) %>% 
  summarize(vader = mean(vader_compound),
            roberta_pos = mean(roberta_positive),
            bertweet_pos = mean(bertweet_positive)) %>% 
  ggplot(aes(x=int_pm, y=roberta_pos)) +
  geom_line()

#### MAPS
usa_counties <- st_read("../data/USA_shapefiles/counties/") |>
  dplyr::select(NAME, STATE_NAME, POP2010) |>
  filter(STATE_NAME != "Alaska") |>
  filter(STATE_NAME != "Hawaii")

# number of tweets by county (per capita)
county_level <- full_data %>% 
  filter(username != "UbicityPrincip") %>%  ## Obviously a bot
  group_by(NAME, STATE_NAME) %>% 
  summarise(num_tweets=n(),
           mean_pollution=mean(pm25, 
                               na.rm=TRUE),
           max_pollution=max(pm25,
                             na.rm=TRUE),
           mean_vader=mean(vader_compound,
                           na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(usa_counties, ., 
            by=c("NAME" = "NAME", "STATE_NAME" = "STATE_NAME")) %>% 
  mutate(tweets_pc=num_tweets/POP2010)


# tweets per capita (county-level)  
county_level %>% 
  ggplot(aes(fill=tweets_pc)) + 
  geom_sf() + 
  scale_fill_viridis(
    option="magma",
    direction=-1
  )

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

