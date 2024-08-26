library(tidyverse)
library(lutz)
library(sf)
library(raster)
library(ggspatial)
library(viridis)
library(reshape2)
library(fixest)
library(gridExtra)

load("../data/models.RData")

# sample characteristics
cnty_aggr <- full_data %>% group_by(CNTY_UNIQUE, date) %>% summarize(pm25=median(pm25),
                                                                     temp=median(temperature),
                                                                     precip=median(precipitation),
                                                                     visibility=median(visibility))
summary(cnty_aggr$pm25)
sd(cnty_aggr$pm25, na.rm=TRUE)
summary(cnty_aggr$temp)
sd(cnty_aggr$temp, na.rm=TRUE)
summary(cnty_aggr$precip)
sd(cnty_aggr$precip, na.rm=TRUE)
summary(cnty_aggr$visibility)
sd(cnty_aggr$visibility, na.rm=TRUE)

summary(full_data$vader_compound)
sd(full_data$vader_compound, na.rm=TRUE)
summary(full_data$roberta_positive)
sd(full_data$roberta_positive)
summary(full_data$roberta_negative)
sd(full_data$roberta_negative)
summary(full_data$bertweet_positive)
sd(full_data$bertweet_positive)
summary(full_data$bertweet_negative)
sd(full_data$bertweet_negative)

# correlation between standardized sentiment measures
scaled <- full_data %>% dplyr::select(vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  drop_na() %>% 
  mutate(across(c(vader_compound:bertweet_negative), ~ (.-mean(.)) / sd(.)))

cor(scaled)

# tweets per person
tweets_user <- full_data %>% 
  dplyr::select(username) %>% 
  st_drop_geometry() %>% 
  group_by(username) %>% 
  summarize(count = n())
summary(tweets_user$count)
sd(tweets_user$count)

# tweets per county
tweets_county <- full_data %>% 
  dplyr::select(CNTY_UNIQUE) %>% 
  st_drop_geometry() %>% 
  group_by(CNTY_UNIQUE) %>% 
  summarize(count = n())
summary(tweets_county$count)
sd(tweets_county$count)

# how many tweets per each pollution bin? 
data %>%
  ggplot(aes(x=pm25_cat)) + 
  geom_histogram(stat="count") +
  theme_bw(base_family = "Times")


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
scaled <- data %>% dplyr::select(date, NAME, day, day_hour, username, main_label, 
                                 pm25_cat, pm25,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))

# positive 
pos <- scaled %>%
       group_by(day) %>% 
       summarize(RoBERTa = mean(roberta_positive),
                 BERTweet = mean(bertweet_positive),
                 VADER = mean(vader_compound)) %>%
       melt(id.vars = "day") %>% 
       mutate(lag=lag(value)) %>% 
       mutate(diff=value-lag)

# negative
neg <- scaled %>%
  group_by(day) %>% 
  summarize(RoBERTa = mean(roberta_negative),
            BERTweet = mean(bertweet_negative)) %>%
  melt(id.vars = "day") %>% 
  mutate(lag=lag(value)) %>% 
  mutate(diff=value-lag)


pos_plot <- pos %>%
  mutate(day = factor(day, levels = c(0, 1, 2, 3, 4, 5, 6),
                      labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>% 
  ggplot(aes(x=day, y=value, group=variable, col=variable)) + 
  geom_line(linewidth=1) +
  geom_point(aes(shape=variable), size=3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", linewidth=1.1) +
  theme_minimal(base_family = "Times") +
  scale_color_manual(name='Sentiment measure',
                     breaks=c('RoBERTa', 'BERTweet', 'VADER'),
                     values=c('RoBERTa' = 'darkgreen', 'BERTweet'='chartreuse2', 'VADER'= 'blue')) +
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)) +
  guides(shape = "none") +
  ylim(-0.1, 0.1) +
  xlab(NULL) +
  ylab("Mean standardized sentiment") 

neg_plot <- neg %>% 
  mutate(day = factor(day, levels = c(0, 1, 2, 3, 4, 5, 6),
                      labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>% 
  ggplot(aes(x=day, y=value, group=variable, col=variable)) + 
  geom_line(linewidth=1) +
  geom_point(aes(shape=variable), size=3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", linewidth=1.1) +
  theme_minimal(base_family = "Times") +
  scale_color_manual(name='Sentiment measure',
                     breaks=c('RoBERTa', 'BERTweet'),
                     values=c('RoBERTa' = 'darkred', 'BERTweet'='magenta')) +
  theme(text=element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)) +
  guides(shape = "none") +
  ylim(-0.1, 0.1) +
  xlab(NULL) +
  ylab(NULL) 

grid.arrange(pos_plot, neg_plot, nrow = 1, widths=c(1,1))


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

# cut pm25 into finer bins
data <- data %>% drop_na() %>% 
  mutate(
    pm25_cat = cut(pm25, breaks = c(seq(from = 0, to = 70, by = 5),Inf), include.lowest = TRUE)
  )

# standardize 
scaled <- data %>% dplyr::select(date, CNTY_UNIQUE, day, day_hour, username, main_label,
                                 pm25_cat, pm25,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))


# county level, nonlinearity
a <- feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat + temp_med + I(temp_med^2) + precip + visibility | CNTY_UNIQUE + date,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
)

# vader
vader_coefs <- coef(a$`lhs: vader_compound`) %>% enframe() %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")
vader_confint <- confint(a$`lhs: vader_compound`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")

interval_levels <- c("[0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]",
                     "(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", 
                     "(55,60]", "(60,65]", "(65,70]", "(70,Inf]")

# smoothed CIs
vader_coefs <- vader_coefs %>%  
  rename_intervals() %>% 
  inner_join(vader_confint %>% rename_intervals(), by = c("name", "intervals")) %>%
  mutate(intervals = factor(intervals, levels = interval_levels)) %>% 
  mutate(intervals_numeric = as.numeric(factor(intervals, levels = interval_levels)))

loess_fit_value <- loess(value ~ intervals_numeric, data = vader_coefs)
loess_fit_lower <- loess(`2.5 %` ~ intervals_numeric, data = vader_coefs)
loess_fit_upper <- loess(`97.5 %` ~ intervals_numeric, data = vader_coefs)

baseline <- data.frame(
  name = "[0,5]",
  value = 0,
  intervals = factor("[0,5]", levels = interval_levels, ordered = TRUE),
  intervals_numeric = 0,
  smooth_value = 0,
  smooth_2.5 = 0,
  smooth_97.5 = 0
)

baseline$`2.5 %` = 0
baseline$`97.5 %` = 0

vader_coefs <- vader_coefs %>%
  mutate(
    smooth_value = predict(loess_fit_value, intervals_numeric),
    smooth_2.5 = predict(loess_fit_lower, intervals_numeric),
    smooth_97.5 = predict(loess_fit_upper, intervals_numeric)
  )

vader_coefs <- bind_rows(baseline, vader_coefs) 

vader_line_plot <- vader_coefs %>%
  ggplot(aes(x = intervals, y = smooth_value, group = 1)) +
  geom_line(color = "darkblue", linewidth=1.2) +
  geom_ribbon(aes(ymin = smooth_2.5, ymax = smooth_97.5), fill = "darkblue", alpha = 0.1) +
  geom_line(aes(y = smooth_2.5), alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_97.5), alpha = 0.5, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth=1.2) +
  theme_bw() +
  xlab(NULL) +
  ylim(-1.1, 0.91) +
  ylab("SD change in sentiment (VADER)") +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90))

hist_plot <- grouped %>% 
  ggplot(aes(x=pm25_cat)) + 
  geom_bar(color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + 
  xlab("Air pollution level (pm2.5)") +
  ylab("Number of tweets") +
  theme(text=element_text(size=15),
        axis.text.x = element_text(angle = 90))


grid.arrange(vader_line_plot, hist_plot, ncol = 1, heights=c(3,1))


# ROBERTA
roberta_pos_coefs <- coef(a$`lhs: roberta_positive`) %>% enframe() %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")
roberta_pos_confint <- confint(a$`lhs: roberta_positive`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")

roberta_pos_coefs <- roberta_pos_coefs %>% 
  rename_intervals() %>% 
  inner_join(roberta_pos_confint %>% rename_intervals(), by = c("name", "intervals")) %>%
  mutate(intervals = factor(intervals, levels = interval_levels)) %>% 
  mutate(intervals_numeric = as.numeric(factor(intervals, levels = interval_levels)))

loess_fit_value <- loess(value ~ intervals_numeric, data = roberta_pos_coefs)
loess_fit_lower <- loess(`2.5 %` ~ intervals_numeric, data = roberta_pos_coefs)
loess_fit_upper <- loess(`97.5 %` ~ intervals_numeric, data = roberta_pos_coefs)

roberta_pos_coefs <- roberta_pos_coefs %>%
  mutate(
    smooth_value = predict(loess_fit_value, intervals_numeric),
    smooth_2.5 = predict(loess_fit_lower, intervals_numeric),
    smooth_97.5 = predict(loess_fit_upper, intervals_numeric)
  )

roberta_neg_coefs <- coef(a$`lhs: roberta_negative`) %>% enframe() %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")
roberta_neg_confint <- confint(a$`lhs: roberta_negative`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")

roberta_neg_coefs <- roberta_neg_coefs %>% 
  rename_intervals() %>% 
  inner_join(roberta_neg_confint %>% rename_intervals(), by = c("name", "intervals")) %>%
  mutate(intervals = factor(intervals, levels = interval_levels)) %>% 
  mutate(intervals_numeric = as.numeric(factor(intervals, levels = interval_levels)))

loess_fit_value <- loess(value ~ intervals_numeric, data = roberta_neg_coefs)
loess_fit_lower <- loess(`2.5 %` ~ intervals_numeric, data = roberta_neg_coefs)
loess_fit_upper <- loess(`97.5 %` ~ intervals_numeric, data = roberta_neg_coefs)

roberta_neg_coefs <- roberta_neg_coefs %>%
  mutate(
    smooth_value = predict(loess_fit_value, intervals_numeric),
    smooth_2.5 = predict(loess_fit_lower, intervals_numeric),
    smooth_97.5 = predict(loess_fit_upper, intervals_numeric)
  )

roberta_pos_coefs <- bind_rows(baseline, roberta_pos_coefs) 
roberta_neg_coefs <- bind_rows(baseline, roberta_neg_coefs) 

roberta_line_plot <- 
  roberta_neg_coefs %>% ggplot(aes(x=intervals, y=smooth_value, group=1)) +
  geom_line(aes(color='negative'), linewidth=1.2) +
  geom_ribbon(aes(ymin = smooth_2.5, ymax = smooth_97.5), fill="darkred", alpha=0.1) +
  geom_line(aes(y = smooth_2.5), alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_97.5), alpha = 0.5, color = "grey") +
  theme_bw() +
  xlab(NULL) +
  ylim(-1.1, 0.91) +
  ylab("SD change in sentiment (RoBERTa)") +
  geom_line(data=roberta_pos_coefs, aes(x=intervals, y=smooth_value, group=1, col='positive'), linewidth=1.2) +
  geom_ribbon(data=roberta_pos_coefs, aes(ymin = smooth_2.5, ymax = smooth_97.5), fill="darkgreen", alpha=0.1) +
  geom_line(data=roberta_pos_coefs, aes(x=intervals, y=smooth_2.5), alpha=0.5, col="grey") +
  geom_line(data=roberta_pos_coefs, aes(x=intervals, y=smooth_97.5), alpha=0.5, col="grey") +
  geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=1.2) +
  theme(text=element_text(size=15), legend.position = c(0.15, 0.10),
        axis.text.x = element_text(angle = 90)) +
  scale_color_manual(name='Sentiment polarity',
                     breaks=c('negative', 'positive'),
                     values=c('negative'='darkred', 'positive'='darkgreen'))

grid.arrange(roberta_line_plot, hist_plot, ncol = 1, heights=c(3,1))

# BERTWEET
bertweet_pos_coefs <- coef(a$`lhs: bertweet_positive`) %>% enframe() %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")
bertweet_pos_confint <- confint(a$`lhs: bertweet_positive`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")

bertweet_pos_coefs <- bertweet_pos_coefs %>% 
  rename_intervals() %>% 
  inner_join(bertweet_pos_confint %>% rename_intervals(), by = c("name", "intervals")) %>%
  mutate(intervals = factor(intervals, levels = interval_levels)) %>% 
  mutate(intervals_numeric = as.numeric(factor(intervals, levels = interval_levels)))

loess_fit_value <- loess(value ~ intervals_numeric, data = bertweet_pos_coefs)
loess_fit_lower <- loess(`2.5 %` ~ intervals_numeric, data = bertweet_pos_coefs)
loess_fit_upper <- loess(`97.5 %` ~ intervals_numeric, data = bertweet_pos_coefs)

bertweet_pos_coefs <- bertweet_pos_coefs %>%
  mutate(
    smooth_value = predict(loess_fit_value, intervals_numeric),
    smooth_2.5 = predict(loess_fit_lower, intervals_numeric),
    smooth_97.5 = predict(loess_fit_upper, intervals_numeric)
  )

bertweet_neg_coefs <- coef(a$`lhs: bertweet_negative`) %>% enframe() %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")
bertweet_neg_confint <- confint(a$`lhs: bertweet_negative`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp_med" & name != "I(temp_med^2)" & name != "precip" & name != "visibility")

bertweet_neg_coefs <- bertweet_neg_coefs %>% 
  rename_intervals() %>% 
  inner_join(bertweet_neg_confint %>% rename_intervals(), by = c("name", "intervals")) %>%
  mutate(intervals = factor(intervals, levels = interval_levels)) %>% 
  mutate(intervals_numeric = as.numeric(factor(intervals, levels = interval_levels)))

loess_fit_value <- loess(value ~ intervals_numeric, data = bertweet_neg_coefs)
loess_fit_lower <- loess(`2.5 %` ~ intervals_numeric, data = bertweet_neg_coefs)
loess_fit_upper <- loess(`97.5 %` ~ intervals_numeric, data = bertweet_neg_coefs)

bertweet_neg_coefs <- bertweet_neg_coefs %>%
  mutate(
    smooth_value = predict(loess_fit_value, intervals_numeric),
    smooth_2.5 = predict(loess_fit_lower, intervals_numeric),
    smooth_97.5 = predict(loess_fit_upper, intervals_numeric)
  )

bertweet_pos_coefs <- bind_rows(baseline, bertweet_pos_coefs) 
bertweet_neg_coefs <- bind_rows(baseline, bertweet_neg_coefs) 

bertweet_line_plot <- 
  bertweet_neg_coefs %>% ggplot(aes(x=intervals, y=smooth_value, group=1)) +
  geom_line(aes(color='negative'), linewidth=1.2) +
  geom_ribbon(aes(ymin=smooth_2.5, ymax=smooth_97.5), fill="darkred", alpha=0.1) +
  geom_line(aes(y = smooth_2.5), alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_97.5), alpha = 0.5, color = "grey") +
  theme_bw() +
  xlab(NULL) +
  ylim(-1.1, 0.91) +
  ylab("SD change in sentiment (BERTweet)") +
  geom_line(data=bertweet_pos_coefs, aes(x=intervals, y=smooth_value, group=1, col='positive'), linewidth=1.2) +
  geom_ribbon(data=bertweet_pos_coefs,aes(ymin=smooth_2.5, ymax=smooth_97.5), fill="darkgreen", alpha=0.1) +
  geom_line(data=bertweet_pos_coefs,aes(x=intervals, y=smooth_2.5), alpha=0.5, col="grey") +
  geom_line(data=bertweet_pos_coefs,aes(x=intervals, y=smooth_97.5), alpha=0.5, col="grey") +
  geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=1.2) +
  theme(text=element_text(size=15), legend.position = c(0.15, 0.10),
        axis.text.x = element_text(angle = 90)) +
  scale_color_manual(name='Sentiment polarity',
                     breaks=c('negative', 'positive'),
                     values=c('negative'='darkred', 'positive'='darkgreen'))

grid.arrange(bertweet_line_plot, hist_plot, ncol = 1, heights=c(3,1))



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



#### MAPS
usa_counties <- st_read("../data/USA_shapefiles/counties/") |>
  dplyr::select(NAME, STATE_NAME, POP2010) |>
  filter(STATE_NAME != "Alaska") |>
  filter(STATE_NAME != "Hawaii") |> 
  mutate(CNTY_UNIQUE=paste(usa_counties$NAME, usa_counties$STATE_NAME))

# county-level aggregation
# we want mean because this is aggregated for the whole month, but in the models we use median
county_level <- data %>% 
  drop_na %>% 
  group_by(CNTY_UNIQUE) %>% 
  summarise(num_tweets=n(),
           mean_pollution=mean(pm25, 
                               na.rm=TRUE),
           median_pollution=median(pm25, 
                               na.rm=TRUE),
           max_pollution=max(pm25,
                             na.rm=TRUE),
           mean_vader=mean(vader_compound,
                           na.rm=TRUE),
           mean_roberta_pos=mean(roberta_positive,
                           na.rm=TRUE),
           mean_roberta_neg=mean(roberta_negative,
                           na.rm=TRUE),
           mean_bertweet_pos=mean(bertweet_positive,
                                 na.rm=TRUE),
           mean_bertweet_neg=mean(bertweet_negative,
                                 na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(usa_counties, ., 
            by=c("CNTY_UNIQUE" = "CNTY_UNIQUE")) %>% 
  mutate(tweets_pc=num_tweets/POP2010)

# how many US counties exceed the WHO limit for long-term exposure to pm2.5?
county_level %>% 
  st_transform(5070) %>% 
  filter(mean_pollution > 15) %>% 
  ggplot(aes(fill=mean_pollution)) + 
  geom_sf(lwd=0.05) +
  theme_void() +
  coord_sf(datum = NA) 

# log 10 number of tweets per county
county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=log(num_tweets, base=10))) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="G",
    direction=-1,
    na.value = "white",
    guide = guide_colourbar(order = 1,
                            title = "Tweets per county (log10)",
                            title.position = 'top')
  ) + 
  theme(
    legend.direction = "horizontal",
    legend.justification = c(0, 0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.key.size = unit(1.5, 'cm')
  )
ggsave(filename = "../figs/paper/tweets_per_county.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")

# tweets per capita (county-level)  
county_level %>% 
  ggplot(aes(fill=log(tweets_pc))) + 
  geom_sf() + 
  scale_fill_viridis(
    option="magma",
    direction=-1
  )

# users per county?

# tweet distribution
usa_states <- st_read("../data/USA_shapefiles/States_shapefile/")
usa_states <- usa_states[-c(2,12),] %>% 
  dplyr::select(State_Code, State_Name)
gps_data <- data %>% 
  st_as_sf(coords=c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(5070)

usa_states %>% 
  st_transform(5070) %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data=gps_data, color="#081d58", alpha=0.2, size=0.2) +
  coord_sf(datum=NA) +
  theme_void()
ggsave(filename = "../figs/paper/tweet_distribution.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")

gps_data_bots <- all_bots %>% 
  st_as_sf(coords=c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(5070)

usa_states %>% 
  st_transform(5070) %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data=gps_data_bots, color="#a00000", alpha=0.2, size=0.2) +
  coord_sf(datum=NA) +
  theme_void()
ggsave(filename = "../figs/paper/tweet_distribution_bots.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")

# mean Vader sentiment per county
county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_vader)) + 
  geom_sf(lwd=0.2) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(order = 1,
                            title = "Mean VADER sentiment",
                            title.position = 'top')
  ) +
  theme(
    text=element_text(size=15),
    legend.direction = "horizontal",
    legend.justification = c(0,0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.key.size = unit(1.5, 'cm')
  )

ggsave(filename = "../figs/paper/mean_vader.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")

# mean roberta sentiment intensity per county
roberta_pos <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_roberta_pos)) + 
  geom_sf(lwd=0.2) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(order = 1,
                            title = "Sentiment intensity",
                            title.position = 'top')
  ) +
  theme(
    text=element_text(size=15),
    legend.direction = "horizontal",
    legend.justification = c(0,0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.key.size = unit(1.5, 'cm')
  )

roberta_neg <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_roberta_neg)) + 
  geom_sf(lwd=0.2) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(title = "Sentiment intensity",
                            title.position = 'top', alpha = 0)
  ) +
  theme(
    legend.direction = "horizontal",
    legend.justification = c(0,0),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(size = 15, color="white"),
    legend.text = element_text(color="white"),
    legend.key.size = unit(1, 'cm'),
    legend.key = element_rect(color="white")
  )

grid.arrange(roberta_pos, roberta_neg, nrow=1)

g <- arrangeGrob(roberta_pos, roberta_neg, nrow=1)
ggsave(filename = "../figs/paper/mean_roberta.png", plot=g, width = 15, 
       height = 7, device='png', dpi=700, bg = "white")


# mean bertweet sentiment intensity per county
bertweet_pos <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_bertweet_pos)) + 
  geom_sf(lwd=0.2) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(title = "Sentiment intensity",
                            title.position = 'top')
  ) +
  theme(
    text=element_text(size=15),
    legend.direction = "horizontal",
    legend.justification = c(0,0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.key.size = unit(1.5, 'cm')
  )

bertweet_neg <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_bertweet_neg)) + 
  geom_sf(lwd=0.2) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(title = "Negative sentiment intensity",
                            title.position = 'top', alpha = 0)
  ) +
  theme(
    legend.direction = "horizontal",
    legend.justification = c(0,0),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(size = 15, color="white"),
    legend.text = element_text(color="white"),
    legend.key.size = unit(1, 'cm'),
    legend.key = element_rect(color="white")
  )

grid.arrange(bertweet_pos, bertweet_neg, nrow=1)
g <- arrangeGrob(bertweet_pos, bertweet_neg, nrow=1)
ggsave(filename = "../figs/paper/mean_bertweet.png", plot=g, width = 15, 
       height = 7, device='png', dpi=700, bg = "white")


#### POLLUTION
ecmwf <- read_stars("../data/ECMWF/ecmwf_july.nc")
pm25 <- st_as_sf(ecmwf[4]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::mutate(
    id = row_number()
  )

rm(ecmwf)
gc()

# mean pollution by county
county_level %>%
st_transform(5070) %>% 
  ggplot(aes(fill=mean_pollution)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="rocket",
    direction=-1,
    na.value = "white",
    guide = guide_colourbar(order = 1,
                            title = "Mean PM2.5 exposure",
                            title.position = 'top')
  ) + 
  theme(
    legend.direction = "horizontal",
    legend.justification = c(0, 0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 15),
    legend.key.size = unit(1.5, 'cm')
  )

ggsave(filename = "../figs/paper/mean_pollution.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")


# max pollution per county
county_level %>%
  st_transform(5070) %>% 
  ggplot(aes(fill=max_pollution)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="rocket",
    direction=-1,
    na.value = "white",
    guide = guide_colourbar(order = 1,
                            title = "Max pm2.5 exposure",
                            title.position = 'top')
  ) + 
  theme(
    legend.direction = "horizontal",
    legend.justification = c(0, 0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 13),
    legend.key.size = unit(1, 'cm')
  )

ggsave(filename = "../figs/paper/max_pollution.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")
