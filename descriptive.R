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
scaled <- data %>% dplyr::select(date, NAME, day, day_hour, username, main_label, 
                                 pm25_cat, pm25,
                                 vader_compound, roberta_positive,
                                 roberta_negative, bertweet_positive,
                                 bertweet_negative) %>% 
  drop_na() %>% 
  mutate(across(c(pm25:bertweet_negative), ~ (.-mean(.)) / sd(.)))


##### REGRESSION TEST
## County + date + username + topic, cluster by county
feols(
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25 | NAME + date + username,
  data = scaled,
  cluster = c("NAME")
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
  c(vader_compound, roberta_positive, roberta_negative, bertweet_positive, bertweet_negative) ~ pm25_cat + temp + precip + visibility | CNTY_UNIQUE + date + day,
  data = grouped,
  cluster = c("CNTY_UNIQUE", "date"),
  weights = grouped$num_tweets
) 

# vader
vader_coefs <- coef(a$`lhs: vader_compound`) %>% enframe() %>% filter(name != "temp" & name != "precip")
vader_confint <- confint(a$`lhs: vader_compound`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp" & name != "precip")

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
  geom_smooth(se=FALSE, color = "darkgreen") +
  geom_ribbon(aes(ymin = smooth_2.5, ymax = smooth_97.5), fill = "darkgreen", alpha = 0.1) +
  geom_line(aes(y = smooth_2.5), alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_97.5), alpha = 0.5, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth=1.2) +
  theme_bw() +
  xlab(NULL) +
  ylab("SD change in sentiment (VADER)") +
  theme(text = element_text(size = 15))

hist_plot <- grouped %>% 
  ggplot(aes(x=pm25_cat)) + 
  geom_bar(color="black", fill="lightblue", alpha=0.5) +
  theme_bw() + 
  xlab("Air pollution level (pm2.5)") +
  ylab("Number of tweets") +
  theme(text=element_text(size=15))


grid.arrange(vader_line_plot, hist_plot, ncol = 1, heights=c(3,1))


# ROBERTA
roberta_pos_coefs <- coef(a$`lhs: roberta_positive`) %>% enframe() %>% filter(name != "temp" & name != "precip")
roberta_pos_confint <- confint(a$`lhs: roberta_positive`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp" & name != "precip")

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

roberta_neg_coefs <- coef(a$`lhs: roberta_negative`) %>% enframe() %>% filter(name != "temp" & name != "precip")
roberta_neg_confint <- confint(a$`lhs: roberta_negative`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp" & name != "precip")

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
  geom_smooth(se=FALSE, aes(color='negative')) +
  geom_ribbon(aes(ymin = smooth_2.5, ymax = smooth_97.5), fill="darkred", alpha=0.1) +
  geom_line(aes(y = smooth_2.5), alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_97.5), alpha = 0.5, color = "grey") +
  theme_bw() +
  xlab(NULL) +
  ylim(-0.9, 0.91) +
  ylab("SD change in sentiment (RoBERTa)") +
  geom_smooth(data=roberta_pos_coefs, se=FALSE, aes(x=intervals, y=smooth_value, group=1, col='positive')) +
  geom_ribbon(data=roberta_pos_coefs, aes(ymin = smooth_2.5, ymax = smooth_97.5), fill="darkgreen", alpha=0.1) +
  geom_line(data=roberta_pos_coefs, aes(x=intervals, y=smooth_2.5), alpha=0.5, col="grey") +
  geom_line(data=roberta_pos_coefs, aes(x=intervals, y=smooth_97.5), alpha=0.5, col="grey") +
  geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=1.2) +
  theme(text=element_text(size=15), legend.position = c(0.15, 0.10)) +
  scale_color_manual(name='Sentiment polarity',
                     breaks=c('negative', 'positive'),
                     values=c('negative'='darkred', 'positive'='darkgreen'))

grid.arrange(roberta_line_plot, hist_plot, ncol = 1, heights=c(3,1))

# BERTWEET
bertweet_pos_coefs <- coef(a$`lhs: bertweet_positive`) %>% enframe() %>% filter(name != "temp" & name != "precip")
bertweet_pos_confint <- confint(a$`lhs: bertweet_positive`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp" & name != "precip")

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

bertweet_neg_coefs <- coef(a$`lhs: bertweet_negative`) %>% enframe() %>% filter(name != "temp" & name != "precip")
bertweet_neg_confint <- confint(a$`lhs: bertweet_negative`) %>% rownames_to_column(., "name") %>% 
  filter(name != "temp" & name != "precip")

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
  geom_smooth(se=FALSE, aes(color='negative')) +
  geom_ribbon(aes(ymin=smooth_2.5, ymax=smooth_97.5), fill="darkred", alpha=0.1) +
  geom_line(aes(y = smooth_2.5), alpha = 0.5, color = "grey") +
  geom_line(aes(y = smooth_97.5), alpha = 0.5, color = "grey") +
  theme_bw() +
  xlab(NULL) +
  ylim(-0.9, 0.91) +
  ylab("SD change in sentiment (BERTweet)") +
  geom_smooth(data=bertweet_pos_coefs, se=FALSE, aes(x=intervals, y=smooth_value, group=1, col='positive')) +
  geom_ribbon(data=bertweet_pos_coefs,aes(ymin=smooth_2.5, ymax=smooth_97.5), fill="darkgreen", alpha=0.1) +
  geom_line(data=bertweet_pos_coefs,aes(x=intervals, y=smooth_2.5), alpha=0.5, col="grey") +
  geom_line(data=bertweet_pos_coefs,aes(x=intervals, y=smooth_97.5), alpha=0.5, col="grey") +
  geom_hline(yintercept=0, linetype="dashed", color="black", linewidth=1.2) +
  theme(text=element_text(size=15), legend.position = c(0.15, 0.10)) +
  scale_color_manual(name='Sentiment polarity',
                     breaks=c('negative', 'positive'),
                     values=c('negative'='darkred', 'positive'='darkgreen'))

grid.arrange(bertweet_line_plot, hist_plot, ncol = 1, heights=c(3,1))



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

# county-level aggregation
county_level <- data %>% 
  drop_na %>% 
  mutate(across(c(vader_compound:bertweet_negative), ~ scale(.))) %>% 
  group_by(NAME, STATE_NAME) %>% 
  summarise(num_tweets=n(),
           mean_pollution=mean(pm25, 
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
            by=c("NAME" = "NAME", "STATE_NAME" = "STATE_NAME")) %>% 
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
    legend.title = element_text(size = 13),
    legend.key.size = unit(1, 'cm')
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

### distribution over grid -- takes a long time
us_grid <- usa_states %>% st_make_grid(n=c(100, 100))
grid_map <- st_intersection(usa_states, us_grid) %>% 
  st_as_sf() %>% 
  mutate(grid_id = 1:n())

tweets_per_grid <- grid_map %>%
  st_transform(5070) %>% 
  st_join(gps_data %>% 
            st_transform(5070)) %>% 
  group_by(grid_id) %>% 
  summarize(total=sum(!is.na(username)))

ggplot() +
  geom_sf(data=tweets_per_grid, aes(fill=total), color=NA) +
  geom_sf(data=usa_counties, size=0.2) + 
  scale_fill_viridis_c(
    option="G",
    direction=-1,
    na.value = "white",
    guide = guide_colourbar(order = 1,
                            title = "Tweet distribution",
                            title.position = 'top')
  )

# mean Vader sentiment per county
county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_vader)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(order = 1,
                            title = "Mean VADER sentiment",
                           title.position = 'top')
  ) +
  #scale_fill_distiller(
  #  palette = "PiYG",
  #  direction=1,
  #  na.value = "white",
  # guide = guide_colourbar(title = "Mean VADER sentiment",
  #                          title.position = 'top')
  #) + 
  theme(
    legend.direction = "horizontal",
    legend.justification = c(0,0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 13),
    legend.key.size = unit(1.2, 'cm')
  )

ggsave(filename = "../figs/paper/mean_vader.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")

# mean positive sentiment intensity per county
roberta_pos <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_roberta_pos)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(order = 1,
                            title = "Mean positive sentiment intensity",
                            title.position = 'top')
  ) +
  theme(
    legend.direction = "horizontal",
    legend.justification = c(1,0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10),
    legend.key.size = unit(1, 'cm')
  )

bertweet_pos <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_bertweet_pos)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey"
  ) + 
  theme(legend.position="none")

grid.arrange(roberta_pos, bertweet_pos, nrow=1)

g <- arrangeGrob(roberta_pos, bertweet_pos, nrow=1)
ggsave(filename = "../figs/paper/mean_positive.png", plot=g, width = 15, 
       height = 7, device='png', dpi=700, bg = "white")


# mean negative sentiment intensity per county
roberta_neg <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_roberta_neg)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey",
    guide = guide_colourbar(title = "Mean negative sentiment intensity",
                            title.position = 'top')
  ) +
  theme(
    legend.direction = "horizontal",
    legend.justification = c(1,0),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 10),
    legend.key.size = unit(1, 'cm')
  )

bertweet_neg <- county_level %>% 
  st_transform(5070) %>% 
  ggplot(aes(fill=mean_bertweet_neg)) + 
  geom_sf(lwd=0.05) + 
  theme_void() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    na.value = "grey"
  ) + 
  theme(legend.position="none")

grid.arrange(roberta_neg, bertweet_neg, nrow=1)
g <- arrangeGrob(roberta_neg, bertweet_neg, nrow=1)
ggsave(filename = "../figs/paper/mean_negative.png", plot=g, width = 15, 
       height = 7, device='png', dpi=700, bg = "white")


#### POLLUTION
ecmwf <- read_stars("../data/ECMWF/july2015_new.nc")
pm25 <- st_as_sf(ecmwf[6]) %>% parse_nums() %>% 
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
                            title = "Mean pm2.5 exposure",
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


#### WEATHER
file_dir <- "../data/weather/PRISM_tmean_stable_4kmD2_20150701_20150731_bil/" 
files <- list.files(path = file_dir, pattern = "PRISM_tmean_stable_4kmD2_201507\\d{2}_bil.bil$",
                    full.names = TRUE)

# empty list to store data
data_list <- list()

for (file in files) {
  raster_data <- raster(file)
  raster_df <- as.data.frame(raster_data, xy = TRUE)
  
  # Extract the date from the file name
  date <- substr(basename(file), 28, 35)  # Adjust the position based on your file name structure
  date <- as.Date(date, format = "%Y%m%d")
  raster_df$date <- date
  
  data_list[[length(data_list) + 1]] <- raster_df
}

big_table <- do.call(rbind, data_list)

# View the resulting data frame
head(big_table)
