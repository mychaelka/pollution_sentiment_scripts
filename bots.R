######## BOTS ANALYSIS
library(tidyverse)

## LOAD DATA
vader <- read_delim('../data/tweets/all/july2015_vader_sentiment.csv', delim = '|')
roberta <- read_delim('../data/tweets/all/july2015_roberta_sentiment.csv', delim = '|') %>% 
  dplyr::select(`...1`, roberta_positive, roberta_neutral, roberta_negative)
bertweet <- read_delim('../data/tweets/all/july2015_bertweet_sentiment.csv', delim = '|') %>%
  dplyr::select(`...1`, bertweet_positive, bertweet_neutral, bertweet_negative)
topics <- read_delim('../data/tweets/all/july2015_topics.csv', delim = '|') %>%
  dplyr::select(`...1`, `arts_&_culture`:`label`)

data <- merge(vader, roberta, by = '...1') %>% 
  merge(bertweet, by = '...1') %>% 
  merge(topics, by = '...1')

rm(vader, roberta, bertweet, topics)
gc()

dt <- data |> dplyr::select(id, username, timestamp, vader_compound, label)
rm(data)
gc()

library(data.table)
setDT(dt)
setorder(dt, username, timestamp)

library(stringr)

# choose only one topic for each tweet
dt$label <- lapply(dt$label, function(x) {
  aux <- gsub('\\[', '', x) %>% 
    gsub('\\]', '', .) %>% 
    strsplit(., ",")
  ifelse(is.na(aux[[1]][1]), "'no_topic'", aux[[1]][1])
}) %>% unlist()

# count number of tweets above threshold and average sentiment in specified time window
count_users <- function(dt, thresholds, time_window) {
  
  counts_df <- data.table(threshold=integer(), 
                          user_count=integer(), 
                          mean_sentiment=double(), 
                          main_topic=character())
  
  for (threshold in thresholds) {
    user_counts <- lapply(unique(dt$username), function(user) {
      user_df <- dt[username == user]
      count_tweets <- sapply(1:nrow(user_df), function(i) {
        sum(user_df$timestamp >= user_df$timestamp[i] & user_df$timestamp < (user_df$timestamp[i] + time_window))
      })
      
      avg_sentiment <- sapply(1:nrow(user_df), function(i) {
        mean(user_df[(user_df$timestamp >= user_df$timestamp[i]) & 
                       (user_df$timestamp < (user_df$timestamp[i] + time_window))]$vader_compound, 
             na.rm=TRUE)
      })
      
      topic <- user_df %>% 
        group_by(label) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% 
        slice_head(n = 1)
      
      idx_above_threshold <- which(count_tweets >= threshold)
      count_tweets <- count_tweets[idx_above_threshold]
      avg_sentiment <- avg_sentiment[idx_above_threshold]
      
      df <- data.table(username = user, count_tweets = count_tweets, 
                       sentiment = avg_sentiment, topic = topic[1,1])
      df[complete.cases(df),]
    })
    
    counts <- rbindlist(user_counts)
    
    top_label <- counts %>% 
      group_by(topic.label) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      slice_head(n = 1)
    
    final <- counts %>% group_by(username) %>% 
      summarise(avg_sentiment = mean(sentiment, na.rm=TRUE)) %>% 
      ungroup() %>% 
      summarise(count = n(), sentiment = mean(avg_sentiment, na.rm=TRUE))
    counts_df <- counts_df %>% add_row(threshold = threshold, 
                                       user_count = final$count, 
                                       mean_sentiment = final$sentiment,
                                       main_topic = top_label$topic.label)
  }
  
  counts_df
}

# Define time windows in seconds and thresholds for tweet counts
time_windows <- c(2, 5, 10, 60)
thresholds <- c(2, 5, 10, 50, 100)

# tweeted more than 50 times a day
time_windows <- 24*60*60
thresholds <- 50

# Initialize a list to store results for each time window
all_results <- data.table(time_window=integer(),
                          threshold=integer(), 
                          user_count=integer(), 
                          mean_sentiment=double(), 
                          main_topic=character())

# Loop through each time window and calculate the user counts
for (window in time_windows) {
  results <- count_users(dt, thresholds, window)
  all_results <- all_results %>% add_row(time_window = window, 
                                         threshold = results$threshold,
                                         user_count = results$user_count,
                                         mean_sentiment = results$final_sentiment,
                                         main_topic = results$main_topic)
}


# Combine all results into a single dataframe
write_csv(all_results, "../data/bots_all.csv")
write_csv(all_results, "../data/day_bots.csv")

# filter users that tweeted more than 50 times in 1 day
data$date <- as.Date(data$timestamp)
bots <- data |>
  group_by(username, date) |>
  summarize(count = n()) |>
  filter(count > 40) |> distinct(username)


write_csv(bots, "../data/bots_usernames.csv")
bots <- data |> filter(username %in% bots$username)
mean(bots$vader_compound)
mean(bots$bertweet_neutral)
mean(bots$roberta_neutral)

# filter data to only show bots -- 5,5 seems like a sweet spot, others start catching more and more regular users
time_window <- 5
tweet_count <- 5
setDT(data)
setorder(data, username, timestamp)

bots <- data |> 
  mutate(time_diff = abs(timestamp - lag(timestamp, tweet_count - 1))) |>
  filter((!is.na(time_diff)) & time_diff <= time_window) |>
  ungroup() |> distinct(username)

write_csv(bots, "../data/bot_usernames.csv")

bots <- data |> filter(username %in% bots$username)
mean(bots$vader_compound)
mean(bots$roberta_negative)
mean(bots$roberta_neutral)
mean(bots$roberta_positive)
mean(bots$bertweet_negative)
mean(bots$bertweet_neutral)
mean(bots$bertweet_positive)
