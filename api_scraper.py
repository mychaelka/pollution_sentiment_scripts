import tweepy

# Twitter API credentials
consumer_key = "your_consumer_key"
consumer_secret = "your_consumer_secret"
access_token = "your_access_token"
access_token_secret = "your_access_token_secret"

# Authenticate to Twitter
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

# Create API object
api = tweepy.API(auth)

# User whose tweets you want to scrape
username = "example_user"

# Number of tweets to scrape
tweet_count = 100

# Scrape tweets from the user's timeline
tweets = api.user_timeline(screen_name=username, count=tweet_count)

# Process and print the scraped tweets
for tweet in tweets:
    print(tweet.text)
    print("--------------------")