from pysentimiento import create_analyzer
from pysentimiento.preprocessing import preprocess_tweet
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from transformers import pipeline, AutoTokenizer, AutoModelForSequenceClassification
from scipy.special import softmax
import pandas as pd
import sys


data = pd.read_csv(sys.argv[1], sep="|")

def preprocess_tweet(tweet):
    """remove user tags and urls"""
    tweet_words = []

    for word in tweet.split(' '):
        word = '@user' if word.startswith('@') and len(word) > 1 else word
        word = 'http' if word.startswith(
            'http') else word
        tweet_words.append(word)

    return " ".join(tweet_words)
    
    
# force text column to string
data["text"] = data["text"].astype(str).apply(preprocess_tweet)

# create columns for different the specific sentiment model
data["bertweet_negative"] = None
data["bertweet_neutral"] = None
data["bertweet_positive"] = None

data["vader_compound"] = None

data["roberta_negative"] = None
data["roberta_neutral"] = None
data["roberta_positive"] = None


# for bertweet
analyzer = create_analyzer(task="sentiment", lang="en")

# for roberta
roberta = "cardiffnlp/twitter-roberta-base-sentiment-latest"
model = AutoModelForSequenceClassification.from_pretrained(roberta)
tokenizer = AutoTokenizer.from_pretrained(roberta)


def bertweet_sentiment(tweet, analyzer):
    result = analyzer.predict(tweet)
    return result


def roberta_sentiment(tweet, model, tokenizer):
    processed = tokenizer(tweet, return_tensors='pt')
    output = model(**processed).logits[0].detach().numpy()
    scores = softmax(output)
    negative, neutral, positive = scores
    return negative, neutral, positive


def vader_sentiment(tweet):
    sid_obj = SentimentIntensityAnalyzer()
    sentiment_dict = sid_obj.polarity_scores(tweet)
    return sentiment_dict['compound']  # compound sentiment is most widely used as a single unidimensional measure of sentiment for a given sentence


for index, row in data.iterrows():
    probas = bertweet_sentiment(row["text"], analyzer).probas
    data.at[index, "bertweet_positive"], data.at[index, "bertweet_neutral"], data.at[index, "bertweet_negative"] = probas['POS'], probas['NEU'], probas['NEG']
    data.at[index, "vader_compound"] = vader_sentiment(row["text"])
    data.at[index, "roberta_negative"], data.at[index, "roberta_neutral"], data.at[index, "roberta_positive"] = roberta_sentiment(row["text"], model, tokenizer)


data.to_csv(sys.argv[2], sep='|')
