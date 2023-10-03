import vaderSentiment
from transformers import AutoTokenizer, AutoModelForSequenceClassification
import pandas as pd
from scipy.special import softmax

sample = pd.read_csv("../data/tweets/tweets_sample.txt", sep='|')