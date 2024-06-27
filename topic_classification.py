import tweetnlp
import pandas as pd

data = pd.read_csv("../data/tweets/all/07-2015_geo.csv", sep='|')

# force text column to string
data["text"] = data["text"].values.astype('str')

# create columns for different topics
data["arts_&_culture"] = None
data["business_&_entrepreneurs"] = None
data["celebrity_&_pop_culture"] = None
data["diaries_&_daily_life"] = None
data["family"] = None
data["fashion_&_style"] = None
data["film_tv_&_video"] = None
data["fitness_&_health"] = None
data["food_&_dining"] = None
data["gaming"] = None
data["learning_&_educational"] = None
data["music"] = None
data["news_&_social_concern"] = None
data["other_hobbies"] = None
data["relationships"] = None
data["science_&_technology"] = None
data["sports"] = None
data["travel_&_adventure"] = None
data["youth_&_student_life"] = None
data["label"] = None

model = tweetnlp.load_model('topic_classification')

for index, row in data.iterrows():
    topics = model.topic(row["text"], return_probability=True)
    data.at[index, "arts_&_culture"] = topics["probability"]["arts_&_culture"]
    data.at[index, "business_&_entrepreneurs"] = topics["probability"]["business_&_entrepreneurs"]
    data.at[index, "celebrity_&_pop_culture"] = topics["probability"]["celebrity_&_pop_culture"]
    data.at[index, "diaries_&_daily_life"] = topics["probability"]["diaries_&_daily_life"]
    data.at[index, "family"] = topics["probability"]["family"]
    data.at[index, "fashion_&_style"] = topics["probability"]["fashion_&_style"]
    data.at[index, "film_tv_&_video"] = topics["probability"]["film_tv_&_video"]
    data.at[index, "fitness_&_health"] = topics["probability"]["fitness_&_health"]
    data.at[index, "food_&_dining"] = topics["probability"]["food_&_dining"]
    data.at[index, "gaming"] = topics["probability"]["gaming"]
    data.at[index, "learning_&_educational"] = topics["probability"]["learning_&_educational"]
    data.at[index, "music"] = topics["probability"]["music"]
    data.at[index, "news_&_social_concern"] = topics["probability"]["news_&_social_concern"]
    data.at[index, "other_hobbies"] = topics["probability"]["other_hobbies"]
    data.at[index, "relationships"] = topics["probability"]["relationships"]
    data.at[index, "science_&_technology"] = topics["probability"]["science_&_technology"]
    data.at[index, "sports"] = topics["probability"]["sports"]
    data.at[index, "travel_&_adventure"] = topics["probability"]["travel_&_adventure"]
    data.at[index, "youth_&_student_life"] = topics["probability"]["youth_&_student_life"]
    data.at[index, "label"] = topics["label"]

data.to_csv('../data/tweets/all/07-2015_topics.csv', sep='|')
