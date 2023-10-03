import csv
from time import sleep
from getpass import getpass
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys


# credentials
NAME = "_ynza_"
PSWD = "scraperpswd"


def login(username, password, driver):
    # wait before the page loads, then input username
    sleep(2)
    name_prompt = driver.find_element(By.XPATH, '//input[@name="text"]')
    name_prompt.send_keys(username)
    next = driver.find_element(By.XPATH, '/html/body/div/div/div/div[1]/div/div/div/div/div/div/div[2]/div[2]/div/div/div[2]/div[2]/div/div/div/div[6]/div')
    next.click()
    # input password
    sleep(2)
    pswd_prompt = driver.find_element(By.XPATH, '//input[@name="password"]')
    pswd_prompt.send_keys(password)
    pswd_prompt.send_keys(Keys.ENTER)


def search(query="", since=None, until=None, geocode=None, driver=None):
    """
    Use advanced search with operators, must use the login() function beforehand
    """
    driver.get("https://www.twitter.com/explore")
    sleep(5)
    
    # add search parameters
    if since is not None:
        query += f', since:"{since}"'
    if until is not None:
        query += f', until:"{until}"'
    if geocode is not None:
        query += f', geocode:"{geocode}"'

    search_window = driver.find_element(By.XPATH, '//input[@aria-label="Search query"]')
    search_window.send_keys(query)
    search_window.send_keys(Keys.ENTER)
    sleep(2)
    driver.find_element(By.LINK_TEXT, 'Latest').click()
    
    # move this below to the scrape_tweets() function
    sleep(2)
    tweets = driver.find_elements(By.XPATH, '//article[@data-testid="tweet"]')
    return tweets


def scrape_tweets(driver):
    """Continously scrape tweets with scrolling"""
    # when time is bounded by both upper and lower timestamp/date, it is possible to 
    # scrape until no further scrolling can be done
    pass


def parse_tweet(tweet):
    """Extract data from a single tweet"""
    username = tweet.find_element(By.XPATH, './/span').text
    handle = tweet.find_element(By.XPATH, './/span[contains(text(), "@")]').text
    try:
        timestamp = tweet.find_element(By.XPATH, './/time').get_attribute('datetime')
    except NoSuchElementException:
        return None
    
    text = tweet.find_element(By.XPATH, './/div[@data-testid="tweetText"]').text
    
    return username, handle, timestamp, text


def create_database(tweets):
    tweets_data = []
    
    for tweet in tweets:
        data = parse_tweet(tweet)
        if data:
            tweets_data.append(data)
    
    return tweets_data


def main():
    options = webdriver.ChromeOptions()
    options.add_argument("--incognito")
    driver = webdriver.Chrome(options=options)
    driver.get("https://www.twitter.com/login")

    login(NAME, PSWD, driver)
    sleep(2)

    tweets = search("weather", until="2015-12-21", geocode="40.768612, -111.906879,10mi", driver=driver)
    database = create_database(tweets)
    return database

if __name__ == "__main__":
    database = main()