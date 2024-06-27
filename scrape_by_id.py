import csv
from time import sleep
from getpass import getpass
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException, TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from datetime import date


# credentials
NAME = ""
PSWD = ""
TEST_IDS = ['617936021947305985', '617936027118903296', '617936177941872640', '617936302890196992',
            '617936382804275200', '617936395940798464', '617936537846738944', '617936557522161664']
TEST_ID = '1689206501255081984'
TEST_USERNAME = 'random'
IDS_PATH = ''


def load_ids(path):
    with open(path, 'r') as text_file:
        lines = text_file.readlines()
    return lines


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


def search_by_id(id, driver):
    url = f'https://twitter.com/{TEST_USERNAME}/status/{id}'
    driver.get(url)
    sleep(3)
    try:
        tweet = driver.find_element(By.XPATH, '//article[@data-testid="tweet"]')
    except NoSuchElementException:
        try:
            if driver.find_element(By.XPATH, '//*[@id="react-root"]/div/div/div[2]/main/div/div/div/div[1]/div/div[3]/div[1]/span').text == "Something went wrong. Try reloading.":
                sleep(300)
            return None
        except NoSuchElementException:
            return None 
    return tweet


def parse_tweet(tweet):
    """Extract data from a single tweet"""
    try:
        username = tweet.find_element(By.XPATH, './/span').text
        handle = tweet.find_element(By.XPATH, './/span[contains(text(), "@")]').text
        timestamp = tweet.find_element(By.XPATH, './/time').get_attribute('datetime')
        location = tweet.find_element(By.XPATH, '//*[@id="react-root"]/div/div/div[2]/main/div/div/div/div[1]/div/section/div/div/div/div/div[1]/div/div/article/div/div/div[3]/div[4]/div/div[1]/div/div/a[2]/span').text
        text = tweet.find_element(By.XPATH, './/div[@data-testid="tweetText"]').text
        # emojis
        try:
            images = tweet.find_elements(By.XPATH, './/img')
            for image in images:
                text += image.get_attribute('alt')
        except NoSuchElementException:
            pass
    except NoSuchElementException:
        return None
    
    return username, handle, timestamp, location, text


def write_data(tweets):
    with open(f'out{date.today().strftime("%d-%m-%Y")}.txt', 'w') as out:
        csv_out = csv.writer(out, delimiter='|')
        csv_out.writerow(['name', 'handle', 'timestamp', 'location', 'text'])
        for tweet in tweets:
            csv_out.writerow(tweet)
            

def main():
    ids = load_ids(IDS_PATH)
    options = webdriver.ChromeOptions()
    options.add_argument("--incognito")
    #options.add_argument("--enable-automation")
    driver = webdriver.Chrome(options=options)
    driver.get("https://www.twitter.com/login")

    login(NAME, PSWD, driver)
    sleep(2)

    tweet_data = None
    tweet = None
    done = open('done.txt', 'w')

    with open(f'out{date.today().strftime("%d-%m-%Y")}.txt', 'w') as out:
        csv_out = csv.writer(out, delimiter='|')
        csv_out.writerow(['name', 'handle', 'timestamp', 'location', 'text'])
        
        # ids = [TEST_ID]
        for id in ids:
            try:
                tweet = search_by_id(id, driver)
            except TimeoutException:
                sleep(900)
            done.write(id)
            if not tweet:
                continue
            tweet_data = parse_tweet(tweet)
            if tweet_data:
                csv_out.writerow(tweet_data)
    
    done.close()


if __name__ == "__main__":
    main()
