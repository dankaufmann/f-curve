import getopt
import sys
import time
import math
import string
import os

import pandas as pd
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
from selenium.common.exceptions import WebDriverException
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC



def main(argumentlist):
    nrarguments = (len(argumentlist) - 1)/2
    argumentlist = argumentlist[1:]
    try:
        short_options = "hk:s:e:"
        long_options = ["help", "searchey=", "startdate=", "enddate="]
        arguments, values = getopt.getopt(argumentlist,short_options,long_options)
    except getopt.GetoptError:
        print('tagi_args.py -k <searchkey> -s <startdate %d.%m-%Y> -e <enddate %d.%m-%Y>')
        sys.exit(2)

    for current_argument, current_value in arguments:
        if current_argument in ("-k", "--searchkey"):
            searchkey = current_value
        elif current_argument in ("-h", "--help") or nrarguments != 3:
            print('tagi_args.py -k <searchkey> -s <startdate %Y-%m-%d> -e <enddate %Y-%m-%d>')
            sys.exit()
        elif current_argument in ("-s", "--startdate"):
            strt = current_value
        elif current_argument in ("-e", "--enddate"):
            end = current_value

    if len(searchkey.split()) != 2:
        sys.exit()
        
    dirname = os.path.dirname(__file__)
    
    if "schweiz" in searchkey.split()[1]:
        # print(searchkey.split()[1])
        path = os.path.join(dirname, 'NZZ\\dom')
    else:
        # print(searchkey.split()[1])
        path = os.path.join(dirname, 'NZZ\\for')

    url = "https://zeitungsarchiv.nzz.ch/#archive"

    # Operating in headless mode
    opts = Options()
    opts.headless = True

    # Start Browser and search for articles in German containing words: Inflation, Arbeitsmarkt, Konjunktur
    browser = webdriver.Firefox(options=opts, executable_path=os.path.join(dirname, "geckodriver.exe"))
    browser.get(url)
    
    time.sleep(5)

    daterange = pd.date_range(start=strt, end=end)

    list_date = []
    list_text = []
    list_title = []
    
    translator = str.maketrans('', '', string.punctuation)

    for date in daterange:
        datestr = date.strftime("%d.%m.%Y")

        key = browser.find_element_by_class_name("fup-archive-query-input")
        key.clear()
        key.send_keys(searchkey)
        
        time.sleep(1)

        beg = browser.find_element_by_class_name("fup-s-date-start")
        beg.clear()
        beg.send_keys(datestr)
        
        time.sleep(1)

        end = browser.find_element_by_class_name("fup-s-date-end")
        end.clear()
        end.send_keys(datestr)
        
        time.sleep(1)

        such = browser.find_element_by_class_name("fup-s-exec-search")
        such.click()
        
        time.sleep(10)

        nr = browser.find_element_by_class_name("fup-archive-result-hits").text
        nr = int(nr.split()[0])
        nr_clicks = math.ceil(nr/50)

        for ii in range(nr_clicks):
            try:
                actions = ActionChains(browser)
                element = browser.find_elements_by_class_name("fup-common-scroll")[1]
                length = element.size["height"]
                browser.execute_script("arguments[0].scrollIntoView();", element);
                actions.move_to_element_with_offset(element, 4, length - 5)
                actions.click()
                actions.perform()
                WebDriverWait(browser,5).until_not(EC.invisibility_of_element_located((By.CLASS_NAME, "fup-archive-result-loader")))
                time.sleep(2)
            except Exception as e:
                # print(e)
                break

        parent = browser.find_elements_by_class_name("fup-archive-result-item")[1:]

        for div in parent:
            title = div.find_element_by_class_name("fup-archive-result-item-article-title").get_attribute('innerHTML')
            text = div.find_element_by_class_name("fup-archive-result-item-article-text").get_attribute('innerHTML')
            list_date.append(date.strftime("%d.%m.%Y"))
            list_text.append(text)
            list_title.append(title)
            
        fname = searchkey.translate(translator)

        df = pd.DataFrame({'date': list_date, 'title': list_title, 'text': list_text})
        df.to_excel(path+"/"+fname+"_"+str(date.strftime("%Y%m%d"))+".xlsx", index=False)
        list_date = []
        list_text = []
        list_title = []
        time.sleep(2)


    browser.close()
    browser.quit()

if __name__ == "__main__":
   main(sys.argv)
