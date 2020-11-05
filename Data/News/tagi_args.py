import pandas as pd
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import time
import sys, getopt
import os


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
        path = os.path.join(dirname, 'TA\\dom')
    else:
        # print(searchkey.split()[1])
        path = os.path.join(dirname, 'TA\\for')

    url = "https://disco-archive-search.publishing.tamedia.ch/horton-archive/?lang=de&tenant=TA"

    # Operating in headless mode
    opts = Options()
    opts.headless = True

    # Start Browser and search for articles in German containing words: Inflation, Arbeitsmarkt, Konjunktur
    browser = webdriver.Firefox(options=opts, executable_path=os.path.join(dirname, "geckodriver.exe"))
    browser.get(url)

    daterange = pd.date_range(start=strt, end=end)

    list_date = []
    list_text = []
    list_author = []
    list_title = []
    list_date_wp = []

    save = 1
    ii = 1
    for date in daterange:
        datestr = date.strftime("%d.%m.%Y")

        key = browser.find_element_by_xpath("//label[contains(text(),'Suchbegriffe')]")
        key.find_element_by_xpath("../div/input").clear()
        key.find_element_by_xpath("../div/input").send_keys(searchkey)

        beg = browser.find_element_by_xpath("//label[contains(text(),'Beginn')]")
        beg.find_element_by_xpath("../div/input").clear()
        beg.find_element_by_xpath("../div/input").send_keys(datestr)

        end = browser.find_element_by_xpath("//label[contains(text(),'Ende')]")
        end.find_element_by_xpath("../div/input").clear()
        end.find_element_by_xpath("../div/input").send_keys(datestr)
        
        time.sleep(2)

        such = browser.find_element_by_xpath("//button/span[contains(text(),'Suchen')]")
        such.click()

        while True:
            try:
                weit = browser.find_element_by_xpath("//button/span/span[contains(text(),'Resultat')]")
                weit.click()
            except:
                break

        try:
            time.sleep(3)
            parent = browser.find_elements_by_xpath("//div[@class='MuiCardContent-root']")
        except:
            try:
                time.sleep(3)
                parent = browser.find_elements_by_xpath("//div[@class='MuiCardContent-root']")
            except:
                print(datestr)
                continue


        for div in parent:
            title = div.find_element_by_xpath("h2").text
            author = div.find_element_by_xpath("h6").text
            text = div.find_element_by_xpath("p").text
            date_wp = div.find_element_by_xpath("span").text

            list_date.append(date.strftime("%d.%m.%Y"))
            list_text.append(text)
            list_title.append(title)
            list_author.append(author)
            list_date_wp.append(date_wp)

        # ii += 1

        #if abs(2000-date.year) % 100000 == 0:
        # if ii == 20:
        df = pd.DataFrame({'date': list_date, 'title': list_title, 'text': list_text, 'author': list_author, 'date_check': list_date_wp})
        df.to_excel(path+"/"+searchkey+"_"+str(date.strftime("%Y%m%d"))+".xlsx", index=False)
        save += 1
        list_date = []
        list_text = []
        list_author = []
        list_title = []
        list_date_wp = []


    # df = pd.DataFrame({'date': list_date, 'title': list_title, 'text': list_text, 'author': list_author, 'date_check': list_date_wp})
    # df.to_excel(path+"data_"+str(save)+".xlsx", index=False)

    browser.close()
    browser.quit()

if __name__ == "__main__":
   main(sys.argv)
