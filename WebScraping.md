# Requirements

The main scripts for the calculation of the f-curve are written in the programming language R. The necessary packages are listed in the file [AllPackages.R](./Codes/AllPackages.R) However, for the web scraping of the daily news articles (NZZ and TA) we switch to [Python](https://www.python.org) for technical reasons. Morover, for scraping the FuW we use [cURL](https://curl.haxx.se). This document explains the necessary steps to be able to run the web scraping scripts. The Python scripts and the curl command are designed to be executed by R. 

Note: These are instructions for Windows Users (with administrator privileges). For other OS it may work a little differently.


## Installation
### Python
- To be able to run the python scripts you need a working installation of [Python 3](https://www.python.org)
- With the packages [pandas](https://pandas.pydata.org/docs/), openpyxl and [selenium](https://selenium-python.readthedocs.io/installation.html) installed (including dependencies). Those can be installed by typing the following in the command line:
````bash
pip install pandas
pip install openpyxl
pip install selenium
````
- Note: We use Firefox ( [Geckodriver](https://github.com/mozilla/geckodriver/releases)) as browser for scraping. 

### cURL
- This command line tool should be installed by default on Windows 10 and other OS
- Otherwise you may find useful information under [https://curl.haxx.se](https://curl.haxx.se)

## Execution
The scripts are executed in R with the `system()` command. 

###  Python
The Python scripts use the following syntax:
````
tagi_args.py -k <searchkey> -s <startdate %Y-%m-%d> -e <enddate %Y-%m-%d>
nzz_args.py -k <searchkey> -s <startdate %Y-%m-%d> -e <enddate %Y-%m-%d>
````
where searchkey is the word you want to search for in the respective archive. Start and end date must be in format  %Y-%m-%d. From R the scripts are executed as:
```R
system(paste0('python  ..\\Data\\News\\nzz_args.py -k "', searchkey,'" -s "',startdate , '" -e "', enddate , '"')
```
The scripts then save an Excel file for each keyword and date (the destination folder must be created manually) which is subsequently processed by R. 

###  cURL
Since cUrl is a command line tool we simply run the following code snippet to save the most recent news in a text file. Again this text file is subsequently processed by R.
````R
  system(paste0('curl "https://www.fuw.ch/wp-content/plugins/fuw-list/api/ajax.php" -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:75.0) Gecko/20100101 Firefox/75.0" -H "Accept: */*" -H "Accept-Language: de,en-US;q=0.7,en;q=0.3" -H "Content-Type: application/x-www-form-urlencoded; charset=UTF-8" -H "X-Requested-With: XMLHttpRequest" -H "Origin: https://www.fuw.ch" -H "Connection: keep-alive" -H "Referer: https://www.fuw.ch/markte/makro/" -H "Cookie: __cfduid=d8695fc49dc4fa7cb3042d9e31b42d4ff1588857885; fuwStats2020=45A5QbY4rzxxU; POPUPCHECK=1588944296151; _ga=GA1.2.1748096360.1588857897; _gid=GA1.2.874599476.1588857897; _gat_main=1; _gat_g=1; _gat_h=1; _gcl_au=1.1.255979795.1588857898; _gat_UA-58327930-30=1; _fbp=fb.1.1588857898454.1896769323; _parsely_session={"%"22sid"%"22:1"%"2C"%"22surl"%"22:"%"22https://www.fuw.ch/"%"22"%"2C"%"22sref"%"22:"%"22https://www.google.com/"%"22"%"2C"%"22sts"%"22:1588857899212"%"2C"%"22slts"%"22:0}; dakt_2_uuid=71a0d9a81ee50f88734c66be98d23a1e; dakt_2_uuid_ts=1588857899328; dakt_2_session_id=367922baff27ee84ac6e40f4ce0ce5dc; _parsely_visitor={"%"22id"%"22:"%"22pid=8a2cb26c1f41d7b807d841ad77d88f9b"%"22"%"2C"%"22session_count"%"22:1"%"2C"%"22last_session_ts"%"22:1588857899212}; __gads=ID=64e2249ac7a0219b:T=1588857897:S=ALNI_Ma_DcuF0BRgdFUd9ot7LGh6ZZUpJg" -H "TE: Trailers" --data "query=category&id=33&offset=0&count=100&excludeCategory"%"5B"%"5D=1229&listId=list-5eb40c2a14444579&listStart=0&listOrderBy=date&listIncludeDraftsAsPreview=0&listTemplate=default&listPage=1&listPages=2&listMoreButton=1&listMoreAutoload=0&listMoreLink=&articleDate=1&articleTime=0&articleTimeTodayFormat=0&articleHighlightDate=0&articleCategories=1&articleCategoriesLinked=1&articleBookmark=1&articleImage=0&articleKicker=1&articleLead=1&articleAuthor=1&articleRanking=1&articleTeaserMarkerDisplay"%"5B"%"5D=7&articleTeaserMarkerDisplay"%"5B"%"5D=8&articleTeaserMarkerDisplay"%"5B"%"5D=9&articleTags=1&articleLinkToBlank=1&articleLinkFreeKey=0&amp=0&jsonLd=0&insertAds=0" -o ',path,'/makro_',enddate,'.txt'))

````
Note: we do this twice. Once for news in category *Makro* and once for news in category *Unternehmen*. 
