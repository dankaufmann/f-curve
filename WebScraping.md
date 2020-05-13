# Requirements

The main scripts for the calculation of the f-curve are written in the programming language R. The necessary packages are listed in the file [AllPackages.R](./Codes/AllPackages.R) However, for the web scraping of the daily news articles (NZZ and TA) we switch to [Python](https://www.python.org) for technical reasons. Morover, for scraping the FuW we use [cURL](https://curl.haxx.se). This document explains the necessary steps to be able to run the web scraping scripts. The Python scripts and the curl command are designed to be executed by R. 


## Installation
### Python
- To be able to run the python scripts you need a working installation of [Python 3](https://www.python.org)
- With the packages [pandas](https://pandas.pydata.org/docs/) and [selenium](https://selenium-python.readthedocs.io/installation.html) installed (including dependencies). Those can be installed by typing the following in the command line:
````bash
pip install pandas
pip install selenium
````
- We use Firefox ( [Geckodriver](https://github.com/mozilla/geckodriver/releases)) as browser for scraping. Make sure to install the right [driver](https://selenium-python.readthedocs.io/installation.html#drivers)
- The operating system must be able to find the driver. Either add it to the systems [PATH variable](https://www.architectryan.com/2018/03/17/add-to-the-path-on-windows-10/) or start the browser with the additional option "executable_path":

```python
browser = webdriver.Firefox(options=opts, executable_path=r'your\path\geckodriver.exe')
```

### cURL
- This command line tool should be installed by default on Windows 10 and other OS
- Otherwise you may find useful information under [https://curl.haxx.se](https://curl.haxx.se)

## Execution
