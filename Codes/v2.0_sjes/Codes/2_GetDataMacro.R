#-------------------------------------------------------------------------------------
# A daily fever curve for the Swiss economy
#-------------------------------------------------------------------------------------
# Feel free to copy, adapt, and use this code for your own purposes at your own risk.
#
# Please cite: 
# Burri, Marc and Daniel Kaufmann (2020): "A daily fever curve for the
# Swiss economy", IRENE Working Paper No. 20-05, University of Neuchâtel,
# https://github.com/dankaufmann/f-curve
#
#-------------------------------------------------------------------------------------
# V 2.0
# TODO: Links BfS ändern mit jedem neuen Wert!
# TODO: Sometimes java error, not clear why
#-------------------------------------------------------------------------------------

# Packages and settings
rm(list = ls())
source("AllPackages.R")

# Comment out for server version
#Sys.setenv(X13_PATH = "C:/Users/daenu/Documents/R/win-library/3.6/x13binary/bin")
checkX13()

#-------------------------------------------------------------------------------------
# Download the data
#-------------------------------------------------------------------------------------
# Macro data, real-time, and indicators
download.file(url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12967657/master", destfile = "../Data/Employment.xlsx", mode="wb")
download.file(url = "https://datenservice.kof.ethz.ch/api/v1/public/ts?keys=kofbarometer&mime=xlsx", destfile = "../Data/KOFBaro.xlsx", mode="wb")
download.file(url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12647423/master", destfile = "../Data/UnempILO.xlsx", mode="wb")
download.file(url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12947871/master", destfile = "../Data/UnempSECO.xlsx", mode="wb")
download.file(url = "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/indikatoren/kss_publish.xls.download.xls/kss_publish.xls", destfile = "../Data/SentimentSECO.xls", mode="wb")
download.file(url = "https://www.bag.admin.ch/dam/bag/de/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-datengrundlage-lagebericht.xlsx.download.xlsx/200325_Datengrundlage_Grafiken_COVID-19-Bericht.xlsx", destfile = "../Data/Covid19Cases.xlsx", mode="wb")
download.file(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "../Data/Covid19CasesJH.xlsx", mode="wb")


# Get real time data bases (real and nominal and deflator)
Covid19     <- read.xlsx("../Data/Covid19cases.xlsx", sheetName = "COVID19 Zahlen", as.data.frame = TRUE, startRow = 7)
Cases    <- xts(Covid19[,2], order.by = Covid19[,1])
Hospital <- xts(Covid19[,4], order.by = Covid19[,1])
Deaths   <- xts(Covid19[,6], order.by = Covid19[,1])

Covid19JH     <- read.csv("../Data/Covid19casesJH.xlsx", sep = ",")
Covid19JH <- Covid19JH[Covid19JH[,2] == "Switzerland",]
Covid19JH <- Covid19JH[,-c(1,2,3,4)]
Dates     <- substring(colnames(Covid19JH), 2)
Dates     <- as.Date(Dates, format = "%m.%d.%y")
CasesJH   <- ts_diff(xts(t(Covid19JH[1,]), order.by = Dates))

# Get real time data bases (real and nominal and deflator)
RealTime     <- read.xlsx("../Data/RealTimeHistory/realtime_database.xlsx", sheetName = "GDP.CH", as.data.frame = TRUE, startRow = 11)
RealPub      <- read.xlsx("../Data/RealTimeHistory/realtime_database.xlsx", sheetName = "GDP.CH", as.data.frame = TRUE, startRow = 10, endRow = 10, header = FALSE)
RealTimeDefl <- read.xlsx("../Data/RealTimeHistory/realtime_database.xlsx", sheetName = "DEFL", as.data.frame = TRUE, startRow = 11)

DateQ <- str_split_fixed(as.character(RealTime[,1]), ":", 2)
Date  <- paste(DateQ[,1], "/", as.numeric(DateQ[,2])*3-2, "/1", sep = "")
Date  <- ymd(Date)

RealTime <- xts(RealTime[, -1], order.by = Date)
colnames(RealTime) <- as.character(as.Date(t(RealPub[1, -1]), "%d.%m.%Y"))
RealTimeDefl <- xts(RealTimeDefl[, -1], order.by = Date)
colnames(RealTimeDefl) <- as.character(as.Date(t(RealPub[1, -1]), "%d.%m.%Y"))
RealTimeNom <- RealTime*RealTimeDefl

Baro   <- read.xlsx("../Data/KOFBaro.xlsx", sheetIndex = 1, as.data.frame = TRUE)
Baro   <- xts(Baro[,2], order.by = as.Date(paste(Baro[,1], "-01", sep = "")))

# At the moment, not continuisly updated
Gap   <- read.xlsx("../Data/CycleHistory/SNBOutputGap.xlsx", sheetIndex = 1, startRow = 16, as.data.frame = TRUE)
Gap   <- xts(Gap[,2], order.by = Gap[,1])

Cycle   <- read.xlsx("../Data/CycleHistory/SNBCycle.xlsx", sheetIndex = 1, startRow = 16, as.data.frame = TRUE)
Cycle   <- xts(Cycle[,2], order.by = as.Date(paste(Cycle[,1], "-01", sep = "")))

OECD   <- ts_xts(ts_fred("CHELOLITONOSTSAM"))

Sent   <- read.xlsx("../Data/SentimentSECO.xls", sheetName = "Data", startRow = 4, as.data.frame = TRUE)
Sent   <- xts(Sent[,3], order.by = as.Date(paste(Sent[,1], Sent[,2], "1", sep = "/")))

CSent  <- ts_xts(ts_fred("LOCOCIORCHQ460S"))

ILODates    <- c(seq(as.Date("1991-04-01"), as.Date("2009-04-01"), by = "year"), seq(as.Date("2010-01-01"), as.Date("2050-12-01"), by = "quarter"))
ILO   <- read.xlsx("../Data/UnempILO.xlsx", sheetName = "Quartalswerte", as.data.frame = TRUE, startRow = 8, endRow = 8, header = FALSE)
ILO   <- ILO[,-1]
ILO   <- (ILO[!is.na(ILO)])
ILO   <- xts(as.matrix(ILO), order.by = ILODates[1:length(ILO)])

ILOSA <- xts(ma(ts_span(ILO, "2010-01-01"), 4), order.by = as.Date(index(ts_span(ILO, "2010-01-01"))))
ILOSA   <- ts_bind(ts_span(ILO, "1991-04-01", "2010-01-01"), ILOSA)
plot(ts_c(ILO, ILOSA))

# read all workbooks
filename <- "../Data/UnempSECO.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets[as.numeric(sheets)>1998], openxlsx::read.xlsx, xlsxFile=filename, startRow = 3)
names(SheetList) <- sheets[as.numeric(sheets)>1998]

# Assemble in time series
for(year in as.numeric(sheets[1]):1999){
  tempDates    <- seq(as.Date(paste(year, "-01-01", sep = "")), as.Date(paste(year, "-12-01", sep = "")), by = "month")
  temp   <- as.data.frame(SheetList[2020-year+1])
  temp   <- temp[1, ]
  
  toDel  <- (colnames(temp) == "X1" | colnames(temp) == "X2"  | colnames(temp) == "VK.2." | temp[1,] == "A"| temp[1,] == "Total")
  toDel[is.na(toDel)] <- FALSE
  temp   <- temp[1, !toDel]
  temp   <- (temp[!is.na(temp)])
  
  if (year == as.numeric(sheets[1])){
    SECO <- xts(as.matrix(temp), order.by = tempDates[1:length(temp)])
  }
  else{
    SECO <- ts_bind(SECO, xts(as.matrix(temp), order.by = tempDates[1:length(temp)]))
  }
}
SECOSA <- seas(ts_ts(SECO))
SECO   <- SECOSA$series$s11

EMPDates    <- seq(as.Date("1991-07-01"), as.Date("2050-12-01"), by = "quarter")
EMP   <- read.xlsx("../Data/Employment.xlsx", sheetIndex = 1, as.data.frame = TRUE, startRow = 8, endRow = 8, header = FALSE)
EMP   <- xts(t(as.matrix(EMP[1, 4:length(EMP)])), order.by = EMPDates[1:length(EMP[1, 4:length(EMP)])])
EMPSA <- seas(ts_ts(EMP))
EMP   <- EMPSA$series$s11


# Save macro data for comparison
save(list = c("EMP", "Baro", "SECO", "ILO", "ILOSA", "Gap", "Cycle", 
              "OECD", "Sent", "CSent", "Cases", "CasesJH", "Deaths", "Hospital",
              "RealTime", "RealTimeDefl", "RealTimeNom"), file = "../Data/MacroData.RData")

