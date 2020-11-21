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
# - Outlier detection added for News
# - We use archive and Web data for Tagi because archive missing for some parts of sample
#-------------------------------------------------------------------------------------

# Packages and settings
#rm(list = ls())
source("AllPackages.R")

endDate     <- Sys.Date()
updateNews  <- F # Choose whether you want to update news (takes up to 20 min)

#-------------------------------------------------------------------------------------
# Download the data
#-------------------------------------------------------------------------------------
# Macro data (only GDP and Covid Cases, rest loaded in separate file) and trendecon
download.file(url = "https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/VIP%20Quartalssch%C3%A4tzungen/qna_p_csa.xls.download.xls/qna_p_csa.xls", destfile = "../Data/PIBSuisse.xls", mode="wb")
download.file(url = "https://raw.githubusercontent.com/trendecon/data/master/data/ch/trendecon_sa.csv", destfile = "../Data/TrendEcon.csv", mode="wb")
download.file(url = "https://www.bag.admin.ch/dam/bag/de/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-datengrundlage-lagebericht.xlsx.download.xlsx/200325_Datengrundlage_Grafiken_COVID-19-Bericht.xlsx", destfile = "../Data/Covid19Cases.xlsx", mode="wb")
download.file(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "../Data/Covid19CasesJH.xlsx", mode="wb")
download.file(url = "https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/indikatoren/wwa_publish.xls.download.xls/wwa_publish.xls", destfile = "../Data/SECOWEA.xls", mode="wb")
download.file(url = "https://datenservice.kof.ethz.ch/api/v1/public/ts?keys=kofbarometer&mime=xlsx", destfile = "../Data/KOFBaro.xlsx", mode="wb")

# Get news indicators
if (updateNews) {
  # Scrape News from Web and save in all.RData file
  # Note:
  #  - can take up to 30 min.
  #  - Depends on Python (including working selenium) and cURL
  updateNewsIndicator()
}

load(file="../Data/News/all.RData")


News.CH <- df_all_ch %>%
  select("time", "mean") %>%
  ts_xts() %>%
  ts_span(start = "2000-01-01", end = endDate)


News.FOR <- df_all_int %>%
  select("time", "mean") %>%
  ts_xts() %>%
  ts_span(start = "2000-01-01", end = endDate)


# Clear outliers
for (n in c("News.CH", "News.FOR")) {
  myVar = get(n)
  assign(paste(n, "_orig", sep = ""), myVar)
  
  myStd       <- sqrt(var(myVar, na.rm = TRUE))
  myMean      <- mean(myVar, na.rm = TRUE)
  outl1        <- myVar>as.numeric(myMean)+3*as.numeric(myStd)
  outl2        <- myVar<as.numeric(myMean)-3*as.numeric(myStd)
  myVar[(outl1|outl2)] <- NA
  
  assign(n, myVar)
  
}

# Get Covid Cases
Covid19     <- xlsx::read.xlsx("../Data/Covid19cases.xlsx", sheetName = "COVID19 Zahlen", as.data.frame = TRUE, startRow = 7)
Cases    <- xts(Covid19[,2], order.by = Covid19[,1])
Hospital <- xts(Covid19[,4], order.by = Covid19[,1])
Deaths   <- xts(Covid19[,6], order.by = Covid19[,1])

Covid19JH     <- read.csv("../Data/Covid19casesJH.xlsx", sep = ",")
Covid19JH <- Covid19JH[Covid19JH[,2] == "Switzerland",]
Covid19JH <- Covid19JH[,-c(1,2,3,4)]
Dates     <- substring(colnames(Covid19JH), 2)
Dates     <- as.Date(Dates, format = "%m.%d.%y")
CasesJH   <- ts_diff(xts(t(Covid19JH[1,]), order.by = Dates))

# Get Indicators
KOF <- read.xlsx("../Data/KOFBaro.xlsx", sheetName = "Sheet1", as.data.frame = TRUE, startRow = 1)
Baro <- xts(KOF$kofbarometer, order.by = as.Date(paste0(KOF$date, "-01")))

SECO <- read.xlsx("../Data/SECOWEA.xls", sheetName = "Data", as.data.frame = TRUE, startRow = 4)
WEA <- xts(SECO[,3], order.by = as.Date(paste0(SECO[,1], "-", SECO[,2], "-1"), format = "%Y-%U-%u"))

GDP         <- read.xlsx("../Data/PIBSuisse.xls", sheetName = "real_q", as.data.frame = TRUE, startRow = 11)
GDP         <- (xts(GDP[!is.na(GDP[,3]),3], order.by = as.Date(paste(GDP[!is.na(GDP[,1]),1], GDP[!is.na(GDP[,2]),2]*3-2, "01", sep = "-"))))
GDPDefl     <- read.xlsx("../Data/PIBSuisse.xls", sheetName = "defl_q", as.data.frame = TRUE, startRow = 11)
GDPDefl         <- (xts(GDPDefl[!is.na(GDPDefl[,3]),3], order.by = as.Date(paste(GDPDefl[!is.na(GDPDefl[,1]),1], GDPDefl[!is.na(GDPDefl[,2]),2]*3-2, "01", sep = "-"))))
#GDPDefl     <- (xts(GDPDefl[,3], order.by = as.Date(paste(GDPDefl[,1], GDPDefl[,2]*3-2, "01", sep = "-"))))
NGDP        <- GDP*GDPDefl

Tecon   <- read.csv("../Data/TrendEcon.csv")
Tecon   <- xts(Tecon[,2], order.by = as.Date(Tecon[,1]))

# Financial market variables
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_gov_y.csv", destfile = "../Data/ObligationsConf.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_dom_non_gov_rating_sbi_y.csv", destfile = "../Data/ObligationsEnt.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/h_vsmi_30.csv", destfile = "../Data/VIX.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_foreign_gov_y.csv", destfile = "../Data/ForGov.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_foreign_cor_y.csv", destfile = "../Data/ForCorp.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_for_rating_sbi_y.csv", destfile = "../Data/ForShort.csv", mode="wb")
download.file(url = "https://www.bundesbank.de/statistic-rmi/StatisticDownload?tsId=BBK01.WT1010&its_csvFormat=de&its_fileFormat=csv&mode=its", destfile = "../Data/GermanBondYield.csv", mode="wb")
download.file(url = "http://sdw.ecb.europa.eu/quickviewexport.do;jsessionid=62044D6532AB70D16C184E5A8FFADEEC?SERIES_KEY=165.YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y&type=csv", destfile = "../Data/EuroShortRate.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsmi.csv", destfile = "../Data/SMI.csv", mode="wb")

# German government bond yields
Gov10.DEU     <- read.csv("../Data/GermanBondYield.csv", sep = ";", skip = 5,  na.strings = "Kein Wert vorhanden", stringsAsFactors = FALSE, )
Gov10.DEU     <- Gov10.DEU[,1:2]
Gov10.DEU[,2] <- as.numeric(gsub(",", ".", gsub("\\.", "", Gov10.DEU[,2])))
Gov10.DEU <- Gov10.DEU[!is.na(Gov10.DEU[,2]),]
Gov10.DEU <- xts(Gov10.DEU[,2], order.by = as.Date(Gov10.DEU[,1]))
Gov1.EUR <-  read.csv("../Data/EuroShortRate.csv", sep = ",", skip = 4, stringsAsFactors = FALSE, )
Gov1.EUR <- xts(Gov1.EUR[,2], order.by = as.Date(Gov1.EUR[,1]))
LIB1.EUR <- ts_ts(ts_fred('EUR12MD156N'))

# Volatility CH
VIX.CH  <- read.csv("../Data/VIX.csv", sep = ";", skip = 1)
VIX.CH  <- xts(VIX.CH[,3], order.by = dmy(VIX.CH[,1]))

# Bond yields history SNB
BondHistory <- read.xlsx("../Data/BondsHistory/SNBBondHistory.xlsx", startRow = 17, sheetName = "Report")
Gov1.CH.h   <- xts(BondHistory$EID1, order.by = as.Date(BondHistory$Date))
Gov2.CH.h   <- xts(BondHistory$EID2, order.by = as.Date(BondHistory$Date))
Gov8.CH.h   <- xts(BondHistory$EID8, order.by = as.Date(BondHistory$Date))
Gov9.CH.h   <- xts(BondHistory$EID8, order.by = as.Date(BondHistory$Date))
Gov10.CH.h  <- xts(BondHistory$EID10, order.by = as.Date(BondHistory$Date))
Bank8.CH.h  <- xts(BondHistory$BANK8, order.by = as.Date(BondHistory$Date))
Ind8.CH.h   <- xts(BondHistory$IND8, order.by = as.Date(BondHistory$Date))
Gov10.DEU.h <- xts(BondHistory$DEU10, order.by = as.Date(BondHistory$Date))
ForA8.h     <- xts(BondHistory$FORA, order.by = as.Date(BondHistory$Date))
ForAA8.h    <- xts(BondHistory$FORAA, order.by = as.Date(BondHistory$Date))
ForAAA8.h  <- xts(BondHistory$FORAAA, order.by = as.Date(BondHistory$Date))

# SMI
SMI  <- read.csv("../Data/SMI.csv", sep = ";", skip = 4)
SMI  <- xts(SMI[, 2], order.by = dmy(SMI[,1]))

# Bonds raw data SIX
Gov         <- read.csv("../Data/ObligationsConf.csv", sep = ";", skip = 4)
GovLab      <- read.csv("../Data/ObligationsConf.csv", sep = ";", nrows = 3)
NonGov      <- read.csv("../Data/ObligationsEnt.csv", sep = ";", skip = 4)
NonGovLab   <- read.csv("../Data/ObligationsEnt.csv", sep = ";", nrows = 3)
ForGov      <- read.csv("../Data/ForGov.csv", sep = ";", skip = 4)
ForGovLab   <- read.csv("../Data/ForGov.csv", sep = ";", nrows = 3)
ForCorp     <- read.csv("../Data/ForCorp.csv", sep = ";", skip = 4)
ForCorpLab  <- read.csv("../Data/ForCorp.csv", sep = ";", nrows = 3)
ForShort    <- read.csv("../Data/ForShort.csv", sep = ";", skip = 4)
ForShortLab <- read.csv("../Data/ForShort.csv", sep = ";", nrows = 3)

# More recent data from SIX
Gov2.CH     <- xts(Gov[,GovLab[2,] == "SBI Dom Gov 1-3 Y"], order.by = dmy(Gov[,1]))
Gov8.CH     <- xts(Gov[,GovLab[2,] == "SBI Dom Gov 7-10 Y"], order.by = dmy(Gov[,1]))
AAA_BBB2.CH <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-BBB 1-3 Y"], order.by = dmy(NonGov[,1]))
AAA_BBB8.CH <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-BBB 7-10 Y"], order.by = dmy(NonGov[,1]))
AAA_AA2.CH  <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-AA 1-3 Y"], order.by = dmy(NonGov[,1]))
AAA_AA8.CH  <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-AA 7-10 Y"], order.by = dmy(NonGov[,1]))
AAA_A8.CH   <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-AA 7-10 Y"], order.by = dmy(NonGov[,1]))
AAA_A2.CH   <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-A 1-3 Y"], order.by = dmy(NonGov[,1]))
AAA_A10.CH  <- xts(NonGov[,NonGovLab[2,] == "SBI Dom Non-Gov AAA-A 7-10 Y"], order.by = dmy(NonGov[,1]))
ForAAA_BBB  <- xts(ForCorp[,ForCorpLab[2,] == "SBI For Corp AAA-BBB Y"], order.by = dmy(ForCorp[,1]))
ForAAA_A    <- xts(ForCorp[,ForCorpLab[2,] == "SBI For Corp AAA-A Y"], order.by = dmy(ForCorp[,1]))
ForAAA_AA   <- xts(ForCorp[,ForCorpLab[2,] == "SBI For Corp AAA-AA Y"], order.by = dmy(ForCorp[,1]))
ForAAA_BBB2 <- xts(ForShort[,ForShortLab[2,] == "SBI For AAA-BBB 1-3 Y"], order.by = dmy(ForShort[,1]))

# Link 2Y history with 1-3 year current data
Gov1.EUR.l <- ts_bind(ts_span(LIB1.EUR, ts_summary(LIB1.EUR)$start, ts_summary(Gov1.EUR)$start), Gov1.EUR)
p <- ts_ggplot(
  `Spliced data`   = Gov1.EUR.l,
  `ECB data`      = Gov1.EUR,
  `LIBOR data`    = LIB1.EUR,
  title = "Short-term euro bond yields"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/LinkedData/Short.EUR.pdf", width = figwidth, height = figheight)

# Link 2Y history with 1-3 year current data
Gov2.CH.l <- ts_bind(ts_span(Gov2.CH.h, ts_summary(Gov2.CH.h)$start, ts_summary(Gov2.CH)$start), Gov2.CH)
p <- ts_ggplot(
  `Spliced data`   = Gov2.CH.l,
  `SIX Data (1-3Y)` = Gov2.CH,
  `SNB Data (2Y)`   = Gov2.CH.h,
  title = "Short-term confederation bond yields"
)
p <- ggLayout(p)
ggsave(filename = "../Results/LinkedData/GovShort.CH.pdf", width = figwidth, height = figheight)

# Link 8Y history with 7-10 year current data
Gov8.CH.l <- ts_bind(ts_span(Gov8.CH.h, ts_summary(Gov8.CH.h)$start, ts_summary(Gov8.CH)$start), Gov8.CH)
p <- ts_ggplot(
  `Spliced data`      = Gov8.CH.l,
  `SIX Data (7-10Y)` = Gov8.CH,
  `SNB Data (8Y)`    = Gov8.CH.h,
  title = "Long-term confederation bond yields"
)
p <- ggLayout(p)
ggsave(filename = "../Results/LinkedData/GovLong.CH.pdf", width = figwidth, height = figheight)

# Link 8Y history banks with average between AA-AAA and A-AAA corporate bonds
AAA8.CH.l <- ts_bind(ts_span(Bank8.CH.h, ts_summary(Bank8.CH.h)$start, ts_summary(AAA_AA8.CH)$start), (AAA_AA8.CH+AAA_A8.CH)/2)
p <- ts_ggplot(
  `Spliced data`                 = AAA8.CH.l,
  `SIX Data (incl. A 7-10Y)`    = AAA_A8.CH,
  `SIX Data (incl. AA 7-10Y)`   = AAA_AA8.CH,
  `SNB Data Banks (8Y)`         = Bank8.CH.h,
  title = "Long-term corporate bond yields"
)
p <- ggLayout(p)
ggsave(filename = "../Results/LinkedData/CorpLong1.CH.pdf", width = figwidth, height = figheight)

# Link 8Y history manufacturing with adjusted AAA-BBB yield index (use same mean and volatility
Ind8.CH.h.adj <- (Ind8.CH.h-mean(ts_span(Ind8.CH.h, ts_summary(AAA_BBB8.CH)$start), na.rm = TRUE)[1])/sqrt(var(ts_span(Ind8.CH.h, ts_summary(AAA_BBB8.CH)$start), na.rm=TRUE))[1]
Ind8.CH.h.adj <- Ind8.CH.h.adj*sqrt(var(AAA_BBB8.CH, na.rm=TRUE))[1] + mean(AAA_BBB8.CH, na.rm = TRUE)[1]
BBB8.CH.l <- ts_bind(ts_span(Ind8.CH.h.adj, ts_summary(Ind8.CH.h.adj)$start, ts_summary(AAA_BBB8.CH)$start), AAA_BBB8.CH)
p <- ts_ggplot(
  `Spliced data`                 = BBB8.CH.l,
  `SIX Data (incl. BBB 7-10Y)`   = AAA_BBB8.CH,
  `SNB Data Manufact (adj. 8Y)`      = Ind8.CH.h.adj ,
  title = "Long-term corporate bond yields"
)
p <- ggLayout(p)
ggsave(filename = "../Results/LinkedData/CorpLong2.CH.pdf", width = figwidth, height = figheight)

# Link 8Y bonds of foreign corporations
# Take the average from SIX data and link with data of SNB
# Not clear that same credit rating and/or maturity
ForCorp.l <- ts_bind(ts_span(ForAAA8.h, ts_summary(ForAAA8.h)$start, ts_summary(ForAAA_A)$start), (ForAAA_A+ForAAA_AA+ForAAA_BBB)/3)
p <- ts_ggplot(
  `Spliced data`   = ForCorp.l,
  `SIX (AAA-A)`    = ForAAA_A,
  `SIX (AAA-AA)`   = ForAAA_AA,
  `SIX (AAA-BBB)`  = ForAAA_BBB,
  `SNB (AAA 8Y)`           = ForAAA8.h,
  title = "Long-term foreign corporate bond yields"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/LinkedData/ForCorpLong.CH.pdf", width = figwidth, height = figheight)

# Construct final data set
IRDIFF.CH   <- Gov2.CH.l - Gov1.EUR.l
TS.CH       <- Gov8.CH.l - Gov2.CH.l
RP.CH      <- AAA8.CH.l - Gov8.CH.l
#RP2.CH      <- BBB8.CH.l - Gov8.CH.l
RPShort.CH  <- AAA_BBB2.CH - Gov2.CH.l
RPShort.FOR <- ForAAA_AA - Gov2.CH.l 
RP.FOR      <- ForCorp.l - Gov8.CH.l  
TS.EUR      <- Gov10.DEU - Gov1.EUR.l
TS.US       <- ts_xts(ts_fred("T10Y2Y"))
VIX.US      <- ts_xts(ts_fred("VIXCLS"))

# Check start and end dates
ts_summary(IRDIFF.CH)
ts_summary(TS.CH)
ts_summary(RP.CH)
ts_summary(RPShort.CH)
ts_summary(RPShort.FOR)
ts_summary(RP.FOR)
ts_summary(TS.EUR)
ts_summary(TS.US)
ts_summary(VIX.US)
ts_summary(VIX.CH)

ts_summary(News.FOR)
ts_summary(News.CH)

# Export the data (only those series that work well!)
Indicators <- ts_c(TS.CH, RP.CH, RPShort.CH, VIX.CH, IRDIFF.CH, News.CH,
                   News.FOR, RP.FOR, RPShort.FOR, TS.US, VIX.US, TS.EUR,
                   Tecon, SMI)

# Save indicators for f-curve
save(list = c("GDP", "NGDP", "GDPDefl", "Cases", "CasesJH", "Deaths", "Hospital", "Indicators", "Baro", "WEA"), file = "../Data/IndicatorData.RData")

