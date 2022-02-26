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
# - We use only a 2 day MA for the news because we drop outliers in the data part
# - We use archive and Web data for Tagi because archive missing for some parts of sample
#-------------------------------------------------------------------------------------

# Packages and settings
#rm(list = ls())
source("AllPackages.R")
normStart <- as.Date("1999-06-01")   # Use some data before because we shift term spread forward
startDate <- as.Date("2000-01-01")

endDate     <- Sys.Date()
noMANews      <- 2      # Number of days uncentered moving average for news (otherwise very volatile) do that in second step...
noMA          <- 7      # Working days for moving average
leadTS        <- 0.5    # Lead of term-spread on the business cycle (in years)
minObs        <- 5      # Min. number of observations

# Choose Which indicators to use
# Indicators <- ts_c(TS.CH, RP.CH, RPShort.CH, VIX.CH, IRDIFF.CH, News.CH,
#                    News.FOR, RP.FOR, RPShort.FOR, TS.US, VIX.US, TS.EUR,
#                    Tecon, SMI)
whichInd   <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
whichNews  <- c(6, 7)
whichFin   <- c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12)
indexDom   <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0,  0, 0)    
indexDom   <- indexDom == 1  # Needs to be TRUE/FALSE

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")
print(colnames(Indicators))

# Estimate the factor
Results <- computeFactors(Indicators, leadTS, noMANews, normStart, startDate, endDate, whichInd, indexDom, minObs)
fc <- ts_xts(Results[[1]]$fc)
fc_for <- ts_xts(Results[[1]]$fc_for)
fc_dom <- ts_xts(Results[[1]]$fc_dom)
fc_res <- ts_xts(Results[[1]]$fc_res)
lastObsDate <- Results[[2]]

# Make sure that counter-cyclical
signInd   <- sign(as.numeric(fc["2009-01-20"])-as.numeric(fc["2007-12-13",]))
fc        <- fc*signInd
fc_dom    <- fc_dom*signInd
fc_for    <- fc_for*signInd
fc_res    <- fc_res*signInd

# Smooth with moving average 
fc_s  <- rollapply(fc, noMA, mean, na.rm = TRUE)

# Estimate the factor only with news data (does not compute decomposition)
Results <- computeFactors(Indicators, leadTS, noMANews, normStart, startDate, endDate, whichNews, TRUE, 1)
fc_news <- ts_xts(Results[[1]]$fc)
signInd   <- sign(as.numeric(fc_news["2009-01-20"])-as.numeric(fc_news["2007-12-13",]))
fc_news   <- fc_news*signInd

# Estimate the factor only with financial data (does not compute decomposition)
Results <- computeFactors(Indicators, leadTS, noMANews, normStart, startDate, endDate, whichFin, TRUE, minObs)
fc_fin  <- ts_xts(Results[[1]]$fc)
signInd <- sign(as.numeric(fc_fin["2009-01-20"])-as.numeric(fc_fin["2007-12-13",]))
fc_fin  <- fc_fin*signInd

# Main chart of curve
DataCor <- as.data.frame(ts_c(ts_pc(GDP), ts_frequency(fc, to = "quarter", aggregate = "mean", na.rm = TRUE)))
Correl  <- cor(DataCor, use = "na.or.complete")[1, 2]
p <- ts_ggplot(
  # `Baseline, five-day moving-av., inv. scale`  = -fc_s ,
  `f-curve, inverse, rescaled`                         = -fc/3+.4,
  `GDP growth`                           = ts_span(ts_pc(GDP), startDate),
  title = paste("Last observation: ", lastObsDate, "", sep = "")
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -6)
p <- addCorr(p, -Correl, "2015-01-01", 2)
p
ggsave(filename = "../Results/MainGDP.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/MainGDP.png", width = figwidth, height = figheight)

# Yearly frequency
DataCor <- as.data.frame(ts_c(ts_pc(ts_frequency(GDP, to = "year", aggregate = "sum", na.rm = FALSE)), ts_frequency(fc, to = "year", aggregate = "mean", na.rm = TRUE)))
Correl  <- cor(DataCor, use = "na.or.complete")[1, 2]
p <- ts_ggplot(
  # `Baseline, five-day moving-av., inv. scale`  = -fc_s ,
  `f-curve, inverse, rescaled`             = -fc+1.5,
  `GDP growth (annual)`                = ts_span(ts_pc(ts_frequency(GDP, to = "year", aggregate = "sum", na.rm = FALSE)), startDate),
  title = "Annual GDP growth and fever curve"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -8)
p <- addCorr(p, -Correl, "2015-01-01", 4)
p
ggsave(filename = "../Results/GDPAnnual.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/GDPAnnual.png", width = figwidth, height = figheight)

# Main chart of curve
ShortLines <- c("2020-03-16",  "2020-04-16", "2020-04-30", "2020-05-27", "2020-10-28", "2020-11-09", "2020-12-19", "2021-02-17", "2021-04-14")
ShortLabels <- c("Covid-19 lockdown", "Easing lockdown ann. (phase I)", "Easing lockdown ann. (phase II)", "Easing lockdown ann. (phase III)", "National restrictions ann.", "Successful vaccine study", "Tighter national restrictions ann.", "Loosening restrictions ann.", "Loosening restrictions ann.")
p <- ts_ggplot(
  ` f-curve, seven-day moving-average`  = ts_span(fc_s, "2020-02-01"),
  `f-curve`                         = ts_span(fc, "2020-02-01"),
  title = paste("Last observation: ", lastObsDate, "", sep = "")
)
p <- ggLayout(p)
p <- ggColor3(p)
p <- addLines(p, ShortLines, ShortLabels, -8)
p <- p + scale_x_date(labels =  scales::date_format("%b %Y"))
ggsave(filename = "../Results/MainGDPShort.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/MainGDPShort.png", width = figwidth, height = figheight)
p

DecompData = data.frame(as.Date(index(fc)), fc_dom, fc_for, fc_res)
colnames(DecompData) = c("Date", "Domestic", "Foreign", "Rest")
DecompData <- melt(DecompData,id.vars = "Date") 
p <- ggplot(DecompData, aes(x = Date, y = value,fill=variable)) + geom_bar(stat='identity')
p <- ggLayout(p)
p <- addLines(p, myLines, myLabels, 3)
p
ggsave(filename = "../Results/Decomposition.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/Decomposition.png", width = figwidth, height = figheight)

p <- ggplot(subset(DecompData, Date>"2020-02-01"), aes(x = Date, y = value,fill=variable)) + geom_bar(stat='identity')
p <- ggLayout(p)
#p <- addLines(p, ShortLines, ShortLabels, -8)
p <- p + scale_x_date(labels =  scales::date_format("%b %Y"))
p
ggsave(filename = "../Results/DecompositionShort.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/DecompositionShort.png", width = figwidth, height = figheight)


# Comparison with other indicators
p <- ts_ggplot(
  `f-curve, inverse (daily)` = ts_span(-normalize(fc), "2020-01-01"), 
  ` SECO WEA (weekly)` = ts_span(normalize(WEA), "2020-01-01"),
  ` KOF Barometer (monthly)` = ts_span(normalize(Baro), "2020-01-01"), 
  title = "Comparison with other indicators",
  subtitle = "All indicators are normalized to be pro-cyclical, have mean 0, and variance 1"
)
p <- ggLayout(p)
p <- p + scale_x_date(labels =  scales::date_format("%b %Y"))
p <- ggColor3(p)
#p <- addLines(p, myLines, myLabels, -18)
p
ggsave(filename = "../Results/ComparisonOther.png", width = figwidth, height = figheight)
ggsave(filename = "../Results/ComparisonOther.pdf", width = figwidth, height = figheight)

# Comparison with other indicators
p <- ts_ggplot(
  `f-curve, inverse (daily)` = ts_span(-normalize(fc), "2000-01-01"), 
  ` SECO WEA (weekly)` = ts_span(normalize(WEA), "2000-01-01"),
  ` KOF Barometer (monthly)` = ts_span(normalize(Baro), "2000-01-01"), 
  title = "Comparison with other indicators",
  subtitle = "All indicators are normalized to be pro-cyclical, have mean 0, and variance 1"
)
p <- ggLayout(p)
p <- ggColor3(p)
#p <- addLines(p, myLines, myLabels, -18)
p
ggsave(filename = "../Results/ComparisonOtherLong.png", width = figwidth, height = figheight)
ggsave(filename = "../Results/ComparisonOtherLong.pdf", width = figwidth, height = figheight)

#load(file="../Data/MacroData.RData")

# Remove last observation (somehow, FOPH does not report all cases and revises later on)
# But keep it later on if it is confirmed
Cases[ts_summary(Cases)$end-2] = NA
Cases[ts_summary(Cases)$end-1] = NA
Cases[ts_summary(Cases)$end] = NA
Cases <- na.omit(Cases)
Deaths[ts_summary(Deaths)$end-2] = NA
Deaths[ts_summary(Deaths)$end-1] = NA
Deaths[ts_summary(Deaths)$end] = NA
Deaths <- na.omit(Deaths)
Hospital[ts_summary(Hospital)$end-2] = NA
Hospital[ts_summary(Hospital)$end-1] = NA
Hospital[ts_summary(Hospital)$end] = NA
Hospital <- na.omit(Hospital)
p <- ts_ggplot(
  `f-curve` = ts_span(fc, "2020-01-01"),
  ` f-curve (seven-day ma)` = ts_span(fc_s, "2020-01-01"),
  `New cases (in 1,000, seven-day ma, FOPH)` = rollapply(Cases, noMA, mean, na.rm = TRUE)/1000,
  title = "New Covid-19 cases",
  subtitle = "Last three days ignored to avoid revisions because of late reports"
)
p <- ggLayout(p)
p <- p + scale_x_date(labels =  scales::date_format("%b %Y"))
p <- ggColor3(p)
#p <- addLines(p, myLines, myLabels, -18)
p
ggsave(filename = "../Results/Covid-19.png", width = figwidth, height = figheight)
ggsave(filename = "../Results/Covid-19.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `f-curve` = ts_span(fc, "2020-01-01"),
  ` f-curve (seven-day ma)` = ts_span(fc_s, "2020-01-01"),
  `New hospitalisations (in 10, seven-day ma, FOPH)` = rollapply(Hospital, noMA, mean, na.rm = TRUE)/10,
  title = "New Covid-19 hospitalisations",
  subtitle = "Last three days ignored to avoid revisions because of late reports"
)
p <- ggLayout(p)
p <- p + scale_x_date(labels =  scales::date_format("%b %Y"))
p <- ggColor3(p)
#p <- addLines(p, myLines, myLabels, -18)
p
ggsave(filename = "../Results/Covid-19_Hospital.png", width = figwidth, height = figheight)
ggsave(filename = "../Results/Covid-19_Hospital.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `f-curve` = ts_span(fc, "2020-01-01"),
  ` f-curve (seven-day ma)` = ts_span(fc_s, "2020-01-01"),
  `New deaths (in 10, seven-day ma, FOPH)` = rollapply(Deaths, noMA, mean, na.rm = TRUE)/10,
  title = "New Covid-19 deaths",
  subtitle = "Last three days ignored to avoid revisions because of late reports"
)
p <- ggLayout(p)
p <- p + scale_x_date(labels =  date_format("%b %Y"))
p <- ggColor3(p)
#p <- addLines(p, myLines, myLabels, -18)
p
ggsave(filename = "../Results/Covid-19_Deaths.png", width = figwidth, height = figheight)
ggsave(filename = "../Results/Covid-19_Deaths.pdf", width = figwidth, height = figheight)


# Save data for later analysis
save(list = c("fc", "fc_s", "fc_news", "fc_fin","lastObsDate"), file = "../Data/f-curve.RData")
toExport <- data.frame(ts_c(fc, fc_s, fc_news, fc_fin, fc_dom, fc_for, fc_res), lastObsDate)
colnames(toExport) <- c("f-curve", "smoothed", "fc-news", "fc-fin", "fc-dom", "fc-for", "fc-res","update")
write.csv(toExport, file = "../Results/f-curve-data.csv")

