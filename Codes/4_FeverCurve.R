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
noMA          <- 5      # Working days for moving average
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
p <- addLines(p, myLines, myLabels, -3.5)
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
ShortLines <- c("2020-03-16", "2020-03-25", "2020-04-03", "2020-04-16", "2020-04-30", "2020-05-27")
ShortLabels <- c("Covid-19 lockdown", "Economic aid package (announced)", "Increase aid package (announced)", "Easing lockdown (phase I, announced)", "Easing lockdown (phase II, announced)", "Easing lockdown (phase III, announced)")
p <- ts_ggplot(
  ` f-curve, five-day moving-average`  = ts_span(fc_s, "2020-02-01"),
  `f-curve`                         = ts_span(fc, "2020-02-01"),
  title = paste("Last observation: ", lastObsDate, "", sep = "")
)
p <- ggLayout(p)
p <- ggColor3(p)
p <- addLines(p, ShortLines, ShortLabels, -8)
p <- p + scale_x_date(labels =  date_format("%b %Y"))
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
p <- addLines(p, ShortLines, ShortLabels, -8)
p <- p + scale_x_date(labels =  date_format("%b %Y"))
p
ggsave(filename = "../Results/DecompositionShort.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/DecompositionShort.png", width = figwidth, height = figheight)


# Remove last observation if it is zero (somehow, FOPH reports a zero case number temporarily)
# But keep it later on if it is confirmed
if(Cases[ts_summary(Cases)$end] == 0){
  Cases[ts_summary(Cases)$end] = NA
}
p <- ts_ggplot(
  `f-curve` = ts_span(fc, "2020-01-01"),
  ` f-curve (5-day ma)` = ts_span(fc_s, "2020-01-01"),
  `New Covid-19 cases (in 100, 5-day ma, FOPH)` = rollapply(Cases, noMA, mean, na.rm = TRUE)/100,
  title = "Comparison with health crisis"
)
p <- ggLayout(p)
p <- p + scale_x_date(labels =  date_format("%b %Y"))
p <- ggColor3(p)
#p <- addLines(p, myLines, myLabels, -18)
p
ggsave(filename = "../Results/Covid-19.png", width = figwidth, height = figheight)
ggsave(filename = "../Results/Covid-19.pdf", width = figwidth, height = figheight)


# Save data for later analysis
save(list = c("fc", "fc_s", "fc_news", "fc_fin","lastObsDate"), file = "../Data/f-curve.RData")
toExport <- data.frame(ts_c(fc, fc_s), lastObsDate)
colnames(toExport) <- c("f-curve", "smoothed", "update")
write.csv(toExport, file = "../Results/f-curve-data.csv")

