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
# V 1.0
#-------------------------------------------------------------------------------------

# Packages and settings
#rm(list = ls())
source("AllPackages.R")
normStart <- as.Date("1999-06-01")   # Use some data before because we shift term spread forward
startDate <- as.Date("2000-01-01")

endDate       <- Sys.Date()
noMANews      <- 5      # Number of days uncentered moving average for news (otherwise very volatile) do that in second step...
noMA          <- 5      # Working days for moving average
leadTS        <- 0.5    # Lead of term-spread on the business cycle (in years)

# Choose Which indicators to use
whichInd   <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
indexDom   <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0,  0,  0)    
indexDom   <- indexDom == 1  # Needs to be TRUE/FALSE

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")

# Estimate the factor
Results <- computeFactors(Indicators, leadTS, noMANews, normStart, startDate, endDate, whichInd, indexDom)
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

# Main chart of curve
DataCor <- as.data.frame(ts_c(ts_pca(GDP), ts_frequency(fc, to = "quarter", aggregate = "mean", na.rm = TRUE)))
Correl  <- cor(DataCor, use = "na.or.complete")[1, 2]
p <- ts_ggplot(
  # `Baseline, five-day moving-av., inv. scale`  = -fc_s ,
  `f-curve, inverse scale`                         = -fc,
  `GDP growth (annualized)`                           = ts_span(ts_pca(GDP), startDate),
 title = paste("Last observation: ", lastObsDate, "", sep = "")
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -11)
p <- addCorr(p, -Correl, "2015-01-01", 6.5)
p
ggsave(filename = "../Results/MainGDP.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/MainGDP.png", width = figwidth, height = figheight)

# Main chart of curve
ShortLines <- c("2020-03-16", "2020-03-25", "2020-04-03", "2020-04-16", "2020-04-30")
ShortLabels <- c("Covid-19 lockdown", "Economic aid package (announced)", "Increase aid package (announced)", "Easing lockdown (phase I, announced)", "Easing lockdown (phase II, announced)")
p <- ts_ggplot(
  `f-curve, five-day moving-average`  = ts_span(fc_s, "2020-02-01"),
  `f-curve, raw data`                         = ts_span(fc, "2020-02-01"),
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
p <- p + ggtitle("f-curve decomposition")
p <- ggLayout(p)
p <- addLines(p, myLines, myLabels, 3)
p
ggsave(filename = "../Results/Decomposition.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/Decomposition.png", width = figwidth, height = figheight)

p <- ggplot(subset(DecompData, Date>"2020-02-01"), aes(x = Date, y = value,fill=variable)) + geom_bar(stat='identity')
p <- ggLayout(p)
p <- p + ggtitle("f-curve decomposition during Covid-19 lockdown")
p <- addLines(p, ShortLines, ShortLabels, -8)
p <- p + scale_x_date(labels =  date_format("%b %Y"))
p
ggsave(filename = "../Results/DecompositionShort.pdf", width = figwidth, height = figheight)
ggsave(filename = "../Results/DecompositionShort.png", width = figwidth, height = figheight)

# Save data for later analysis
save(list = c("fc", "fc_s", "lastObsDate"), file = "../Data/f-curve.RData")
toExport <- data.frame(fc, fc_s, lastObsDate)
colnames(toExport) <- c("f-curve", "smoothed", "update")
write.csv(toExport, file = "../Results/f-curve-data.csv")


