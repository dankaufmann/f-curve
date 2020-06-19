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
# - Option for moving average for news indicators
# - Non-annualized GDP growth rates
#-------------------------------------------------------------------------------------

# Packages and settings
rm(list = ls())
source("AllPackages.R")
startSample <- "2000-01-01"
noMANews      <- 2 
# TODO: End date Sys.Date() when automatic updates
endDate     <- as.Date("2020-06-04")

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")
load(file="../Data/MacroData.RData")
Tecon <- Indicators$Tecon

p <- ts_ggplot(
  `News sentiment CH`          = ts_span(rollapply(Indicators$News.all.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  `News sentiment foreign`     = ts_span(rollapply(Indicators$News.all.FOR, noMANews, mean, na.rm = TRUE), startSample, endDate),
  title = "News sentiment"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/News.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `News sentiment CH (v1.0)`   = ts_span(rollapply(Indicators$News.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  `News sentiment CH (all)`     = ts_span(rollapply(Indicators$News.all.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  title = "News sentiment"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/News_Comparison1.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `News sentiment CH (v1.0)`   = ts_span(rollapply(Indicators$News.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  `News sentiment CH (comb)`     = ts_span(rollapply(Indicators$News.comb.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  title = "News sentiment"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/News_Comparison2.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `News sentiment CH (v1.0)`   = ts_span(rollapply(Indicators$News.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  `News sentiment CH (w/o Tagi)`     = ts_span(rollapply(Indicators$News.ex.CH, noMANews, mean, na.rm = TRUE), startSample, endDate),
  title = "News sentiment"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/News_Comparison3.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `GDP growth`            = ts_span(ts_pc(GDP), startSample),
  `Nom. GDP growth`       = ts_span(ts_pc(NGDP), startSample),
  `GDP deflator growth`   = ts_span(ts_pc(GDPDefl), startSample),
  `Output gap (SNB)`      = ts_span(Gap, startSample),
  title = "GDP and deflator"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/GDP.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `Employment growth`     = ts_span(ts_pc(EMP), startSample),
  `Unemployment (SECO)`   = ts_span(SECO, startSample),
  `Unemployment (ILO)`    = ts_span(ILOSA, startSample),
  title = "Labor market"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/LaborMarket.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `KOF Barometer/10-10`        = ts_span(Baro/10-10, startSample),
  `SNB Business Cycle Index`  = ts_span(Cycle, startSample),
  `OECD CLI - 100`             = ts_span(OECD-100, startSample),
  title = "Composite leading indicators"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/CLI.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `Internet search sentiment (trendEcon, 15d ma)` = ts_span(rollapply(Tecon, 15, mean, na.rm = TRUE), startSample),
  `Consumer sentiment (SECO)/50`          = ts_span(CSent/50, startSample),
  `Business Cycle Sentiment (SECO)`       = ts_span(Sent, startSample),
  title = "Sentiment indicators"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/Sentiment.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `Term spread CH`                  = ts_span(Indicators$TS.CH, startSample),
  `Interest rate diff. CH - EUR`    = ts_span(Indicators$IRDIFF.CH, startSample),
  `Term spread US`                  = ts_span(Indicators$TS.US, startSample),
  `Term spread EUR`                 = ts_span(Indicators$TS.EUR, startSample),
  title = "Bond market"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/BondMarket.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `VIX CH`             = ts_span(Indicators$VIX.CH, startSample),
  `VIX US`             = ts_span(Indicators$VIX.US, startSample),
  title = "Stock market"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/StockMarket.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `Risk premium (8Y)` = ts_span(Indicators$RP.CH, startSample),
  #`Risk premium (lower rating)`  = ts_span(Indicators$RP2.CH, startSample),
  `Risk premium (2Y)`      = ts_span(Indicators$RPShort.CH, startSample),
  title = "Risk premia Swiss companies"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/RiskPremiaCH.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  `Risk premium (foreign)`             = ts_span(Indicators$RP.FOR, startSample),
  `Risk premium (foreign 2Y bonds)`    = ts_span(Indicators$RPShort.FOR, startSample),
  title = "Risk premia foreign companies"
)
p <- ggLayout(p)
p
ggsave(filename = "../Results/PlotsData/RiskPremiaFOR.pdf", width = figwidth, height = figheight)


