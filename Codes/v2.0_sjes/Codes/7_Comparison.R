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
#-------------------------------------------------------------------------------------

# Packages and settings
rm(list = ls())
source("AllPackages.R")
startDate <- as.Date("2000-01-01")

# TODO: End date Sys.Date() when automatic updates
endDate     <- as.Date("2020-06-04")

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")
load(file="../Data/MacroData.RData")
load(file="../Data/f-curve.RData")

Ind    <- fc
IndLab <- "fc"
Tecon <- Indicators$Tecon

#-------------------------------------------------------------------------------------
# Do leading indicator analysis according to Neusser
#-------------------------------------------------------------------------------------
# f-curve and all other indicators
myTitles <- c("KOF Economic Barometer", "Swiss Economic Confidence (SECO)", "Consumer sentiment (SECO)", 
              "Perceived economic situation (trendEcon)", "CLI (OECD)", "Business Cycle Index (SNB)")
myIndic  <- list(Baro, Sent, CSent, Tecon, OECD, Cycle)
myNames  <- c("Baro", "Sent", "CSent", "Tecon", "OECD", "Cycle")
prewhite = TRUE

for(i in 1:length(myNames)){
  
  myInd  <- myIndic[[i]]
  myFreq <- ts_summary(myInd)$freq
  
  if(myFreq < 12){
    # 1) Aggregate to quarterly frequency
    fcQ <- ts_frequency(Ind, to = "quarter", aggregate= "mean", na.rm = TRUE)
    CLI <- ts_frequency(myInd, to = "quarter", aggregate= "mean", na.rm = TRUE)
    myFreq <- 4
    
  }else{
    # 1) Aggregate to monthly frequency
    fcQ <- ts_frequency(Ind, to = "month", aggregate= "mean", na.rm = TRUE)
    CLI <- ts_frequency(myInd, to = "month", aggregate= "mean", na.rm = TRUE)
    myFreq <- 12
    
  }  
  
  CLI <- ts_span(CLI, startDate, endDate)
  fcQ <- ts_span(fcQ, startDate, endDate)
  
  # 2) Compute pre-whitened cross-correlation
  if(prewhite == TRUE){
    Model.fcQ <- auto.arima(fcQ, max.d = 0, max.p = (myFreq+1), max.q = 0, ic = c("bic"))
    Model.CLI <- auto.arima(CLI, max.d = 0, max.p = (myFreq+1), max.q = 0, ic = c("bic"))
    res.fcQ <- xts(resid(Model.fcQ), order.by = index(fcQ))
    res.CLI <- xts(resid(Model.CLI), order.by = index(CLI))
  }else{
    res.fcQ <- fcQ
    res.CLI <- CLI
  }
  
  if(myNames[i] == "OECD"){
    Model.CLI <- arima(CLI, order = c(4, 0, 0))
    res.CLI <- xts(resid(Model.CLI), order.by = index(CLI))
  }
  
  # Do CCF (see Neusser ch. 12.1)
  CrossC       <- ccf(ts_ts(res.CLI), ts_ts(res.fcQ), lag.max = myFreq, plot = FALSE)
  CrossC$ciu   <- CrossC$acf
  CrossC$cil   <- CrossC$acf
  CrossC$ciu[] <- 1.96*CrossC$n.used^(-1/2)
  CrossC$cil[] <- -1.96*CrossC$n.used^(-1/2)
  
  Data <- data.frame(CrossC$lag*myFreq, CrossC$acf, CrossC$cil, CrossC$ciu)
  colnames(Data) <- c("s", "ccf", "lower", "upper")
  DataCI <- Data[,c(1, 3, 4)]
  p <- ggplot(Data, aes(x = s, y = ccf)) + geom_bar(stat='identity' , width=0.2)
  p <- p + geom_line(data=DataCI, aes(x=s, y=lower), colour="blue", linetype = "dashed")
  p <- p + geom_line(data=DataCI, aes(x=s, y=upper), colour="blue", linetype = "dashed")
  p <- ggLayout(p)
  p <- p + ggtitle(myTitles[i])+ylab("Cross-correlation, indicator (t+s), f-curve (t)")+xlab("Displacement (s)")
  p
  ggsave(paste("../Results/CrossCorr_", IndLab, "_", myNames[i], ".pdf", sep =""), width = figwidth, height = figheight)

}

#-------------------------------------------------------------------------------------
# Do comparison with other data
#-------------------------------------------------------------------------------------
if(IndLab == "fc"){
  
p <- ts_ggplot(
  #`Baseline, five-day unc. ma, inv. scale`  = -fc_s/1.8 ,
  `. f-curve, inverse, rescaled`  = -Ind/5,
  `Employment growth`   = ts_span(ts_pc(EMP), startDate),
  title = "Employment"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -2)
p
ggsave(filename = "../Results/ComparisonEmployment.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  #`Baseline, five-day unc. ma, rescaled`  = fc_s/5+2.5 ,
  `f-curve, rescaled`  = Ind/5+2.5,
  `Unempmloyment rate (sa)`   = ts_span(SECO, startDate),
  title = "Unempmloyment rate (SECO)"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, 0)
p
ggsave(filename = "../Results/ComparisonUnempSECO.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  #`Baseline, five-day unc. ma, rescaled`  = fc_s/4+3.5 ,
  `f-curve, rescaled`  = Ind/4+3.5,
  `Unempmloyment rate (sa)`   = ts_span(ILOSA, startDate),
  title = "Unemployment rate (ILO)"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, 4.5)
p
ggsave(filename = "../Results/ComparisonUnempILO.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  #`Baseline, five-day unc. ma, inv. scale`  = -fc_s*3+100 ,
  `f-curve, inverse, rescaled`  = -Ind*3+100,
  `KOF Barometer`   = ts_span(Baro, startDate),
  title = "KOF Barometer"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, 60)
p
ggsave(filename = "../Results/ComparisonBaro.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
#  `Baseline, five-day unc. ma, inv. scale`  = -fc_s/2.5 ,
  `. f-curve, inverse, rescaled`  = -Ind/2.5,
  `Swiss economic confidence (SECO-SEC)`   = ts_span(Sent, startDate),
  title = "Swiss economic confidence"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -6)
p
ggsave(filename = "../Results/ComparisonSent.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  #`Baseline, five-day unc. ma, inv. scale`  = -fc_s/2.5 ,
  `f-curve, inverse, rescaled`  = -Ind/2.5,
  `Output gap (SNB)`   = ts_span(Gap, startDate),
  title = "Output gap"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -6)
p
ggsave(filename = "../Results/ComparisonGap.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
#  `Baseline, five-day unc. ma, inv. scale`  = -fc_s/2.5 ,
  `. f-curve, inv. scale`  = -Ind/2.5,
  `Business Cycle Index (SNB)`   = ts_span(Cycle, startDate),
  title = "Business Cycle Index"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -6)
p
ggsave(filename = "../Results/ComparisonCycle.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  #`Baseline, five-day unc. ma, inv. scale`  = -fc_s/2.5+100,
  `f-curve, inverse, rescaled`  = -Ind/2.5+100,
  `OECD CLI`   = ts_span(OECD, startDate),
  title = "OECD Composite Leading Indicator"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, 95)
p
ggsave(filename = "../Results/ComparisonOECD.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
  #`Baseline, five-day unc. ma, inv. scale`  = -fc_s*9,
  `. f-curve, inverse, rescaled`  = -Ind*9,
  `Consumer Sentiment (SECO)`   = ts_span(CSent, startDate),
  title = "Consumer sentiment"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -100)
p
ggsave(filename = "../Results/ComparisonConsSent.pdf", width = figwidth, height = figheight)

p <- ts_ggplot(
#  `Baseline, five-day unc. ma, inv. scale`  = -fc_s/3,
  `f-curve, inverse, rescaled`  = -Ind/3,
  `Perceived economic situation (15-day ma.)`  = rollapply(Tecon, 15, mean, na.rm = TRUE),
  title = "Internet sentiment (trendEcon)"
)
p <- ggLayout(p)
p <- ggColor2(p)
p <- addLines(p, myLines, myLabels, -5)
p
ggsave(filename = "../Results/ComparisonTrendEcon15d.pdf", width = figwidth, height = figheight)
}
