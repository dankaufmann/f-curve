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
endDate   <- Sys.Date()

# Settings real time evaluation
normStart <- as.Date("1999-01-01")
startDate <- as.Date("2000-01-01")
realTimeEst <- FALSE   # CHoose whether reestimate factor every time

# Choose Which indicators to use for rolling estimation (do decompostion)
#whichInd   <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#indexDom   <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  1,  1)    # After choosing indicators
#leadTS     <- 0.5       # Lead of term-spread on the business cycle (in years)

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")
load(file="../Data/MacroData.RData")
load(file="../Data/f-curve.RData")

# Will be transformed to growth rates for forecast and evaluation!
# Either NGDP/GDP or RealTimeNom/Realtime
depVar    <- GDP
vintVar   <- RealTime
depVarLab <- "GDP"
H         <- 4       # Forecast horizon (including last quarter)

Ind       <- fc_news
IndLab    <- "fc_news"

noLags    <- 1       # How many lags in direct forecasting model
noAR      <- 1       # How many lags in benchmark AR(p) model 
startVint <- 1       # With which GDP vintage to start pseudo out-of-sample analysis

Bench     <- Baro    # Benchmark indicator

#-------------------------------------------------------------------------------------
# Do real-time forecast evaluation for current quarter compared to final GDP
#-------------------------------------------------------------------------------------
FcstFc.h0 <- ts_span(vintVar, "2000-01-01")
FcstFc.h0[,] <- NA
FcstFc.h1 <- FcstFc.h0
ErrFc.h0  <- FcstFc.h0
ErrFc.h1  <- FcstFc.h0

FcstAR.h0 <- FcstFc.h0
FcstAR.h1 <- FcstFc.h0
ErrAR.h0  <- FcstFc.h0
ErrAR.h1  <- FcstFc.h0

FcstBench.h0 <- FcstFc.h0
FcstBench.h1 <- FcstFc.h0
ErrBench.h0  <- FcstFc.h0
ErrBench.h1  <- FcstFc.h0

FcstVint.h0 <- FcstFc.h0
FcstVint.h1 <- FcstFc.h0
ErrVint.h0  <- FcstFc.h0
ErrVint.h1  <- FcstFc.h0

# Final GDP for computing forecast errors
Y.final <- ts_pc(depVar)

for (v in startVint:dim(vintVar)[2]){
  
  thisObsDate <- colnames(vintVar[, v])
  
  # Compute factor
  if(realTimeEst){
    Results <- computeFactors(Indicators, leadTS, normStart, startDate, thisObsDate, whichInd, indexDom)
    Ind     <- Results[[1]]
    
  }else{
    Ind <- Ind
  }
  
  # 0) Construct the data
  Y.h0    <- ts_pc(vintVar[, v])
  Y.h1    <- L.op((ts_pc(vintVar[, v])), -1)
  
  # Include each quarter data up to the xx month and xxx day
  #include_curr <- (allMonths-allQuarters*3+3 <=  currMonth-currQuart*3+3 & allDays <= currDay)
  #fc_curr      <- fc
  #fc_curr[!include_curr] <- NA
  Ind_curr <- Ind
  Ind_curr[index(Ind)>=thisObsDate, ] <- NA
  X.l0         <- (ts_frequency(Ind_curr, to = "quarter", aggregate = "mean", na.rm = TRUE))
  Data <- ts_c(Y.h0, Y.h1, X.l0)
  if(noLags>0){
    for(p in 1:noLags){
      temp <- L.op(ts_frequency(Ind_curr, to = "quarter", aggregate = "mean", na.rm = TRUE), p)
      Data <- ts_c(Data, temp)
    }
  }
 
  thisObsDateM <- as.Date(paste(year(thisObsDate), month(thisObsDate), "01", sep = "-"))
  Bench_curr <- Bench
  Bench_curr[index(Bench)>=thisObsDateM, ] <- NA
  B.l0         <- (ts_frequency(Bench_curr, to = "quarter", aggregate = "mean", na.rm = TRUE))
  Data <- ts_c(Data, B.l0)
  if(noLags>0){
    for(p in 1:noLags){
      temp <- L.op(ts_frequency(Bench_curr, to = "quarter", aggregate = "mean", na.rm = TRUE), p)
      Data <- ts_c(Data, temp)
    }
  }
  
  colnames(Data) <- c("Y.h0", "Y.h1", paste("X.l", 0:noLags, sep = ""), paste("B.l", 0:noLags, sep = ""))
  Data   <- ts_span(Data, ts_summary(X.l0)$start)
  Xindex <- startsWith(colnames(Data), "X.l")
  Bindex <- startsWith(colnames(Data), "B.l")
  
  Model.h0  <- Arima(Data[,1], order = c(0, 0, 0), xreg = Data[,Xindex])
  #summary(Model.h0)
  Fcst.h0     <- forecast(Model.h0, xreg = Data[ts_summary(Data$X.l0)$end, Xindex], h = 1, level = c(50, 80, 90, 95))
  
  Model.h1  <- Arima(Data[,2], order = c(0, 0, 0), xreg = Data[,Xindex])
  #summary(Model.h1)
  Fcst.h1     <- forecast(Model.h1, xreg = Data[ts_summary(Data$X.l0)$end, Xindex], h = 1, level = c(50, 80, 90, 95))
  
  
  
  Model.B.h0  <- Arima(Data[,1], order = c(0, 0, 0), xreg = Data[,Bindex])
  #summary(Model.h0)
  Fcst.B.h0     <- forecast(Model.B.h0, xreg = Data[ts_summary(Data$B.l0)$end, Bindex], h = 1, level = c(50, 80, 90, 95))
  
  Model.B.h1  <- Arima(Data[,2], order = c(0, 0, 0), xreg = Data[,Bindex])
  #summary(Model.h1)
  Fcst.B.h1     <- forecast(Model.B.h1, xreg = Data[ts_summary(Data$B.l0)$end, Bindex], h = 1, level = c(50, 80, 90, 95))
  
  Model.ar1 <- Arima(Data$Y.h0, order = c(noAR, 0, 0), include.constant= TRUE)
  #summary(Model.ar1)
  Fcst.ar1     <- forecast(Model.ar1, h = 2, level = c(50, 80, 90, 95))
  
  # 3) Save the point forecasts
  currDate <- as.Date(ts_summary(vintVar[, v])$end)
  Date.h0  <- seq.Date(currDate, length.out = 2, by = "quarter")[2]
  Date.h1  <- seq.Date(currDate, length.out = 3, by = "quarter")[3]
  
  FcstFc.h0[Date.h0, v] <- Fcst.h0$mean
  FcstFc.h1[Date.h1, v] <- Fcst.h1$mean
  
  FcstBench.h0[Date.h0, v] <- Fcst.B.h0$mean
  FcstBench.h1[Date.h1, v] <- Fcst.B.h1$mean
  
  FcstAR.h0[Date.h0, v] <- Fcst.ar1$mean[1]
  FcstAR.h1[Date.h1, v] <- Fcst.ar1$mean[2]
  
  if(ts_summary(Y.final)$end >= Date.h0){
    FcstVint.h0[Date.h0, v] <- ts_pc(vintVar[,v+1])[Date.h0]
    
    ErrFc.h0[Date.h0, v]   <- Y.final[Date.h0] - as.numeric(Fcst.h0$mean[1])
    ErrBench.h0[Date.h0, v] <- Y.final[Date.h0] - as.numeric(Fcst.B.h0$mean[1])
    ErrAR.h0[Date.h0, v]   <- Y.final[Date.h0] - as.numeric(Fcst.ar1$mean[1])
    ErrVint.h0[Date.h0, v] <- Y.final[Date.h0] - as.numeric(FcstVint.h0[Date.h0, v])
  }
  if(ts_summary(Y.final)$end >= Date.h1){
    FcstVint.h1[Date.h1, v] <- ts_pc(vintVar[,v+2])[Date.h1]
    
    ErrFc.h1[Date.h1, v]   <- Y.final[Date.h1] - as.numeric(Fcst.h1$mean[1])
    ErrBench.h1[Date.h1, v] <- Y.final[Date.h1] - as.numeric(Fcst.B.h1$mean[1])
    ErrAR.h1[Date.h1, v]   <- Y.final[Date.h1] - as.numeric(Fcst.ar1$mean[2])
    ErrVint.h1[Date.h1, v] <- Y.final[Date.h1] - as.numeric(FcstVint.h1[Date.h1, v])
  }
}

# 4) Compute evaluation measures (RMSE, rel. RMSE to AR(1) forecast)
FcstAR.h0   <- ts_span(FcstAR.h0, "2001-01-01")
FcstBench.h0   <- ts_span(FcstBench.h0, "2001-01-01")
FcstFc.h0   <- ts_span(FcstFc.h0, "2001-01-01")
FcstVint.h0 <- ts_span(FcstVint.h0, "2001-01-01")
FcstAR.h1   <- ts_span(FcstAR.h1, "2001-01-01")
FcstFc.h1   <- ts_span(FcstFc.h1, "2001-01-01")
FcstBench.h1   <- ts_span(FcstBench.h1, "2001-01-01")
FcstVint.h1 <- ts_span(FcstVint.h1, "2001-01-01")

# Create one version with all data and one version exluding crises
FcstDates.h0 <- index(ts_span(ErrAR.h0, "2001-01-01"))
FcstDates.h1 <- index(ts_span(ErrAR.h1, "2001-01-01"))

ErrAR.h0    <- rowMeans(ts_span(ErrAR.h0, "2001-01-01"), na.rm = TRUE)
ErrFc.h0    <- rowMeans(ts_span(ErrFc.h0, "2001-01-01"), na.rm = TRUE)
ErrBench.h0 <- rowMeans(ts_span(ErrBench.h0, "2001-01-01"), na.rm = TRUE)
ErrVint.h0  <- rowMeans(ts_span(ErrVint.h0, "2001-01-01"), na.rm = TRUE)
ErrAR.h1    <- rowMeans(ts_span(ErrAR.h1, "2001-01-01"), na.rm = TRUE)
ErrFc.h1    <- rowMeans(ts_span(ErrFc.h1, "2001-01-01"), na.rm = TRUE)
ErrBench.h1 <- rowMeans(ts_span(ErrBench.h1, "2001-01-01"), na.rm = TRUE)
ErrVint.h1  <- rowMeans(ts_span(ErrVint.h1, "2001-01-01"), na.rm = TRUE)

# RMSE all data
RMSEFc.h0   <- sqrt(mean(ErrFc.h0^2, na.rm = TRUE))
RMSEFc.h1   <- sqrt(mean(ErrFc.h1^2, na.rm = TRUE))
RMSEBench.h0   <- sqrt(mean(ErrBench.h0^2, na.rm = TRUE))
RMSEBench.h1   <- sqrt(mean(ErrBench.h1^2, na.rm = TRUE))
RMSEAR.h0   <- sqrt(mean(ErrAR.h0^2, na.rm = TRUE))
RMSEAR.h1   <- sqrt(mean(ErrAR.h1^2, na.rm = TRUE))
RMSEVint.h0 <- sqrt(mean(ErrVint.h0^2, na.rm = TRUE))
RMSEVint.h1 <- sqrt(mean(ErrVint.h1^2, na.rm = TRUE))

# RMSE excluding crisis
exCrisis.h0 <- year(FcstDates.h0)!=2008&year(FcstDates.h0)!=2009&year(FcstDates.h0)!=2020
exCrisis.h1 <- year(FcstDates.h1)!=2008&year(FcstDates.h1)!=2009&year(FcstDates.h1)!=2020
RMSEFc.Ex.h0   <- sqrt(mean(ErrFc.h0[exCrisis.h0]^2, na.rm = TRUE))
RMSEFc.Ex.h1   <- sqrt(mean(ErrFc.h1[exCrisis.h1]^2, na.rm = TRUE))
RMSEBench.Ex.h0   <- sqrt(mean(ErrBench.h0[exCrisis.h0]^2, na.rm = TRUE))
RMSEBench.Ex.h1   <- sqrt(mean(ErrBench.h1[exCrisis.h1]^2, na.rm = TRUE))
RMSEAR.Ex.h0   <- sqrt(mean(ErrAR.h0[exCrisis.h0]^2, na.rm = TRUE))
RMSEAR.Ex.h1   <- sqrt(mean(ErrAR.h1[exCrisis.h1]^2, na.rm = TRUE))
RMSEVint.Ex.h0 <- sqrt(mean(ErrVint.h0[exCrisis.h0]^2, na.rm = TRUE))
RMSEVint.Ex.h1 <- sqrt(mean(ErrVint.h1[exCrisis.h1]^2, na.rm = TRUE))

# RMSE excluding SECO
exSECO.h0 <- year(FcstDates.h0)!=2019&year(FcstDates.h0)!=2020
exSECO.h1 <- year(FcstDates.h1)!=2019&year(FcstDates.h1)!=2020
RMSEFc.ExSec.h0   <- sqrt(mean(ErrFc.h0[exSECO.h0]^2, na.rm = TRUE))
RMSEFc.ExSec.h1   <- sqrt(mean(ErrFc.h1[exSECO.h1]^2, na.rm = TRUE))
RMSEBench.ExSec.h0   <- sqrt(mean(ErrBench.h0[exSECO.h0]^2, na.rm = TRUE))
RMSEBench.ExSec.h1   <- sqrt(mean(ErrBench.h1[exSECO.h1]^2, na.rm = TRUE))
RMSEAR.ExSec.h0   <- sqrt(mean(ErrAR.h0[exSECO.h0]^2, na.rm = TRUE))
RMSEAR.ExSec.h1   <- sqrt(mean(ErrAR.h1[exSECO.h1]^2, na.rm = TRUE))
RMSEVint.ExSec.h0 <- sqrt(mean(ErrVint.h0[exSECO.h0]^2, na.rm = TRUE))
RMSEVint.ExSec.h1 <- sqrt(mean(ErrVint.h1[exSECO.h1]^2, na.rm = TRUE))

# DM Test
Test.h0     <- dm.test(ErrFc.h0, ErrAR.h0, alternative = c("less"), h = 1, power = 2)
Test.h1     <- dm.test(ErrFc.h1, ErrAR.h1, alternative = c("less"), h = 2, power = 2)
Test2.h0    <- dm.test(ErrVint.h0, ErrFc.h0, alternative = c("less"), h = 1, power = 2)
Test2.h1    <- dm.test(ErrVint.h1, ErrFc.h1, alternative = c("less"), h = 2, power = 2)
Test3.h0     <- dm.test(ErrFc.h0, ErrBench.h0, alternative = c("less"), h = 1, power = 2)
Test3.h1     <- dm.test(ErrFc.h1, ErrBench.h1, alternative = c("less"), h = 2, power = 2)

# DM Test excluding crisis
Test.Ex.h0     <- dm.test(ErrFc.h0[exCrisis.h0], ErrAR.h0[exCrisis.h0], alternative = c("less"), h = 1, power = 2)
Test.Ex.h1     <- dm.test(ErrFc.h1[exCrisis.h1], ErrAR.h1[exCrisis.h1], alternative = c("less"), h = 2, power = 2)
Test2.Ex.h0    <- dm.test(ErrVint.h0[exCrisis.h0], ErrFc.h0[exCrisis.h0], alternative = c("less"), h = 1, power = 2)
Test2.Ex.h1    <- dm.test(ErrVint.h1[exCrisis.h1], ErrFc.h1[exCrisis.h1], alternative = c("less"), h = 2, power = 2)
Test3.Ex.h0     <- dm.test(ErrFc.h0[exCrisis.h0], ErrBench.h0[exCrisis.h0], alternative = c("less"), h = 1, power = 2)
Test3.Ex.h1     <- dm.test(ErrFc.h1[exCrisis.h1], ErrBench.h1[exCrisis.h1], alternative = c("less"), h = 2, power = 2)

# DM Test excluding SECO
Test.ExSec.h0     <- dm.test(ErrFc.h0[exSECO.h0], ErrAR.h0[exSECO.h0], alternative = c("less"), h = 1, power = 2)
Test.ExSec.h1     <- dm.test(ErrFc.h1[exSECO.h1], ErrAR.h1[exSECO.h1], alternative = c("less"), h = 2, power = 2)
Test2.ExSec.h0    <- dm.test(ErrVint.h0[exSECO.h0], ErrFc.h0[exSECO.h0], alternative = c("less"), h = 1, power = 2)
Test2.ExSec.h1    <- dm.test(ErrVint.h1[exSECO.h1], ErrFc.h1[exSECO.h1], alternative = c("less"), h = 2, power = 2)
Test3.ExSec.h0     <- dm.test(ErrFc.h0[exSECO.h0], ErrBench.h0[exSECO.h0], alternative = c("less"), h = 1, power = 2)
Test3.ExSec.h1     <- dm.test(ErrFc.h1[exSECO.h1], ErrBench.h1[exSECO.h1], alternative = c("less"), h = 2, power = 2)

print("")
print("")
print("Forecast evaluation compared to first release")
Tab1  <- rbind(c("$h=0$", round(RMSEVint.h0, 2), round(RMSEFc.h0, 2), round(RMSEVint.h0/RMSEFc.h0, 2), round(Test2.h0$p.value, 3)), 
              c("$h=1$", round(RMSEVint.h1, 2), round(RMSEFc.h1, 2), round(RMSEVint.h1/RMSEFc.h1, 2), round(Test2.h1$p.value, 3)))
write.table(Tab1, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalVint_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("Forecast evaluation compared to AR(1)")
Tab2  <- rbind(c("$h=0$", round(RMSEFc.h0, 2), round(RMSEAR.h0, 2), round(RMSEFc.h0/RMSEAR.h0, 2), round(Test.h0$p.value, 3)), 
               c("$h=1$", round(RMSEFc.h1, 2), round(RMSEAR.h1, 2), round(RMSEFc.h1/RMSEAR.h1, 2), round(Test.h1$p.value, 3)))
write.table(Tab2, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalAR_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("Forecast evaluation compared to Benchmark")
Tab3  <- rbind(c("$h=0$", round(RMSEFc.h0, 2), round(RMSEBench.h0, 2), round(RMSEFc.h0/RMSEBench.h0, 2), round(Test3.h0$p.value, 3)), 
               c("$h=1$", round(RMSEFc.h1, 2), round(RMSEBench.h1, 2), round(RMSEFc.h1/RMSEBench.h1, 2), round(Test3.h1$p.value, 3)))
write.table(Tab3, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalBench_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("No vintages")
print(sum(!is.na(ErrAR.h0)))
print(sum(!is.na(ErrAR.h1)))


# Everything excluding crisis
print("")
print("")
print("Forecast evaluation compared to first release")
Tab1  <- rbind(c("$h=0$", round(RMSEVint.Ex.h0, 2), round(RMSEFc.Ex.h0, 2), round(RMSEVint.Ex.h0/RMSEFc.Ex.h0, 2), round(Test2.Ex.h0$p.value, 3)), 
               c("$h=1$", round(RMSEVint.Ex.h1, 2), round(RMSEFc.Ex.h1, 2), round(RMSEVint.Ex.h1/RMSEFc.Ex.h1, 2), round(Test2.Ex.h1$p.value, 3)))
write.table(Tab1, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalVint_Ex_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("Forecast evaluation compared to AR(1)")
Tab2  <- rbind(c("$h=0$", round(RMSEFc.Ex.h0, 2), round(RMSEAR.Ex.h0, 2), round(RMSEFc.Ex.h0/RMSEAR.Ex.h0, 2), round(Test.Ex.h0$p.value, 3)), 
               c("$h=1$", round(RMSEFc.Ex.h1, 2), round(RMSEAR.Ex.h1, 2), round(RMSEFc.Ex.h1/RMSEAR.Ex.h1, 2), round(Test.Ex.h1$p.value, 3)))
write.table(Tab2, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalAR_Ex_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("Forecast evaluation compared to Benchmark")
Tab3  <- rbind(c("$h=0$", round(RMSEFc.Ex.h0, 2), round(RMSEBench.Ex.h0, 2), round(RMSEFc.Ex.h0/RMSEBench.Ex.h0, 2), round(Test3.Ex.h0$p.value, 3)), 
               c("$h=1$", round(RMSEFc.Ex.h1, 2), round(RMSEBench.Ex.h1, 2), round(RMSEFc.Ex.h1/RMSEBench.Ex.h1, 2), round(Test3.Ex.h1$p.value, 3)))
write.table(Tab3, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalBench_Ex_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("No vintages")
print(sum(!is.na(ErrAR.h0)))
print(sum(!is.na(ErrAR.h1)))


# Everything excluding SECO
print("")
print("")
print("Forecast evaluation compared to first release")
Tab1  <- rbind(c("$h=0$", round(RMSEVint.ExSec.h0, 2), round(RMSEFc.ExSec.h0, 2), round(RMSEVint.ExSec.h0/RMSEFc.ExSec.h0, 2), round(Test2.ExSec.h0$p.value, 3)), 
               c("$h=1$", round(RMSEVint.ExSec.h1, 2), round(RMSEFc.ExSec.h1, 2), round(RMSEVint.ExSec.h1/RMSEFc.ExSec.h1, 2), round(Test2.ExSec.h1$p.value, 3)))
write.table(Tab1, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalVint_ExSeco_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("Forecast evaluation compared to AR(1)")
Tab2  <- rbind(c("$h=0$", round(RMSEFc.ExSec.h0, 2), round(RMSEAR.ExSec.h0, 2), round(RMSEFc.ExSec.h0/RMSEAR.ExSec.h0, 2), round(Test.ExSec.h0$p.value, 3)), 
               c("$h=1$", round(RMSEFc.ExSec.h1, 2), round(RMSEAR.ExSec.h1, 2), round(RMSEFc.ExSec.h1/RMSEAR.ExSec.h1, 2), round(Test.ExSec.h1$p.value, 3)))
write.table(Tab2, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalAR_ExSeco_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("Forecast evaluation compared to Benchmark")
Tab3  <- rbind(c("$h=0$", round(RMSEFc.ExSec.h0, 2), round(RMSEBench.ExSec.h0, 2), round(RMSEFc.ExSec.h0/RMSEBench.ExSec.h0, 2), round(Test3.ExSec.h0$p.value, 3)), 
               c("$h=1$", round(RMSEFc.ExSec.h1, 2), round(RMSEBench.ExSec.h1, 2), round(RMSEFc.ExSec.h1/RMSEBench.ExSec.h1, 2), round(Test3.ExSec.h1$p.value, 3)))
write.table(Tab3, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/EvalBench_ExSeco_", depVarLab, "_", IndLab, "_",  "RT", realTimeEst, ".tex", sep =""))
print("")
print("")
print("No vintages")
print(sum(!is.na(ErrAR.h0)))
print(sum(!is.na(ErrAR.h1)))
