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
# - Non-annualized growth rates
#-------------------------------------------------------------------------------------

# Packages and settings
#rm(list = ls())
source("AllPackages.R")
startDate <- as.Date("2000-01-01")
endDate   <- Sys.Date()

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")
load(file="../Data/f-curve.RData")

# Will be transformed to growth rates for forecast and evaluation!
# Either NGDP/GDP or RealTimeNom/Realtime
depVar    <- GDP
depVarLab <- "GDP"
H         <- 4       # Forecast horizon (including last quarter)

Ind       <- fc
noLags    <- 1       # How many lags in direct forecasting model

#-------------------------------------------------------------------------------------
# Do direct forecast based on state of info today
#-------------------------------------------------------------------------------------
# First variable is the indicator based on information up to the last observation each quarter
# Basically tell us what would be the forecast for the current quarter GDP growth rate
lastObsDep  <- as.Date(paste(year(ts_summary(depVar)$end), month(ts_summary(depVar)$end), "01", sep = "-"))
FcstDates   <- seq.Date(lastObsDep, length.out = (H+1), by = "quarter")[2:(H+1)]
lastObsInd  <- ts_summary(Ind)$end
Fcst        <- xts(rep(NA, H), order.by = FcstDates)
FcstVar     <- xts(rep(NA, H), order.by = FcstDates)
FcstCI      <- xts(array(NA, c(H, 4)), order.by = FcstDates)
colnames(FcstCI) <- c("Upper90", "Lower90", "Upper95", "Lower95")

# Choose indicator
allQuarters <- quarter(index(Ind))
allMonths   <- month(index(Ind))
allDays     <- day(index(Ind))
currQuart   <- quarter(lastObsInd)
currMonth   <- month(lastObsInd)
currDay     <- day(lastObsInd)

for(h in 1:length(FcstDates)){
  
  # If the indicator is available for the quarter to forecast, do not shift the dependent variable
  if(lastObsDate >= FcstDates[h]){
    
    # construct data set
    Y.h0  <- ts_pc(depVar)
    
    # Remove observations that were not observed in the past
    if(lastObsDate < FcstDates[h+1]){
      # Include each quarter data up to the xx month and xxx day
      #to_include <- (allMonths-allQuarters*3+3 <=  currMonth-currQuart*3+3 & allDays <= currDay)
      #X.l0  <- ts_frequency(Ind[to_include,], to = "quarter", aggregate = "mean", na.rm = TRUE)
      X.l0  <- ts_frequency(Ind, to = "quarter", aggregate = "mean", na.rm = TRUE)
      
    }else{
      X.l0  <- ts_frequency(Ind, to = "quarter", aggregate = "mean", na.rm = TRUE)
    }
    
    Data <- ts_c(Y.h0, X.l0)
    if(noLags>0){
      for(p in 1:noLags){
        temp <- L.op(ts_frequency(Ind, to = "quarter", aggregate = "mean", na.rm = TRUE), p)
        Data <- ts_c(Data, temp)
        
      }
    }
    colnames(Data) <- c("Y.h0", paste("X.l", 0:noLags, sep = ""))
    Data   <- ts_span(Data, ts_summary(X.l0)$start)
    Xindex <- startsWith(colnames(Data), "X.l")
    
    Model.h0  <- Arima(Data[,1], order = c(0, 0, 0), xreg = Data[,Xindex])
    summary(Model.h0)
    Fcst.h0    <- forecast(Model.h0, xreg = Data[FcstDates[h], Xindex], h = 1, level = c(50, 80, 90, 95))
    Fcst[h]    <- Fcst.h0$mean
    FcstVar[h] <- getForecastVariance(Fcst.h0)
    
    FcstCI$Upper90[h] <- Fcst[h]+1.64*sqrt(FcstVar[h])
    FcstCI$Lower90[h] <- Fcst[h]-1.64*sqrt(FcstVar[h])
    FcstCI$Upper95[h] <- Fcst[h]+1.96*sqrt(FcstVar[h])
    FcstCI$Lower95[h] <- Fcst[h]-1.96*sqrt(FcstVar[h])
    
  }else{
    
    # Otherwise shift the dependent variable
    
    # If we made a backcast, shfit the dpeendent variable by one observation less
    if(lastObsDate >= FcstDates[1]){
      offs <- 1 
    }
    
    # Remove observations that were not observed in the past
    # Include each quarter data up to the xx month and xxx day
    #to_include <- (allMonths-allQuarters*3+3 <=  currMonth-currQuart*3+3 & allDays <= currDay)
    
    # construct data set
    Y.h   <- L.op(ts_pc(depVar), -(h-offs))
    X.l0  <- ts_frequency(Ind, to = "quarter", aggregate = "mean", na.rm = TRUE)
    #X.l0  <- ts_frequency(Ind[to_include,], to = "quarter", aggregate = "mean", na.rm = TRUE)
    
    Data <- ts_c(Y.h, X.l0)
    if(noLags>0){
      for(p in 1:noLags){
        temp <- L.op(ts_frequency(Ind, to = "quarter", aggregate = "mean", na.rm = TRUE), p)
        Data <- ts_c(Data, temp)
        
      }
    }
    colnames(Data) <- c("Y.h", paste("X.l", 0:noLags, sep = ""))
    Data   <- ts_span(Data, ts_summary(X.l0)$start)
    Xindex <- startsWith(colnames(Data), "X.l")
    
    Model.h  <- Arima(Data[,1], order = c(0, 0, 0), xreg = Data[,Xindex])
    summary(Model.h)
    Fcst.h     <- forecast(Model.h, xreg = Data[ts_summary(Data$X.l0)$end, Xindex], h = 1, level = c(50, 80, 90, 95))
    Fcst[h]    <- Fcst.h$mean
    FcstVar[h] <- getForecastVariance(Fcst.h)
    
    FcstCI$Upper90[h] <- Fcst[h]+1.64*sqrt(FcstVar[h])
    FcstCI$Lower90[h] <- Fcst[h]-1.64*sqrt(FcstVar[h])
    FcstCI$Upper95[h] <- Fcst[h]+1.96*sqrt(FcstVar[h])
    FcstCI$Lower95[h] <- Fcst[h]-1.96*sqrt(FcstVar[h])
  }
}

Temp <- round(c(as.numeric(ts_pc(depVar)[ts_summary(depVar)$end]), as.numeric(Fcst[1]), as.numeric(Fcst[2]), as.numeric(Fcst[3]), as.numeric(Fcst[4])), 1)
Temp <- round(Temp, 2)
Intv <- c("-", paste("[", round(FcstCI$Lower90[1], 1), ", ", round(FcstCI$Upper90[1], 1), "]", sep = ""),
              paste("[", round(FcstCI$Lower90[2], 1), ", ", round(FcstCI$Upper90[2], 1), "]", sep = ""),
              paste("[", round(FcstCI$Lower90[3], 1), ", ", round(FcstCI$Upper90[3], 1), "]", sep = ""),
              paste("[", round(FcstCI$Lower90[4], 1), ", ", round(FcstCI$Upper90[4], 1), "]", sep = ""))
          
Dates <- seq.Date(lastObsDep, length.out = (H+1), by = "quarter")[1:(H+1)]
Tab  <- rbind(t(Temp), t(Intv))
Tab  <- rbind(paste(year(Dates), " Q", quarter(Dates), sep = ""), Tab)

#-------------------------------------------------------------------------------------
# Simulate forecast density for probability of large downturn
#-------------------------------------------------------------------------------------
# Simulate the forecast density assuming that the forecast error is normally distributed
# This implies that y(t+h) ~ N(y(t+h|t), sigh^2), that is, the future value of GDP growth
# is normally distributed with a mean equal to the point forecast and variance equal to the
# forecast error variance.
# Compute forecast error Variance
NSim    <- 5000    # Note that you can test your codes with a small number of simulations to increase speed and calculate the final results with a higher number

SimFcst.h0 <- rnorm(NSim, Fcst[1], sqrt(FcstVar[1]))
SimFcst.h1 <- rnorm(NSim, Fcst[2], sqrt(FcstVar[2]))
SimFcst.h2 <- rnorm(NSim, Fcst[3], sqrt(FcstVar[3]))
SimFcst.h3 <- rnorm(NSim, Fcst[4], sqrt(FcstVar[4]))

# Compute the probability of a negative growth rate
PNeg2020Q1 = mean(SimFcst.h0 < -1)
PNeg2020Q2 = mean(SimFcst.h1 < -1)
PNeg2020Q3 = mean(SimFcst.h2 < -1)
PNeg2020Q4 = mean(SimFcst.h3 < -1)

Tab  <- rbind(Tab, round(c(NaN, PNeg2020Q1, PNeg2020Q2, PNeg2020Q3, PNeg2020Q4), 2))
Tab  <- t(Tab)
Tab[1, 4] <- "-"

print("")
print("")
print("Forecast")
write.table(Tab, sep = "&", eol = "\\\\ ", quote = FALSE,col.names = FALSE, row.names = FALSE, file = paste("../Results/Fcst_", depVarLab, ".tex", sep =""))
colnames(Tab) <- c("Date", "Point forecast", "90% Interval forecast", "P[Growth<-1%]")

# Export nice table for github as png
kable(Tab, "html", caption=paste("f-curve forecast of GDP growth (last observation: ", lastObsDate, ")", sep = "")) %>%
  kable_styling(full_width = FALSE, font_size = 22) %>%
  row_spec(1, color = "black") %>%
  row_spec(c(2:5), color = "blue") %>%
  save_kable(paste("../Results/Fcst_Table_", depVarLab, ".png", sep = ""))