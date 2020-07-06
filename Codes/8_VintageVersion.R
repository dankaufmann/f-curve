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
#rm(list = ls())
source("AllPackages.R")
endDate   <- Sys.Date()

#-------------------------------------------------------------------------------------
# Get the data
#-------------------------------------------------------------------------------------
load(file="../Data/IndicatorData.RData")
# load(file="../Data/MacroData.RData") # Not necessary in vintages
load(file="../Data/f-curve.RData")

#-------------------------------------------------------------------------------------
# Save all data as a vintage
#-------------------------------------------------------------------------------------
# Create folder if missing
mainDir <- getwd()
outDir <- makeOutDir(mainDir, paste("/../Vintages/", endDate, sep =""))

# Save macro data for comparison
save.image(file = paste(outDir, "/AllData.RData", sep = ""))

# Copy results folder
file.copy("../Results/", outDir, recursive=TRUE)
