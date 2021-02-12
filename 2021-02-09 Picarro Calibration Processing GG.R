##Picarro Calibration Processing

#Written by Genevieve Goebel
#Edited February 5, 2021



#### Initial Setup ####
#Install packages
install.packages("readtext")

#Load packages
library(readtext)
library(ggplot2)
library(ggpubr)
library(dplyr)

#Check working directory
getwd()

#Set working directory
#***CHANGE DIRECTORY TO FOLDER NESTED IN DESKTOP OF COMPUTER IN USE***#
setwd("/Users/goeb_genevieve/Desktop/Thesis/")



#### File Polishing ####
rawCal <- read.table(file.choose(), header = TRUE)
rawCal2 <- read.table(file.choose(), header = TRUE)

#Check column names
names(rawCal)

#List the column names to be kept in modified dataframe
modCal <- rawCal[,c("DATE","TIME", "HP_12CH4", "HP_12CH4_dry", "HP_13CH4", 
                    "Delta_iCH4_Raw", "HP_Delta_iCH4_Raw", "HP_Delta_iCH4_5min", 
                    "X12CO2", "X12CO2_dry", "X13CO2", "Delta_Raw_iCO2", "Delta_5min_iCO2")]
str(modCal)
modCal2 <- rawCal2[,c("DATE","TIME", "HP_12CH4", "HP_12CH4_dry", "HP_13CH4", 
                    "Delta_iCH4_Raw", "HP_Delta_iCH4_Raw", "HP_Delta_iCH4_5min", 
                    "X12CO2", "X12CO2_dry", "X13CO2", "Delta_Raw_iCO2", "Delta_5min_iCO2")]
str(modCal2)

#Create date-time object column
modCal$DT <- paste(modCal$DATE, as.character (modCal$TIME))
modCal$DT <- as.POSIXct(modCal$DT, format = "%Y-%m-%d %H:%M:%S")
modCal2$DT <- paste(modCal2$DATE, as.character (modCal2$TIME))
modCal2$DT <- as.POSIXct(modCal2$DT, format = "%Y-%m-%d %H:%M:%S")



#### Finding Calibration Values ####

#CO2 ~400 ppm
modCal400ppm <- subset(modCal, X12CO2 < 400)

#Line graph for looking at injection curves by concentration
cal400ppm <- ggplot(modCal400ppm, aes(x = DT, y = X12CO2)) + geom_line()
cal400ppm

modCal400ppmtime <- subset(modCal400ppm, DT > "2021-01-27 17:15:00" & DT < "2021-01-27 17:16:00")

#CO2 ~2000 ppm
modCal2000ppm <- subset(modCal2, X12CO2 > 1920)

#Line graph for looking at injection curves by concentration
cal2000ppm <- ggplot(modCal2000ppm, aes(x = DT, y = X12CO2)) + geom_line()
cal2000ppm

modCal2000ppmtime <- subset(modCal2000ppm, DT > "2021-01-27 17:22:00" & DT < "2021-01-27 17:23:10")



#### Calibration Collection ####
finalCal <- data.frame(matrix(ncol = 9, nrow = 2))
names(finalCal) <- c("Standard", "Cert. CH4 Conc.", "Cert. CH4 Iso.", "Cert. CO2 Conc.", "Cert. CO2 Iso.", "CH4 Conc.", "CH4 Iso.", "CO2 Conc.", "CO2 Iso.")
finalCal[1,] <- c("400 ppm", 1.770, -48.4, 399, -8.5, 0, 0, 0, 0)
finalCal[2,] <- c("2000 ppm", 499, -68.6, 2000, -32.5, 0, 0, 0, 0)

#Mean CO2 concentration for 400 ppm standard
meanCO2400ppm <- mean(modCal400ppmtime$X12CO2)
finalCal[1, 8] <- meanCO2400ppm
#Mean CO2 isotope for 400 ppm standard
meanDeltCO2400ppm <- mean(modCal400ppmtime$Delta_Raw_iCO2)
finalCal[1, 9] <- meanDeltCO2400ppm
#Mean CH4 concentration for 400 ppm standard
meanCH4400ppm <- mean(modCal400ppmtime$HP_12CH4)
finalCal[1, 6] <- meanCH4400ppm
#Mean CH4 isotope for 400 ppm standard
meanDeltCH4400ppm <- mean(modCal400ppmtime$HP_Delta_iCH4_Raw)
finalCal[1, 7] <- meanDeltCH4400ppm

#Mean CO2 concentration for 2000 ppm standard
meanCO22000ppm <- mean(modCal2000ppmtime$X12CO2)
finalCal[2, 8] <- meanCO22000ppm
#Mean CO2 isotope for 400 ppm standard
meanDeltCO22000ppm <- mean(modCal2000ppmtime$Delta_Raw_iCO2)
finalCal[2, 9] <- meanDeltCO22000ppm
#Mean CH4 concentration for 400 ppm standard
meanCH42000ppm <- mean(modCal2000ppmtime$HP_12CH4)
finalCal[2, 6] <- meanCH42000ppm
#Mean CH4 isotope for 400 ppm standard
meanDeltCH42000ppm <- mean(modCal2000ppmtime$Delta_iCH4_Raw)
finalCal[2, 7] <- meanDeltCH42000ppm



#### Export ####
write.csv(finalCal, "2021-01-27 Picarro Calibration.csv")

