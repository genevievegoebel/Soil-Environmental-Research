#### Initial Setup ####

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data")

#Load packages
library(tidyverse)
library(lubridate)

#### Find Files ####

#Directs program to path = "yyyy-m-dd/measurement type" and reads all file names by pattern = ".suffix"
files <- list.files(path = "2019-5-16 CCASE/Injections", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
files

#### Processing ####
i=0
j=0
k=0

for(i in files){
  file <- read.csv(i, skip = 1, header = TRUE)
  file$DateTime <- as.POSIXct(paste(file$Date, file$Time), format = "%m/%d/%Y %H:%M:%S")
  file <- file[,c(7,9,56)]
  file <- file[-c(23, 24, 48, 49),]
}

for(k in file$DateTime){
  k1 <- strptime(k, "%Y-%m-%d %H:%M:%0S")
  k2 <- strptime((k+1), "%Y-%m-%d %H:%M:%0S")
  if(as.numeric(k2-k1, units = "secs") > 2){
    print(k)
    #stop <- k
  }
}

#Make line graph
#ggplot(file) +
#aes(x = delta_t, y = Value) +
#geom_line()

#write.csv()