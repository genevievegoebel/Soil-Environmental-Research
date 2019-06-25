#### Initial Setup ####

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data")

#Load packages
library(tidyverse)
library(dplyr)

#### Find Files ####

#Directs program to path = "yyyy-m-dd/measurement type" and reads all file names by pattern = ".suffix"
files <- list.files(path = "2019-6-12 CCASE/Injections", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
files

#### Processing ####
i=0
j=0
k=0

for(i in files){
  file <- read.csv(i, skip = 1, header = TRUE)
  file$DateTime <- as.POSIXct(paste(file$Date, file$Time), format = "%m/%d/%Y %H:%M:%S")
  file <- file[,c(7,9,56)]
  file <- file[-nrow(file),]
  for(k in 2:length(file$DateTime)){
    diffTime <- as.integer(difftime(file$DateTime[k], file$DateTime[k-1]))
    if(diffTime>2){
      print("Found time gap")
    }
  }
}

#Make line graph
#ggplot(file) +
#aes(x = delta_t, y = Value) +
#geom_line()

#write.csv()