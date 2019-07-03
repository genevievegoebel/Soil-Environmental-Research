#### Initial Setup ####

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data")

#Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)

#### Find Files ####

#Directs program to path = "yyyy-m-dd/measurement type" and reads all file names by pattern = ".suffix"
files <- list.files(path = "2019-6-12 CCASE/Injections", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
files

#### Processing ####

#Considers every .csv in list of files from the specified folder
i=0
for(i in files){
  file <- read.csv(i, skip = 1, header = TRUE)
  #Creates new column with date-time objects by joining date and time columns
  file$DateTime <- as.POSIXct(paste(file$Date, file$Time), format = "%m/%d/%Y %H:%M:%S")
  #Narrows dataframe down to the columns needed for processing
  file <- file[,c(7,9,56)]
  #Deletes last row in file
  file <- file[-nrow(file),]
  #Initializes a vector to keep track of where to constrain measurements based on time gaps
  cropVector <- vector(mode="logical", length=0)
  #Looks for time gaps of more than 2 seconds
  j=0
  k=0
  for(j in 2:length(file$DateTime)){
    diffTime <- as.integer(difftime(file$DateTime[j], file$DateTime[j-1]))
    if(diffTime>2){
      #Counter to check if program is finding time gaps in the folder's files
      k=k+1
      #Adds row # to vector to record the bounds of a single measurement window
      cropVector <- c(cropVector, j)
      #Print row where time gap is found
      print(cropVector)
    }
  }
  if(k==0){
    graphData1 <- file
  } else if(k==1){
    graphData1 <- file[1:cropVector[1]-1,]
    graphData2 <- file[cropVector[1]:sum(complete.cases(file)),]
  } else if(k==2){
    graphData1 <- file[1:cropVector[1]-1,]
    graphData2 <- file[cropVector[1]:cropVector[2]-1,]
    graphData3 <- file[cropVector[2]:sum(complete.cases(file)),]
  }
  #Make line graphs
  plot1 <- ggplot(data = graphData1, aes(x = delta_t, y = Value, group = 1)) +
    geom_line() +
    geom_point()
  plot2 <- ggplot(data = graphData2, aes(x = delta_t, y = Value, group = 1)) +
    geom_line() +
    geom_point()
  plot3 <- ggplot(data = graphData3, aes(x = delta_t, y = Value, group = 1)) +
    geom_line()
}
plot1
plot2
plot3

#write.csv()