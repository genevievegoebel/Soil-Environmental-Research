#### Initial Setup ####

#Install packages
install.packages("digest")

#Load packages
library(tidyverse)
library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(reshape2)
library(readr)
library(data.table)
library(digest)

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/2019-6-12 CCASE/Injections")

#### Find Files ####

#Directs program to path = "yyyy-m-dd/measurement type" and reads all file names by pattern = ".suffix"
files <- list.files(path = "2019-6-12 CCASE/Injections", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
files

#### Processing ####

i=0
j=0
k=0
m=0
alldata=NULL

#Body of loop for consolidating raw data
for(i in files){
  file <- read.csv(i, skip = 1, header = TRUE)
  #Deletes last annotation row in file 
  file <- file[-nrow(file),]
  #Creates new column with date-time objects by joining date and time columns
  file$DateTime <- as.POSIXct(paste(file$Date, file$Time), format = "%m/%d/%Y %H:%M:%S")
  #Narrows dataframe down to the columns needed for processing
  file <- file[,c(7,9,56)]
  #Considers every .csv in list of files from the specified folder and combines into one dataframe
  alldata <- rbind(alldata, file)
}










testfile <- read.csv("9_O2.csv", skip = 1, header = TRUE)
testfile <- testfile[-nrow(testfile),]
#Creates new column with date-time objects by joining date and time columns
testfile$DateTime <- as.POSIXct(paste(testfile$Date, testfile$Time), format = "%m/%d/%Y %H:%M:%S")
#Narrows dataframe down to the columns needed for processing
testfile <- testfile[,c(7,9,56)]

str(testfile)
begin<-as.POSIXct("2019-06-12 00:00:01")
testfile$DT<-as.numeric(testfile$DateTime)-as.numeric(begin)

#Rolling regression to get flux slopes for each 5 data point period
FUNrr <- function(Z){
  mod=summary(lm(formula=Value~DT, data=as.data.frame(Z))); 
  return (cbind((coef(mod)[2]), mod$r.squared))
}

rr <- rollapply(testfile[, c(2,4)], width=5, FUN = FUNrr, by.column=FALSE, fill=NA, align="right")
rrdata1 <- cbind(testfile, rr)

#Rolling regression to mark where slopes are less than 15% different from the mean slope
FUNsd = function(V){
  test2 = ifelse(V[,5] < 0.1 && V[,5] > -0.1 && V[,6] > 0.9, 20.4, 20);
  return (test2)
}

rrs0 <- rollapply(rrdata1, width=5, FUN = FUNsd, by.column=FALSE, fill=NA, align="right") 
rrdata2 <- cbind(rrdata1, rrs0)









#Give each section where the slope is consistent an unique number to allow the use of apply
length <- as.numeric(length(rrdata2$rrs0))
index <- vector(mode = "numeric", length = length)
counter = 1

str(rrdata2)
for(i in 10:length){
  if(rrdata2$rrs0[i] > rrdata2$rrs0[i-1]){
    index[i]=counter
    counter=counter+1
  } else if(rrdata2$rrs0[i] == rrdata2$rrs0[i-1]){
      index[i] = index[i-1]
    } else{
      index[i]=NA
    }
}

rrdata2 <- cbind(rrdata2, index)

#See how well fluxes were ID'd with the method
ggplot(rrdata2) +
  geom_line(aes(x=DT, y=Value), colour="black")+
  geom_point(aes(x=DT, y=rrs0), color="blue")+ #peaks ID'd with calculus
  geom_point(aes(x=DT, y=index), color="green")+
  scale_y_continuous(limits = c(19.75, 21.25)) #set scale for CO2










#Initializes a vector to keep track of where to constrain measurements based on time gaps
cropper <- vector(mode="logical", length=0)
cropper[1] <- FALSE

#Loop that looks for time gaps of more than 2 seconds (for indexing)
for(j in 2:length(alldata$DateTime)){
  diffTime <- as.integer(difftime(alldata$DateTime[j], alldata$DateTime[j-1]))
  if(diffTime>2){
    #Adds row # to vector to record the bounds of measurement windows
    cropper[j] <- TRUE
  }
  else{
    cropper[j] <- FALSE
  }
}
alldata <- cbind(alldata, cropper)










#Directs program to processing key file = "yyyy-m-dd/processed measurement type"
dataProcessed <- read.csv("2019-6-12 Processed Injection Data.csv")

#Body of loop for processing notebook key and data (.csv should be made manually prior to attempting this)



  #Make line graphs
  ggplot(data = alldata, aes(x = delta_t, y = Value, group = 1)) +
    geom_line() +
    geom_point()
  
  ggplot(data = alldata, aes(x = DateTime, y = Value)) +
    geom_line() +
    geom_point(aes(color = cropper))
