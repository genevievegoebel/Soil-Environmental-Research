#### Initial Setup ####

#Install packages
install.packages("tidyverse")
install.packages("digest")

#Load packages
library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)
library(readr)
library(digest)

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/")

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

begin <- as.POSIXct("2019-06-12 00:00:01")
alldata$DT <- as.numeric(alldata$DateTime)-as.numeric(begin)

#Rolling regression to get flux slopes for each 5-10 data point period
FUNbigrr <- function(Z){
  mod=summary(lm(formula=Value~DT, data=as.data.frame(Z))); 
  return (cbind(format((coef(mod)[2]), scientific = FALSE), format(mod$r.squared, scientific = FALSE)))
}

bigrr <- rollapply(alldata[, c(2,4)], width=5, FUN = FUNbigrr, by.column=FALSE, fill=NA, align="right")
bigrrdata1 <- cbind(alldata, bigrr)

names(bigrrdata1) <- c("delta_t", "Value", "DateTime", "DT", "slope", "r.sqr")
str(bigrrdata1)
bigrrdata1$slope <- as.numeric(as.character(bigrrdata1$slope))
bigrrdata1$r.sqr <- as.numeric(as.character(bigrrdata1$r.sqr))
str(bigrrdata1)

#Flagging with ifelse() function should work because it's vectorized, but it is not
bigrrs0 <- vector(mode = "numeric", length = length(bigrrdata1$slope))
bigrrs0 <- ifelse(bigrrdata1$slope < 0.02 & bigrrdata1$slope > -0.02 & bigrrdata1$r.sqr > 0.9, 21.5, 20)

#Rolling regression to mark where slopes are less than 15% different from the mean slope
FUNbigs0 = function(G){
  test2 = ifelse(G[,5] < 0.05 && G[,5] > -0.05 && G[,6] > 0.75, 20.4, 20);
  return (test2)
}
bigrrs0 <- rollapply(bigrrdata1, width=10, FUN = FUNbigs0, by.column=FALSE, fill=NA, align="right") 

bigrrdata2 <- cbind(bigrrdata1, bigrrs0)

#### Indexing ####

#Give each section where the slope is consistent an unique number to allow the use of apply
biglength <- as.numeric(length(bigrrdata2$bigrrs0))
bigindex <- vector(mode = "numeric", length = biglength)
counter = 1

for(i in 15:biglength){
  if((bigrrdata2$bigrrs0[i])>(bigrrdata2$bigrrs0[i-1])){
    bigindex[i]=counter
    counter=counter+1
  } else if((bigrrdata2$bigrrs0[i])==(bigrrdata2$bigrrs0[i-1])){
    bigindex[i] = bigindex[i-1]
  } else{
    bigindex[i]=NA
  }
}

bigrrdata2 <- cbind(bigrrdata2, bigindex)
max(bigrrdata2$bigindex, na.rm = TRUE)

#See how well fluxes were ID'd with the method
ggplot(bigrrdata2) +
  geom_line(aes(x=DT, y=Value), color="black")+
  geom_point(aes(x=DT, y=bigrrs0), color="blue")+ #peaks ID'd with calculus
  scale_y_continuous(limits = c(19.75, 22)) #set scale for CO2

bigrrdata2 %>%
  group_by(bigindex) %>%
  summarize(bigfiboxaverage = format(mean(Value, na.rm=TRUE), digits = 5)) %>%
  filter(!is.na(bigindex), !bigindex==0)














#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/2019-6-12 CCASE/Injections")

#### Test file processing ####

testfile <- read.csv("9_O2.csv", skip = 1, header = TRUE)
testfile <- testfile[-nrow(testfile),]
#Creates new column with date-time objects by joining date and time columns
testfile$DateTime <- as.POSIXct(paste(testfile$Date, testfile$Time), format = "%m/%d/%Y %H:%M:%S")
#Narrows dataframe down to the columns needed for processing
testfile <- testfile[,c(7,9,56)]

begin<-as.POSIXct("2019-06-12 00:00:01")
testfile$DT<-as.numeric(testfile$DateTime)-as.numeric(begin)

#Rolling regression to get flux slopes for each 5 data point period
FUNrr <- function(K){
  mod=summary(lm(formula=Value~DT, data=as.data.frame(K))); 
  return (cbind(format((coef(mod)[2]), scientific = FALSE), format(mod$r.squared, scientific = FALSE)))
}

rr <- rollapply(testfile[,c(2,4)], width=5, FUN = FUNrr, by.column=FALSE, fill=NA, align="right")
rrdata1 <- cbind(testfile, rr)

names(rrdata1) <- c("delta_t", "Value", "DateTime", "DT", "slope", "r.sqr")
str(rrdata1)
rrdata1$slope <- as.numeric(as.character(rrdata1$slope))
rrdata1$r.sqr <- as.numeric(as.character(rrdata1$r.sqr))
str(rrdata1)

#Flagging with ifelse() function should work because it's vectorized, but it is not
rrs0 <- vector(mode = "numeric", length = length(rrdata1$slope))
rrs0 <- ifelse(rrdata1$slope < 0.05 & rrdata1$slope > -0.05 & rrdata1$r.sqr > 0.75, 20.4, 20)

#Rolling regression to mark where slopes are less than 15% different from the mean slope
FUNs0 = function(V){
  test3 = ifelse(V[,5] < 0.05 && V[,5] > -0.05 && V[,6] > 0.75, 20.4, 20);
  return (test3)
}
rrs0 <- rollapply(rrdata1, width=10, FUN = FUNs0, by.column=FALSE, fill=NA, align="right") 

rrdata2 <- cbind(rrdata1, rrs0)

#### Test file indexing ####

#Give each section where the slope is consistent an unique number to allow the use of apply
length <- as.numeric(length(rrdata2$rrs0))
index <- vector(mode = "numeric", length = length)
counter = 1
for(i in 15:length){
  if((rrdata2$rrs0[i])>(rrdata2$rrs0[i-1])){
    index[i]=counter
    counter=counter+1
  } else if((rrdata2$rrs0[i])==(rrdata2$rrs0[i-1])){
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
  scale_y_continuous(limits = c(19.75, 21.25)) #set scale for CO2

rrdata2 %>%
  group_by(index) %>%
  summarize(fiboxaverage = format(mean(Value, na.rm=TRUE), digits = 5)) %>%
  filter(!is.na(index), !index==0)










#Directs program to processing key file = "yyyy-m-dd/processed measurement type"
processeddata <- read.csv("2019-6-12 Processed Injection Data.csv")
