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
library(data.table)

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/Fibox/")

#### File Finder ####

#Directs program to path = "yyyy-m-dd/measurement type" and reads all file names by pattern = ".suffix"
files <- list.files(path = "2019-6-4 DOF/Injections", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
#View list of file names
files

#### Functions ####

#For rolling regression
FUNrr <- function(K){
  mod=summary(lm(formula=Value~DT, data=as.data.frame(K))); 
  return (cbind(format((coef(mod)[2]), scientific = FALSE), format(mod$r.squared, scientific = FALSE)))
}

#For testing for slopes close to 0
FUNs0 = function(V){
  test = ifelse(V[,4] < 0.05 && V[,4] > -0.05 && V[,5] > 0.8, 21.5, 20);
  return (test)
}

#### Processing ####

#Initialize numeric objects for looping and counting
i=0
j=0
counter = 1

#Sets time for all datetime objects to be relative to, this way they can be used for arithmetic
begin <- as.POSIXct("2019-06-12 00:00:01")

#Creates empty vector and dataframe to extract and collect desired measurement segments
choose <- vector(mode = "numeric")
selected <- data.frame(matrix(ncol = 8, nrow = 0))

#Body of loop for looking at files one at a time
for(i in files){
  file <- read.csv(i, skip = 1, header = TRUE)
  #Deletes last 'annotation' row in file 
  file <- file[-nrow(file),]
  #Creates new column with date-time objects by joining date and time columns
  file$DateTime <- as.POSIXct(paste(file$Date, file$Time), format = "%m/%d/%Y %H:%M:%S")
  #Narrows dataframe down to the columns needed for processing (Value and DateTime)
  file <- file[,c(9,56)]
  #Creates column to convert DateTime to a number that's easy to do math with
  file$DT <- as.numeric(file$DateTime)-as.numeric(begin)
  
  #Rolling regression to get flux slopes for each 5 data point period (uses FUNrr)
  rr <- rollapply(file[,c(1,3)], width=5, FUN = FUNrr, by.column=FALSE, fill=NA, align="right")
  rrdata1 <- cbind(file, rr)
  #Name each column for easy reference
  names(rrdata1) <- c("Value", "DateTime", "DT", "slope", "r.sqr")
  #Change object type of slope and r-squared to numeric
  rrdata1$slope <- as.numeric(as.character(rrdata1$slope))
  rrdata1$r.sqr <- as.numeric(as.character(rrdata1$r.sqr))
  
  #Rolling average to mark where slopes are as close to 0 as possible (uses FUNs0)
  rrs0 <- rollapply(rrdata1, width=10, FUN = FUNs0, by.column=FALSE, fill=NA, align="right") 
  rrdata2 <- cbind(rrdata1, rrs0)
  
  #### Indexing ####
  
  #Initialize vector for indexes and later merging to main dataframe
  length <- as.numeric(length(rrdata2$rrs0))
  index <- vector(mode = "numeric", length = length)
  index <- NA
  #Loop to give each section where the slope is consistent an index number for later selection
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
  rrdata3 <- cbind(rrdata2, index)
  
  #See how well fluxes were ID'd with the method
  filegraph <- ggplot(rrdata3)+
    geom_line(aes(x=DT, y=Value), color="black")+
    geom_point(aes(x=DT, y=rrs0))+ #level segments ID'd with calculus
    geom_text(aes(x=DT, y=rrs0), vjust=-2, label=index, color="blue")+
    scale_y_continuous(limits = c(19, 22.5)) #set scale for CO2
  print(filegraph)
  
  #Asks user to enter numbers corresponding to best indexed segment(s)
  prompt1 <- "Which segments are usable? Please enter as #s separated by a space."
  choose <- as.integer(strsplit(readline(prompt1), " ")[[1]])
  
  #Collects rows of main dataframe as subsets based on best indexes and stores them in another dataframe
  for(j in 1:length(choose)){
    selection <- subset(rrdata3, index==choose[j])
    selected <- rbind(selected, selection)
  }
}

#Creates time column to match time column from manually entered key
selected$Time <- strftime(selected$DateTime, format = "%H:%M")

timeindex <- selected %>%
  group_by(index) %>%
  mutate(rownum = row_number()) %>%
  group_by(index) %>%
  filter(rownum == max(rownum)) %>%
  select(index, timeindex = Time)

selected <- left_join(selected, timeindex) %>%
  select(-Time)

#Calculates averages based on groupings by index
averages <- selected %>%
  group_by(index, timeindex) %>%
  summarize(fiboxaverage = format(mean(Value, na.rm=TRUE), digits = 5)) %>%
  filter(!is.na(index) & index!=0)

#Directs program to processing key file = "yyyy-m-dd/processed measurement type"
processeddata <- read_csv("2019-6-4 Processed Injection Data.csv", col_types = "dffcdddddl")
processeddata <- processeddata[,-c(7, 8, 10)]

test <- left_join(processeddata, averages, by = c("Time" = "timeindex"))

dupes <- unique(test$Time[duplicated(test$Time)])
dupes

write.csv(test, "Processed 2019-6-4 Test.csv")
