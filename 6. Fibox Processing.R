#### Initial Setup ####

#Check working directory
getwd()

#Set working directory
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data")

#Load packages
library(tidyverse)

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
  file <- file[,c(2,7,9)]
  #for(k in file$Time){
    #if(difftime() > 2)
  #}
}

for(k in file$Time){
  if(difftime() > 2){
    
  }
}

#Make line graph
#ggplot(file) +
#aes(x = delta_t, y = Value) +
#geom_line()

#write.csv()