# # # # # # # # # # # # # # # # # # # # # #
#     Fiber-Optic Data Processing Code    #
#            GG January 22, 2020          #
# # # # # # # # # # # # # # # # # # # # # #

#### Initial Setup ####

#Install packages
install.packages("tidyverse")
install.packages("digest")
install.packages("data.table")
install.packages("anytime")

#Load packages
library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)
library(readr)
library(digest)
library(data.table)
library(anytime)

#Check working directory
getwd()

#Set working directory
#***CHANGE DIRECTORY TO FOLDER NESTED IN DESKTOP OF COMPUTER IN USE***#
#***MUST CONTAIN ALL RAW DATA FILES (BOTH INJECTIONS AND STANDARDS) PLUS THE PROCESSED DATA FILE***#
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/Fibox/Averages Processing/SWaN/")

#### Functions ####

#For rolling regression
FUNrr <- function(K){
  mod=summary(lm(formula=Value~DT, data=as.data.frame(K))); 
  return (cbind(format((coef(mod)[2]), scientific = FALSE), format(mod$r.squared, scientific = FALSE)))
}

#For testing for slopes close to 0
FUNs0 = function(V){
  test = ifelse(V[,5] < 0.05 && V[,5] > -0.05 && V[,6] > 0.8, 21.5, 20);
  return (test)
}

#### File Finder ####
#Directs program to path = "yyyy-m-dd/measurement type" and reads all file names by pattern = ".suffix"
#***MUST CHANGE PATTERN TO "*_O2" ON WINDOWS COMPUTERS AND "*.csv" FOR MACS***#
files <- list.files(path = "2020-11-19 SWaN", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
#View list of file names
files

#### Processing ####

#Initialize numeric objects for looping and counting
counter = 1

#Sets time for all datetime objects to be relative to, this way they can be used for arithmetic
#***CHANGE TO 1ST SECOND OF MEASUREMENT DATE***#
begin <- as.POSIXct("2020-11-19 00:00:01")

#Creates empty vector and dataframe to extract and collect desired measurement segments
choose <- vector(mode = "numeric")
selected <- data.frame(matrix(ncol = 8, nrow = 0))

#Body of loop for looking at files one at a time
i=0
masterfile <- data.frame(matrix(ncol = 55, nrow = 0))
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
  file$group <- NA
  
  #Creates one large masterfile
  masterfile <- rbind(masterfile, file)
  orderedMaster <- masterfile[order(as.Date(masterfile$DateTime)),]
  
  #Looks for time gaps of more than 2 seconds
  j=0
  k=1
  for(j in 2:length(file$DateTime)){
    diffTime <- as.integer(difftime(file$DateTime[j], file$DateTime[j-1]))
    if(diffTime>2){
      k = k+1
    }
    file$group[j] <- k
  }
  file$group[1] = 1
  
  l=0
  for(l in 1:k){
    if(k==1){
      timesubset <- file
    } else{
      timesubset <- subset(file, group == l)
    }
    
    #Rolling regression to get flux slopes for each 3 data point period (uses FUNrr)
    rr <- rollapply(timesubset[,c(1,3)], width=3, FUN = FUNrr, by.column=FALSE, fill=NA, align="right")
    rrdata1 <- cbind(timesubset, rr)
    #Name each column for easy reference
    if(nrow(rrdata1)>1){
      names(rrdata1) <- c("Value", "DateTime", "DT", "group", "slope", "r.sqr")
    } else {
      names(rrdata1) <- c("Value", "DateTime", "DT", "group", "slope")
    }
    #Change object type of slope to numeric
    rrdata1$slope <- as.numeric(as.character(rrdata1$slope))
    #Change object type of r.sqr to numeric if it exists in the current selection of data
    if("r.sqr" %in% colnames(rrdata1)){
      rrdata1$r.sqr <- as.numeric(as.character(rrdata1$r.sqr))
    }
    
    #Rolling average to mark where slopes are as close to 0 as possible (uses FUNs0)
    rrs0 <- rollapply(rrdata1, width=5, FUN = FUNs0, by.column=FALSE, fill=NA, align="right") 
    rrdata2 <- cbind(rrdata1, rrs0)
    
    #### Indexing ####
    
    #Initialize vector for indexes and later merging to main dataframe
    length <- as.numeric(length(rrdata2$rrs0))
    index <- vector(mode = "numeric", length = length)
    index <- NA
    
    if(nrow(rrdata2)>8){
      #Loop to give each section where the slope is consistent an index number for later selection
      m=0
      for(m in 8:length){
        if((rrdata2$rrs0[m])>(rrdata2$rrs0[m-1])){
          index[m]=counter
          counter=counter+1
        } else if((rrdata2$rrs0[m])==(rrdata2$rrs0[m-1])){
          index[m] = index[m-1]
        } else{
          index[m]=NA
        }
      }
    }
    rrdata3 <- cbind(rrdata2, index)
    rrdata3$rrs0 <- as.numeric(rrdata3$rrs0)
    rrdata3$index <- as.numeric(rrdata3$index)
    
    padding <- 2
    lower_lim <- min(rrdata3$Value, na.rm = TRUE) - padding
    upper_lim <- max(rrdata3$Value, na.rm = TRUE) + padding
    
    #See how well fluxes were ID'd with the method
    filegraph <- ggplot(rrdata3)+
      geom_line(aes(x=DT, y=Value), color="black")+
      geom_point(aes(x=DT, y=rrs0))+ #level segments ID'd with calculus
      geom_text(aes(x=DT, y=rrs0), vjust=-2, label=index, color="blue")+
      scale_y_continuous(limits = c(lower_lim, upper_lim)) #set scale for CO2
    print(filegraph)
    
    #Asks user to enter numbers corresponding to best indexed segment(s)
    prompt1 <- "Which segments are usable? Please enter as a single #."
    choose <- as.integer(strsplit(readline(prompt1), " ")[[1]])
    
    #Collects rows of main dataframe as subsets based on best indexes and stores them in another dataframe
    n=0
    for(n in 1:length(choose)){
      selection <- subset(rrdata3, index==choose[n])
      selected <- rbind(selected, selection)
    }
  }
}

write.csv(selected, "2020-11-19 Selected Segments.csv")

#Creates time column to match time column from manually entered key
selected$Time <- strftime(as.character(selected$DateTime), format = "%H:%M")

timeindex <- selected %>%
  group_by(index) %>%
  mutate(rownum = row_number()) %>%
  group_by(index) %>%
  filter(rownum == min(rownum)) %>%
  select(index, realtime= DateTime, timeindex = Time)

selected <- left_join(selected, timeindex) %>%
  select(-Time, -DateTime)

#### Calculating ####

#Calculates averages based on groupings by index
averages <- selected %>%
  group_by(index, timeindex, realtime) %>%
  summarize(fiboxaverage = format(mean(Value, na.rm=TRUE), digits = 5)) %>%
  filter(!is.na(index) & index!=0)
averages <- averages[order(averages$realtime),]

avgdupes <- unique(averages$timeindex[duplicated(averages$timeindex)])

#### Assigning ####

#Directs program to processing key file = "yyyy-m-dd/processed measurement type"
processeddata <- read_csv("2020-11-19 processed data.csv", col_types = "cfffcddddc")
#Remove notes column
#processeddata <- processeddata[,-10]
#Creates empty column for the assigned averages to be entered into
processeddata$FiboxAv <- NA
#Changes Time column to date/time object for future manipulation
processeddata$numTime <- parse_time(processeddata$Time, format = "%H:%M")
#Reorders processeddata by time of measurement as it was entered from the field notebook
processeddata <- processeddata[order(processeddata$numTime),]
#Deletes the numeric time column after it's used to reorder the dataframe
processeddata <- processeddata[,-c(11)]

pddupes <- unique(processeddata$Time[duplicated(processeddata$Time)])

#Goes through every unique ID# in the "processeddata" file
for(o in 1:length(processeddata$File)) {
  #Checks to make sure nothing has been entered into the fibox average column before continuing
  if(is.na(processeddata$FiboxAv[o]) == TRUE){
    #Goes through every index in averages one at a time for comparison to each ID# in the outer loop
    for(p in 1:length(averages$index)) {
      #Goes through each duplicate timeindex to prevent more than one ID# recieve the same average value
      for(q in 1:length(pddupes)){
        for(r in 1:length(avgdupes)){
          #Confirms that the processeddata entry has an average value with a matching timeindex
          if(processeddata$Time[o] == averages$timeindex[p]){
            #Checks if the measurement time and average timeindex are both unique
            if(processeddata$Time[o] != pddupes[q] && averages$timeindex[p]!= avgdupes[r]){
              #Assigns processeddata entry at o with average value at p
              processeddata$FiboxAv[o] <- averages$fiboxaverage[p]
              #Checks if the measurement time is a duplicate, but the average timeindex is unique
            } else if(processeddata$Time[o] == pddupes[q] && averages$timeindex[p]!= avgdupes[r]){
              #Assign the average value at p to the first of the two duplicate realtime value entries in proccesseddata
              processeddata$FiboxAv[o] <- averages$fiboxaverage[p]
              #Leave the second of the two duplicate realtime value entries in proccesseddata blank
              processeddata$FiboxAv[o+1] <- ""
              #Checks if the measurement time is unique, but the average timeindex is a duplicate
            } else if(processeddata$Time[o] != pddupes[q] && averages$timeindex[p]== avgdupes[r]){
              #Assign average value at p to lesser realtime value entry in proccesseddata at o
              processeddata$FiboxAv[o] <- averages$fiboxaverage[p]
              #Assign average value at p+1 to processeddata o+1
              processeddata$FiboxAv[o+1] <- averages$fiboxaverage[p+1]
              #Checks to see if the measurement time and the average time index are both unique
            } else if(processeddata$Time[o] == pddupes[q] && averages$timeindex[p]== avgdupes[r]){
              #Assign the average value at p to the first of the two duplicate realtime value entries in proccesseddata
              processeddata$FiboxAv[o] <- averages$fiboxaverage[p]
              #Assign the next average value with the same time to the second of the two duplicate realtime value entries in proccesseddata
              processeddata$FiboxAv[o+1] <- averages$fiboxaverage[p+1]
            }
          }
        }
      }
    }
  }
  if(is.na(processeddata$FiboxAv[o]) == TRUE){
    processeddata$FiboxAv[o] <- ""
  }
}



#BE ADVISED THAT THE NESTED LOOPS TAKE A WHILE TO COMPLETE, PLEASE ALLOW IT TO FINISH
#THIS WILL BE INDICATED IN THE CONSOLE PANE WHEN THE STOP SIGN ICON VANISHES FROM THE TOP RIGHT CORNER



#### Exporting ####
#Make sure the date in the desired title is correct
write.csv(processeddata, "2020-11-19 Processed Data Output.csv")
