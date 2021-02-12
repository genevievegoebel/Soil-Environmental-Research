# # # # # # # # # # # # # # # # # # # # # #
#      Mayfly Logger Processing Code      #
#          GG March 16-20, 2020           #
# # # # # # # # # # # # # # # # # # # # # #

#Change as appropriate for computer in use
setwd("/Users/goeb_genevieve/Desktop/Thesis/Data/Data Analysis/Logger Processing/")
getwd()


#### Polished Sensor Key ####
#Bring in logger key file (May be named "SensorID.csv")
loggerKey <- read.csv("SWaNLoggerSensorKey.csv", header=TRUE)

loggerKey$ID = paste(loggerKey$Box, loggerKey$Sensor, sep = "_", collapse = NULL)
loggerKey$ID <- as.factor(loggerKey$ID)
str(loggerKey)

#### Logger 1 ####

#If already made
logger1 <- read.csv("2019 Logger 1 Compiled.csv", header=TRUE) 

#logger1 <- read.delim("2019 Compile Logger 1.txt", header = FALSE)
#logger1 <- logger1[-c(1:2),-c(2,4,6,8)]

#names(logger1) <- c("DateTime", "SensorID", "VWC", "Temp", "BEC")
#logger1$Date <- format(as.POSIXct(strptime(logger1$DateTime, "%Y-%m-%d %H:%M:%S", tz="")), format = "%Y-%m-%d")
#logger1$Time <- format(as.POSIXct(strptime(logger1$DateTime, "%Y-%m-%d %H:%M:%S", tz="")), format = "%H:%M:%S")
#logger1 <- logger1[,-1]

#logger1$Box <- 1

#logger1$ID = paste(logger1$Box, logger1$SensorID, sep = "_", collapse = NULL)
#logger1$ID <- as.factor(logger1$ID)
#str(logger1)

#logger1 <- join(logger1, loggerKey, by="ID")

#write.csv(logger1, "2019 Logger 1 Compiled.csv")




#### Logger 2 ####

#If already made
logger2 <- read.csv("2019 Logger 2 Compiled.csv", header=TRUE)

#logger2 <- read.delim("2019 Compile Logger 2.txt", header = FALSE)
#logger2 <- logger2[-c(1:2),-c(2,4,6,8)]

#names(logger2) <- c("DateTime", "SensorID", "VWC", "Temp", "BEC")
#logger2$Date <- format(as.POSIXct(strptime(logger2$DateTime, "%Y-%m-%d %H:%M:%S", tz="")), format = "%Y-%m-%d")
#logger2$Time <- format(as.POSIXct(strptime(logger2$DateTime, "%Y-%m-%d %H:%M:%S", tz="")), format = "%H:%M:%S")
#logger2 <- logger2[,-1]

#logger2$Box <- 2

#logger2$ID = paste(logger2$Box, logger2$SensorID, sep = "_", collapse = NULL)
#logger2$ID <- as.factor(logger2$ID)
#str(logger2)

#logger2 <- join(logger2, loggerKey, by="ID")

#logger2$Date = as.Date(logger2$Date, format = "%m/%d/%y") 

#write.csv(logger2, "2019 Logger 2 Compiled.csv")

 
#### Compiled Loggers ####
#Combine polished logger dataframes
loggerdata <- rbind(logger1, logger2)

#Correct raw VWC value
loggerdata$fixedVWC=3.879*10^-4*loggerdata$VWC-0.6956

#Create dataframe temp and VWC data by plot, depth, and day; organize dataframe
logger_day = aggregate(cbind(Temp,fixedVWC) ~ Plot+Depth+Treatment+Date, data=loggerdata, FUN="mean") 
logger_day$Date = as.Date(logger_day$Date, format = "%m/%d/%y") 
logger_day$Plot=as.factor(logger_day$Plot)
logger_day = logger_day[order(logger_day$Date),]

#write.csv(logger_day, "2019 Processed Logger Data.csv")





#Overlay Mel's data to check variability

logger_day$Depth <- as.factor(as.character(logger_day$Depth))

#### Moisture Graphs ####
#Graph soil moisture over time by plot
ggplot(logger_day, aes(y=fixedVWC, x=Date, col=Plot, pch=Depth)) + 
  geom_point(size=3) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("Date")))+
  ylab("VWC") +
  theme_bw()+
  theme(axis.text.x=element_text(size=16, colour="black"),
        axis.text.y=element_text(size=16, colour="black"),
        axis.title.x=element_text(size=18, colour="black"),
        axis.title.y=element_text(size=18, colour="black"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        #legend.position="none",
        axis.line=element_line(colour="black"))

#Graph soil moisture over time by treatment
ggplot(logger_day, aes(y=fixedVWC, x=Date, col=Treatment, pch=Depth)) + 
  geom_point(size=3) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("Date")))+
  ylab("VWC") +
  theme_bw()+
  theme(axis.text.x=element_text(size=16, colour="black"),
        axis.text.y=element_text(size=16, colour="black"),
        axis.title.x=element_text(size=18, colour="black"),
        axis.title.y=element_text(size=18, colour="black"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        #legend.position="none",
        axis.line=element_line(colour="black"))


#### Temperature Graphs ####
#Graph soil temperature over time by plot
ggplot(logger_day, aes(y=Temp, x=Date, col=Plot, pch=Depth)) + 
  geom_point(size=3) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("Date")))+
  ylab("Temp") +
  theme_bw()+
  theme(axis.text.x=element_text(size=16, colour="black"),
        axis.text.y=element_text(size=16, colour="black"),
        axis.title.x=element_text(size=18, colour="black"),
        axis.title.y=element_text(size=18, colour="black"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        #legend.position="none",
        axis.line=element_line(colour="black"))

#Graph soil temperature over time by treatment
ggplot(logger_day, aes(y=Temp, x=Date, col=Treatment, pch=Depth)) + 
  geom_point(size=3) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("Date")))+
  ylab("Temp") +
  theme_bw()+
  theme(axis.text.x=element_text(size=16, colour="black"),
        axis.text.y=element_text(size=16, colour="black"),
        axis.title.x=element_text(size=18, colour="black"),
        axis.title.y=element_text(size=18, colour="black"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        #legend.position="none",
        axis.line=element_line(colour="black"))
