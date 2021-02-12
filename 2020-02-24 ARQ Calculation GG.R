##Harvard Forest Gas Well Data Processing and ARQ Calculation

#Written by Genevieve Goebel and Caitlin Hicks Pries
#Edited February 24, 2020

#Change as appropriate for computer in use
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/ARQ Calculations/")
getwd()

library(plyr)
library(MASS)
library(reshape2)
library(dplyr)
library(car)
library(ggplot2)
library(nlme)
library(doBy)
library(zoo)
library(splines)



#### Functions ####
sterr = function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
getstr = function(mystring, initial.character, final.character){
  #check that all 3 inputs are character variables
  if (!is.character(mystring)){
    stop('The parent string must be a character variable.')
  }
  if (!is.character(initial.character)){
    stop('The initial character must be a character variable.')
  }
  if (!is.character(final.character)){
    stop('The final character must be a character variable.')
  }
  #pre-allocate a vector to store the extracted strings
  snippet = rep(0, length(mystring))
  for (i in 1:length(mystring)){
    # extract the initial position
    initial.position = gregexpr(initial.character, mystring[i])[[1]][1] + 1
    # extract the final position
    final.position = gregexpr(final.character, mystring[i])[[1]][1] - 1
    # extract the substring between the initial and final positions, inclusively
    snippet[i] = substr(mystring[i], initial.position, final.position)
  }
  return(snippet)
}



#### File Finder ####
#Directs program to path = "yyyy-m-dd processed data" and reads all file names by pattern = ".suffix"
#***MUST CHANGE PATTERN TO "* processed data" ON WINDOWS COMPUTERS AND "*processed data.csv" FOR MACS***#
files <- list.files(path = "2019 ARQ CSVs", pattern = "*.csv", full.names = TRUE, recursive = FALSE)
#View list of file names
files

dates <- data.frame(matrix(ncol = 1, nrow = 8))
names(dates) <- "ymd"
alldata <- data.frame(matrix(ncol = 12, nrow = 0))
alldata_avgC <- data.frame(matrix(ncol = 5, nrow = 0))

for(i in 1:length(files)){
  dates$ymd[i] <- getstr(files[i], "<", ">")
  data <- read.csv(files[i], header = TRUE)
  data <- data[,c("File","Plot", "Treatment", "Depth", "Time", "CO2", "O2", "Fibox.Av.Manual")]
  data$Date <- dates$ymd[i]
  data$DT <- paste(data$Date, as.character (data$Time))
  data$DT <- as.POSIXct(data$DT, format = "%Y-%m-%d %H:%M")
  #Creates one large masterfile
  alldata <- rbind(alldata, data)
}



#### Standards ####
stds <- read.csv("stds.csv", header=TRUE)
standards <- subset(alldata, Treatment=="standard"|Treatment=="ambient")
standards <- merge(standards, stds, by="File") #creates file with standard measured and known (actual) values
str(standards)



#### Measurements ####

#Remove standards from data
data = subset(alldata, Treatment!="standard")

#Change depths from factor to numeric
data$Depth.cm=revalue(data$Depth, c("OA"=4, "10"=14, "30"=34, "50"=54, "0"=0, "Snow"=0)) #, "Snow"=0 #If snow, is the ambient above the soil
data$Depth.cm=as.numeric(levels(data$Depth.cm))[data$Depth.cm]

#write.csv(data, "2019 Gas Well Data.csv")



#### Corrections ####
#Subset data by date for making corrections one at a time
date <- '2019-10-18'

selectedstan <- standards %>%
  filter(Date == as.Date(date))

selectedstan$DT <- as.numeric(selectedstan$DT)
#selectedstan$CO2 <- as.numeric(selectedstan$CO2)
str(selectedstan)

#Average because each measurement is duplicated
selectedstanavg=aggregate(cbind(CO2, Fibox.Av.Manual, DT, CO2.actual, O2.actual)~Plot+Treatment, data=selectedstan, FUN=mean, na.action = na.pass)
selectedstanavg$DT <- as.POSIXct(selectedstanavg$DT, origin = '1970-01-01', tz = "EST")
names(selectedstanavg)[names(selectedstanavg)=="Fibox.Av.Manual"] <- "O2"

selecteddata <- data %>%
  filter(Date == as.Date(date))

selecteddata$DT <- as.numeric(selecteddata$DT)
#selecteddata$CO2 <- as.numeric(selecteddata$CO2)
str(selecteddata)

#Average because each measurement is duplicated
selecteddataavg=aggregate(cbind(CO2, Fibox.Av.Manual, DT)~Plot+Treatment+Depth+Depth.cm, data=selecteddata, FUN=mean)
selecteddataavg$DT <- as.POSIXct(selecteddataavg$DT, origin = '1970-01-01', tz = "EST")
names(selecteddataavg)[names(selecteddataavg)=="Fibox.Av.Manual"] <- "O2"

#Correct CO2 values
ggplot(selectedstanavg, aes(x=CO2, y=CO2.actual, color=DT)) + 
  geom_point(size=4) 

#Model of calibration curve
CO2.m=summary(lm(CO2.actual~CO2, subset(selectedstanavg, CO2.actual>1000)))
CO2.m #Look at summary, check R squared
selecteddataavg$corrCO2=ifelse(selecteddataavg$CO2<2000,selecteddataavg$CO2,CO2.m$coefficients[2]*selecteddataavg$CO2-CO2.m$coefficients[1])

#Correct O2 values, subset
selectedstan_h=subset(selectedstanavg, O2.actual>19) #mix likely has user error, so leave out
ggplot(selectedstan_h, aes(x=O2, y=O2.actual, color=DT)) + 
  geom_point(size=4) 

#Correct for drift in O2 before correct to the absolute value
selectedstan_h$corrfact=selectedstan_h$O2.actual-selectedstan_h$O2 #creates correction factor column
#Pattern with standards and ambients
ggplot(selectedstan_h, aes(x=DT, y=corrfact, color=DT)) + 
  geom_point(size=4) 
#Pattern with just ambient
ggplot(subset(selectedstan_h, Treatment=="ambient"), aes(x=DT, y=corrfact, color=DT)) + 
  geom_point(size=4)



#### Splines ####
#Change number for splines to fit data best
driftcorrtry=lm(corrfact~ns(DT,10), selectedstan_h)
summary(driftcorrtry)
DT=selectedstan_h[,5]

#What do estimated correction factors look like?
selectedstan_h$corrfact.p=predict(driftcorrtry, newdata=DT)
selectedstan_h$dcorrO2=selectedstan_h$O2+predict(driftcorrtry, newdata=DT)
ggplot(selectedstan_h, aes(x=DT, y=corrfact.p)) + #plot of correction factors
  geom_line(size=1) +
  geom_point(aes(x=DT, y=corrfact, color=Treatment), data=selectedstan_h, size=3)

#O2 standard curve correction
ggplot((subset(selectedstan_h, Treatment=="standard")), aes(x=dcorrO2, y=O2.actual, color=DT)) + 
  geom_point(size=4)
ggplot(selectedstan_h, aes(x=dcorrO2, y=O2.actual, color=DT)) + #includes ambient samples
  geom_point(size=4)

O2.m=lm(O2.actual~dcorrO2, selectedstan_h)
summary(O2.m)

#Drift correct O2 values for gas well data
DT=selecteddataavg[,7]
selecteddataavg$dcorrO2=selecteddataavg$O2+predict(driftcorrtry, newdata=DT)

#Apply correction to samples
selecteddataavg$corrO2=O2.m$coefficients[2]*selecteddataavg$dcorrO2+O2.m$coefficients[1]
#Apply correction to standards
selectedstan_h$corrO2=O2.m$coefficients[2]*selectedstan_h$dcorrO2+O2.m$coefficients[1]

#Calculate ambient CO2
ambient=subset(selectedstan_h, Treatment=="ambient")
ambCO2=mean(ambient$CO2, na.rm=T)

#Calculate ARQ based on known atmospheric values
selecteddataavg$ARQ=(0.76*((selecteddataavg$corrCO2-405)/((20.95*10000)-(selecteddataavg$dcorrO2*10000))))

#Calulate ARQ with known O2, sampled CO2
selecteddataavg$ARQ=ifelse((0.76*((selecteddataavg$corrCO2-ambCO2)/((20.95*10000)-(selecteddataavg$dcorrO2*10000))))>0, #so if negative no value
                (0.76*((selecteddataavg$corrCO2-ambCO2)/((20.95*10000)-(selecteddataavg$dcorrO2*10000)))), Inf)

#Remove surface values from dataset because ARQ isn't calculated for surface values
selectedprofiledata=subset(selecteddataavg, Treatment!="ambient") #Remove ambients
dataS=subset(selectedprofiledata, Depth!="Snow") #Remove snow

#Graph all data (no summary)
ggplot(subset(selectedprofiledata), aes(x=ARQ, y=Depth.cm, color=Treatment)) + 
  geom_point() + 
  ylab("Depth")+
  xlab("ARQ")+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        axis.line=element_line(colour="black"))+
  coord_cartesian(ylim=c(0, 40))+
  scale_y_reverse(breaks=seq(0, 40, 10))

#Summary Table ARQ for Single Time Point (", Plot" taken out of summaryBy)
selectedprofiledata=summaryBy(ARQ~c(Depth.cm, Treatment), data=dataS, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))
selectedprofiledata$Date=date

#Graph summary
ggplot(selectedprofiledata, aes(x=ARQ.m, y=Depth.cm, color=Treatment)) + 
  geom_errorbarh(aes(xmin=ARQ.m-ARQ.se, xmax=ARQ.m+ARQ.se), col="black", height=1.5)+
  geom_point(size=4) + 
  scale_colour_manual(values=c("green", "blue", "orange", "red"))+ 
  #ggtitle("Depthwise ARQ at SWaN December 16, 2019 ")+
  ylab("Depth (cm)")+
  xlab("ARQ")+
  theme_bw()+
  theme(axis.text.x=element_text(size=14, colour="black"),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_text(size=16, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        legend.text = element_text(colour="black", size = 16),
        legend.title = element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        legend.position=c(0.15,0.15),
        axis.line=element_line(colour="black"))+
  coord_cartesian(ylim = c(0, 55), xlim = c(0.4, 2))+
  scale_y_reverse(breaks=seq(0, 55, 10))

#Change title to have selected date
write.csv(selectedprofiledata, "2019-06-27 ARQ.csv")






#Make one large dataframe with ARQ for all dates
alldataARQ <- rbind(alldataARQ, selecteddataARQ)
str(alldataARQ)

#Run once all dates have been done one at a time...

#Remove infinite ARQ values
alldata_avgC_int <- subset(alldata_avgC, ARQ.m!="Inf")

#Change depth to factor
alldata_avgC_int$Depth.cm <- as.factor(alldata_avgC_int$Depth.cm)

#Make date a datetime type
alldata_avgC_int$Date <- as.POSIXct(alldata_avgC_int$Date)
alldata_avgC_int <- alldata_avgC_int[order(as.Date(alldata_avgC_int$Date)),]

write.csv(alldata_avgC_int, "2019 ARQ Compiled.csv")



#### Graphing ####
#ARQ over time by treatment with depth color coding
ggplot(alldata_avgC_int, aes(x = Date, y = ARQ.m, color = Depth.cm)) +
  geom_line() +
  #geom_errorbarh(aes(xmin=ARQ.m-ARQ.se, xmax=ARQ.m+ARQ.se), col="black")+
  geom_point(size=4) + 
  scale_colour_manual(values=c("red", "orange", "green", "blue")) +
  theme(axis.text.x=element_text(size=14, colour="black", hjust=1),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_text(size=16, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        legend.text = element_text(colour="black", size = 16),
        legend.title = element_blank(),
        legend.key = element_rect("white"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14, colour="black"),
        panel.border=element_blank(), 
        legend.position=c(0.05,0.15),
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.line=element_line(colour="black")) +
  coord_cartesian(ylim = c(0, 5)) +
  facet_wrap(alldata_avgC_int$Treatment, ncol = 2)

treatmentavsARQ <- alldata_avgC_int %>%
  group_by(Date, Treatment) %>%
  summarize(treatARQ = format(mean(ARQ.m, na.rm=TRUE), digits = 5))
treatmentavsARQ$treatARQ <- as.numeric(treatmentavsARQ$treatARQ)

#ARQ over time by treatment
ggplot(treatmentavsARQ, aes(x = Date, y = treatARQ, color = Treatment, group = Treatment)) +
  #geom_errorbarh(aes(xmin=ARQ.m-ARQ.se, xmax=ARQ.m+ARQ.se), col="black")+
  geom_point(size=4) + 
  geom_line() +
  scale_colour_manual(values=c("red", "orange", "green", "blue")) +
  ylab("ARQ")+
  xlab("Date")+
  coord_cartesian(ylim=c(0, 2)) +
  theme(axis.text.x=element_text(size=14, colour="black", hjust=1),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_text(size=16, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        legend.text = element_text(colour="black", size = 16),
        legend.title = element_blank(),
        legend.key = element_rect("white"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14, colour="black"),
        panel.border=element_blank(), 
        legend.position=c(0.25,0.85),
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.line=element_line(colour="black"))


#ARQ over time by depth
ggplot(alldata_avgC_int, aes(x = Date, y = ARQ.m, color = Treatment)) +
  geom_line() +
  geom_point(size=4) + 
  #geom_errorbar(aes(ymin=ARQ.m-ARQ.se, ymax=ARQ.m+ARQ.se), col="black", width=0.2) +
  scale_colour_manual(values=c("green", "blue", "orange", "red")) +
  theme(axis.text.x=element_text(size=14, colour="black", hjust=1),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_text(size=16, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        legend.text = element_text(colour="black", size = 16),
        legend.title = element_blank(),
        legend.key = element_rect("white"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14, colour="black"),
        panel.border=element_blank(), 
        legend.position=c(0.15,0.4),
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.line=element_line(colour="black")) +
  coord_cartesian(ylim = c(0, 3)) +
  facet_wrap(alldata_avgC_int$Depth.cm, ncol = 2)
  


#Summary table by depth
depthdata_avgC=summaryBy(ARQ~c(Date, Depth), data=dataS, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))

#ARQ over time by depth
ggplot(depthdata_avgC, aes(x = Date, y = ARQ.m, color = Depth)) +
  #geom_errorbarh(aes(xmin=ARQ.m-ARQ.se, xmax=ARQ.m+ARQ.se), col="black")+
  geom_point(size=4) + 
  geom_line() +
  scale_colour_manual(values=c("orange", "green", "blue", "red")) +
  theme(axis.text.x=element_text(size=14, colour="black", hjust=1),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_text(size=16, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        legend.text = element_text(colour="black", size = 16),
        legend.title = element_blank(),
        legend.key = element_rect("white"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14, colour="black"),
        panel.border=element_blank(), 
        legend.position=c(0.05,0.15),
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        axis.line=element_line(colour="black")) +
  coord_cartesian(ylim = c(0, 5))



#Excludes nitrogen treatments
nonitrogendata=subset(timedata_avgC, Treatment=="Control" | Treatment=="Heated")

ggplot(nonitrogendata, aes(x = Date, y = ARQ.m, color = Depth)) +
  #geom_errorbarh(aes(xmin=ARQ.m-ARQ.se, xmax=ARQ.m+ARQ.se), col="black")+
  geom_point(size=4) + 
  geom_line() +
  scale_colour_manual(values=c("orange", "green", "blue", "red")) +
  #ggtitle("Seasonal ARQ of Heated Plot Experiment")+
  ylab("ARQ")+
  xlab("Date")+
  theme(axis.text.x=element_text(size=14, colour="black", hjust=1),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_text(size=16, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        legend.text = element_text(colour="black", size = 16),
        legend.title = element_blank(),
        legend.key = element_rect("white"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14, colour="black"),
        panel.background = element_rect(fill="white"),
        panel.grid=element_blank(),
        #panel.border=element_rect(colour="black"), 
        legend.position=c(0.15,0.35),
        axis.line=element_line(colour="black")) +
  coord_cartesian(ylim = c(0, 5)) +
  facet_wrap(nonitrogendata$Treatment, ncol = 1)
