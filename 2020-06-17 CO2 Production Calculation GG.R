## Depthwise CO2 Production Calculations for the Harvard Forest

#Written by Genevieve Goebel and Caitlin Hicks Pries
#Edited February 26 - June 17, 2020


#### Note ####
#Running ARQ Code is a prerequisite for calculating CO2 production
#ARQ Code initializes all necessary packages and creates dataframes that are needed
#If ARQ has not be calculated yet, do so first and return to this afterwards


#Example dataframe
#exampledata <- read.csv("Example Datefile 2018-10-09.csv", header = TRUE)

#Compare data to previous dataframe
#str(exampledata)
#str(selecteddataavg)



#### Initialize Program ####
#Change as appropriate for computer in use
setwd("/Users/goeb_genevieve/Desktop/Data Analysis/Hicks Pries/Data/CO2 Production Calculations/")
getwd()

install.packages("caret")

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
library(tidyverse)
library(caret)
sterr = function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))





#### Import and Prepare Key Information ####

#Change column object types for use
selecteddataavg$Plot <- as.factor(selecteddataavg$Plot)
selecteddataavg$Depth.cm <- as.integer(selecteddataavg$Depth.cm)



#Calculate average daily ambient CO2 for selected date
selecteddataavgambs <- selecteddataavg %>%
  filter(Treatment=="ambient")
ambCO2 <- mean(selecteddataavgambs$corrCO2)

  #avCO2s <- aggregate(ambsdata$CO2, list(ambsdata$Date), mean)
  #names(avCO2s) <- c("Date", "Av CO2")
  #Change date format for merge compatibility
  #avCO2s$Date <- as.Date(avCO2s$Date, format = "%Y-%m-%d")
  #str(avCO2s)

#Retrieve average ambient CO2 for selected date
#ambCO2 <- avCO2s$AvAmbDay[avCO2s$Date==date] 
selectedprofiledata$Av.Amb.CO2 <- ambCO2



#Average daily air temperature and 10 cm soil temperature from HF Fisher weather station
weatherSWaN2019 <- read.csv("2019 HF Station Data.csv", header=TRUE)
#Change date format for merge compatibility
weatherSWaN2019$Date <- as.Date(weatherSWaN2019$Date, format = "%m/%d/%y")
str(weatherSWaN2019)
#Retrieve average air temperature for selected date
dailyAvAirTemp <- weatherSWaN2019$Air.Temp.Av[weatherSWaN2019$Date==date]
selectedprofiledata$Av.Air.Temp <- dailyAvAirTemp



#Limit data to just gas well measurements in the soil profile, eliminate surface values
selectedprofiledata <- subset(selecteddataavg, Treatment!="ambient")
#Change POSIXct format to character for parse compatibility
selectedprofiledata$DT <- as.character(selectedprofiledata$DT)
#Parse out date and time columns for DT column
selectedprofiledata$Date <- parse_date(selectedprofiledata$DT, format = "%Y%.%m%.%d %H:%M:%S")
selectedprofiledata$Time <- parse_time(selectedprofiledata$DT, format = "%Y%.%m%.%d %H:%M:%S")
#Change data format for merge compatibility
selectedprofiledata$Date <- as.Date(selectedprofiledata$Date, format = "%Y-%m-%d")



#Import data from field notebook to get surface VWC measurements and depthwise temperature
notebookdata <- read.csv("2019-10-18 VWC Temp Data.csv", header=TRUE)
notebookdata = subset(notebookdata, Treatment!="ambient")
notebookdata = subset(notebookdata, Treatment!="standard")
notebookdata$VWC <- notebookdata$VWC/100
str(notebookdata)



#Refine logger data to average values from the date being analyzed to access depthwise VWC measurements
dailyloggerdata <- logger_day %>%
  filter(Date == as.Date(date))
#Average VWC measurements by treatment and depth
dailyloggerdataavg = summaryBy(fixedVWC~c(Treatment, Depth), data=dailyloggerdata, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))



selectedprofiledata$Temp <- NA
selectedprofiledata$VWC <- NA
#Run through mainframe (selectedprofiledata), notebookdata, and dailyloggerdataavg to transfer all temp measurements and OA VWC values by plot and depth
for(i in 1:length(selectedprofiledata$Plot)){
  #Check if row has empty temp value
  if(is.na(selectedprofiledata$Temp[i])){
    #Scan through notebook data to assign corresponding temp value
    for(j in 1:length(notebookdata$Plot)){
      if(selectedprofiledata$Plot[i] == notebookdata$Plot[j] & selectedprofiledata$Depth[i] == notebookdata$Depth[j]){
        selectedprofiledata$Temp[i] <- notebookdata$Temp[j]
      }
    }
  }
  #Check if row has empty VWC value
  if(is.na(selectedprofiledata$VWC[i])){
    #Scan through notebook data to assign OA VWCs for all plots
    for(j in 1:length(notebookdata$Plot)){
      if(selectedprofiledata$Plot[i] == notebookdata$Plot[j] & selectedprofiledata$Depth[i] == notebookdata$Depth[j]){
        selectedprofiledata$VWC[i] <- notebookdata$VWC[j]
      }
    }
    #Scan through logger data to assign 10, 30, and 50 VWCs (assign Nitrogen and Heated Nitrogen treatments Control and Heated values respectively)
    for(k in 1:length(dailyloggerdataavg$fixedVWC.m)){
      #Covers depths 10 and 30 for Control and Heated plots 
      if(selectedprofiledata$Treatment[i] == dailyloggerdataavg$Treatment[k]  & selectedprofiledata$Depth[i] == dailyloggerdataavg$Depth[k]){
        selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
      #Covers 50 depth for Control plots
      }else if(selectedprofiledata$Treatment[i] == "Control" & selectedprofiledata$Depth[i] == 50){
        if(dailyloggerdataavg$Treatment[k] == "Control" & dailyloggerdataavg$Depth[k] == 30){
          selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
        }
      #Covers Nitrogen plots
      }else if(selectedprofiledata$Treatment[i] == "Nitrogen"){
        #Covers 10 depth
        if(selectedprofiledata$Depth[i] == 10){
          if(dailyloggerdataavg$Treatment[k] == "Control" & dailyloggerdataavg$Depth[k] == 10){
            selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
          }
        #Covers 30 and 50 depths
        }else if(selectedprofiledata$Depth[i] == 30 | selectedprofiledata$Depth[i] == 50){
          if(dailyloggerdataavg$Treatment[k] == "Control" & dailyloggerdataavg$Depth[k] == 30){
            selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
          }
        }
      #Covers 50 depth for Heated plots
      }else if(selectedprofiledata$Treatment[i] == "Heated" & selectedprofiledata$Depth[i] == 50){
        if(dailyloggerdataavg$Treatment[k] == "Heated" & dailyloggerdataavg$Depth[k] == 30){
          selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
        }
      #Covers Heated-Nitrogen plots
      }else if(selectedprofiledata$Treatment[i] == "Heated-Nitrogen"){
        #Covers 10 depth
        if(selectedprofiledata$Depth[i] == 10){
          if(dailyloggerdataavg$Treatment[k] == "Heated" & dailyloggerdataavg$Depth[k] == 10){
            selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
          }
        #Covers 30 and 50 depths
        }else if(selectedprofiledata$Depth[i] == 30 | selectedprofiledata$Depth[i] == 50){
          if(dailyloggerdataavg$Treatment[k] == "Heated" & dailyloggerdataavg$Depth[k] == 30){
            selectedprofiledata$VWC[i] <- dailyloggerdataavg$fixedVWC.m[k]
          }
        }
      }
    }
  }
}
selectedprofiledata$VWC <- selectedprofiledata$VWC*100
str(selectedprofiledata)



#Create a list of all plot numbers
#plotlist <- list(1, 2)
plotlist <- list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24)
Plot <- unlist(plotlist)

#Test dataframe
#testselecteddata1 <- selectedprofiledata %>%
  #filter(Plot=="1"|Plot=="2")



#Data for depth 0 (Don't run if snow is present, depth 0 already measured)
Depth.cm = rep(0, length(Plot))
data0 = data.frame(cbind(Depth.cm, Plot))
datasub = subset(selectedprofiledata, Depth.cm==4)
datasub$Plot <- as.numeric(as.character(datasub$Plot))
datasub <- datasub[order(datasub$Plot),]
data0$Treatment = datasub$Treatment
corrCO2 = rep(ambCO2, length(Plot))
data0 = data.frame(cbind(data0, corrCO2))
data0$Temp = dailyAvAirTemp
data0$VWC = datasub$VWC
#data0$Av.Air.Temp = dailyAvAirTemp
data0$k = datasub$k
data0$Plot = as.factor(data0$Plot)
str(data0)

selectedprofiledata2 = rbind.fill(selectedprofiledata, data0)

selectedprofiledata2$Depth.cm <- as.integer(selectedprofiledata2$Depth.cm)
selectedprofiledata2$Plot <- as.factor(selectedprofiledata2$Plot)



#Bulk density data by depth and plot
BD <- read.csv("SWaN_BD.csv", header=TRUE)
str(BD)


#### Estimate Bulk Density ####

#Create polynomial functions to use for modeling BD at depths
BD_values2=lm(BD~poly(middepth, 2, raw=TRUE), data=BD,na.action=na.omit)  
BD_values3=lm(BD~poly(middepth, 3, raw=TRUE), data=BD,na.action=na.omit)
BD_values4=lm(BD~poly(middepth, 4, raw=TRUE), data=BD,na.action=na.omit)
#Look for polynomial with lowest AIC for best fit
AIC(BD_values2,BD_values3, BD_values4)

#Use one of the polynomials to estimate BD at depths we do not have data for
middepth=seq(0, 55,length.out=1000)
pred.frame=data.frame(middepth)
pred.frame$middepth=as.numeric(as.character(pred.frame$middepth))

#Change model here to look at the fit of other polynomials
pred.frame$BD=predict(BD_values3, newdata=pred.frame) 

#Visually check fit and AIC to decide on what order polynomial
ggplot(BD) +
  geom_point(aes(x=BD, y=middepth, colour=Plot))+#, shape=Treatment))+
  # geom_line(aes(x=CO2md, y=Depth.cm, colour=Plot))+
  theme_bw() +      #makes bg white
  xlab("BD") +
  ylab("Depth") +
  theme(axis.text.x=element_text(size=14, colour="black"),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        panel.border=element_blank(), 
        axis.line=element_line(colour="black"))+
  geom_point(data=pred.frame, aes(x=BD, y=middepth), size=.5)+
  #coord_cartesian(xlim=c(0, 30000))
  scale_y_reverse(breaks=seq(-1, 60, 10))


#Adding interpolated bulk densities for each middepth
middepth=c(0, 2, 9, 24, 44)
BDest=data.frame(middepth)
BDest$middepth=as.numeric(as.character(BDest$middepth))
BDest$BD=predict(BD_values3, newdata=BDest) #Use calculated model
BDest$Depth.cm=c(0, 4, 14, 34, 54)
selectedprofiledata3=merge(selectedprofiledata2, BDest[,c(2,3)], by="Depth.cm")

#Create depth (m) column by converting depth (cm) column
selectedprofiledata3$Depth.m=selectedprofiledata3$Depth.cm/100

#Ambient above soil value of CO2, creates new column
selectedprofiledata3$k=ambCO2

str(selectedprofiledata3)





#### Calculate CO2 by Depth ####

#Build linear model
C_values = lmList(corrCO2 ~ Depth.m|Plot, data = selectedprofiledata3, na.action=na.omit)
summary(C_values)

#Build first order polynomials
C_values2 = lmList(corrCO2 ~ poly(Depth.m, 2, raw = TRUE)|Plot, data = selectedprofiledata3, na.action = na.omit) 
summary(C_values2)
C_values3 = lmList(corrCO2 ~ poly(Depth.m, 3, raw = TRUE)|Plot, data = selectedprofiledata3, na.action = na.omit)
summary(C_values3)
C_values4 = lmList(corrCO2 ~ poly(Depth.m, 4, raw = TRUE)|Plot, data = selectedprofiledata3, na.action = na.omit)
summary(C_values4)

#Build second order polynomial, force intercept through ambient CO2 value
C_values5 = lmList(corrCO2 ~ -1 + Depth.m + (I(Depth.m^2)) + offset(k)|Plot, data = selectedprofiledata3, na.action = na.omit)
summary(C_values5)

AIC(C_values, C_values2, C_values3, C_values4, C_values5)



#Check model fit
plots <- factor(Plot)
Plot = rep(levels(plots), each=100)
#Change last # to # of plots
Depth.m = rep(seq(0, .55, length.out=100), 23) 
k = rep(ambCO2, length(Plot))

pred.frame = data.frame(cbind(Plot, Depth.m, k))
pred.frame$Depth.m=as.numeric(as.character(pred.frame$Depth.m))
pred.frame$k=as.numeric(as.character(pred.frame$k))
str(pred.frame)
#Change to model you want to look at
pred.frame$corrCO2=predict(C_values5, newdata=pred.frame)



#Check fit and AIC to decide on what order polynomial
ggplot(selectedprofiledata3) +
  geom_point(aes(x=corrCO2, y=Depth.m, colour=Plot))+ #, shape=Treatment))+
  # geom_line(aes(x=CO2md, y=Depth.cm, colour=Plot))+
  theme_bw() +      #makes bg white
  xlab("CO2") +
  ylab("Depth") +
  theme(axis.text.x=element_text(size=14, colour="black"),
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        panel.border=element_blank(), 
        axis.line=element_line(colour="black"))+
  geom_point(data=pred.frame, aes(x=corrCO2, y=Depth.m, colour=Plot), size=.5)+
  #coord_cartesian(xlim=c(0, 30000))
  scale_y_reverse(breaks=seq(-1, .90, .10))



#For second order polynomial
C_coef = nlme:::coef.lmList(C_values5)
C_coef <- data.frame(as.character(rownames(C_coef)), C_coef)

#Don't force intercept (for dates with snow)
#names(C_coef)=c("Plot", "c", "b", "a") 

#Force intercept (for dates without snow, force through ambient)
names(C_coef)=c("Plot", "b", "a")

#Calculate derivative for each well depth and surface, change in CO2 per depth increment
C_coef$C_0=C_coef$a*2*.0001+C_coef$b
C_coef$C_4=C_coef$a*2*.04+C_coef$b
C_coef$C_10=C_coef$a*2*.10+C_coef$b
C_coef$C_30=C_coef$a*2*.30+C_coef$b
C_coef$C_50=C_coef$a*2*.50+C_coef$b
names(C_coef)

colnums <- ncol(C_coef)
#Choose last 5 columns
dataC_final=C_coef[,c(1,(colnums-4):colnums)]

#Melt from wide format to long format
dataC_final=melt(dataC_final[,c(1:6)], id="Plot")
dataC_final$Plot=as.character(dataC_final$Plot)
names(dataC_final)=c("Plot", "variable", "CO2.per.m.poly")

#Add numeric depth column
dataC_final$Depth.cm=revalue(dataC_final$variable, c("C_0"=0, "C_4"=4, "C_10"=14, "C_30"=34, "C_50"=54))
dataC_final$Depth.cm=as.numeric(as.character(dataC_final$Depth.cm))

#Now you have a dataframe with dC/dZ values, merge with original dataframe
data3=merge(selectedprofiledata3, dataC_final, by=c("Plot", "Depth.cm")) 





#### Calculate Flux Density Discretely for O Horizon ####

#Try discrete calculations in molar density CO2 per meter
data4=reshape(data3, idvar=c("Plot", "Treatment"), timevar="Depth.cm", direction="wide")
names(data4)

data4$C_0=(data4$corrCO2.4-data4$k.4)/(.04-0)
data4$C_4=(data4$corrCO2.14-data4$corrCO2.4)/(.14-.04)
data4$C_14=(data4$corrCO2.34-data4$corrCO2.14)/(.34-.14)
data4$C_34=(data4$corrCO2.54-data4$corrCO2.34)/(.54-.34)
#Discrete calculations cause a loss in one depth increment due to subtraction, assign same slope to 30 and 50
data4$C_54=(data4$corrCO2.54-data4$corrCO2.34)/(.54-.34)



colnums <- ncol(data4)
#Choose last 5 columns in wide format
data5=data4[,c(1,(colnums-4):colnums)]
#Melt from wide format to long format
data5=melt(data5[c(1:6)], id="Plot")
data5$Plot=as.character(data5$Plot)
names(data5)=c("Plot", "variable", "CO2.per.m.dis")

data5$Depth.cm=revalue(data5$variable, c("C_0"=0, "C_4"=4, "C_14"=14, "C_34"=34, "C_54"=54))
data5$Depth.cm=as.numeric(as.character(data5$Depth.cm))

data6=merge(data3, data5, by=c("Plot", "Depth.cm")) 



#Assign discrete value to top depth and derivative-based values (dC/dz) to the rest of the depths
data6$CO2.per.m=ifelse(data6$Depth.cm==0, data6$CO2.per.m.dis, data6$CO2.per.m.poly)

#Assign derivative-based values (dC/dz)
#data6$CO2.per.m = data6$CO2.per.m.poly 



#Retrieve daily barometric air pressure from HF weather station dataframe
dayweather <- weatherSWaN2019 %>%
  filter(Date == as.Date(date))
daymBar = dayweather$Barometric.Pres.Av



#Use the Ideal Gas Law to calculate molar density of air in mol/m^3
data6$airmd=(daymBar/1000)/((8.3144621*10^-5)*(data6$Temp+273)) 
#Multiply ppm/depth slope by molar density slope to convert units to umol CO2/m^3
data6$CO2md.per.m=data6$airmd*data6$CO2.per.m 





#### Calculate Diffusion (D) for Fick's Law ####

#Calculate porosity (assumption that substrate is all mineral causes some inaccuracy for O horizon, 0~4 cm)
data6$por=ifelse(data6$Depth.cm>1, 100-(data6$BD/2.65*100), 100-(data6$BD/1.4*100))
#Organic matter assumed particle density 1.4 (Davidson et al. 2006b)

#Correcting for 20% rock (Contosta et al. 2016)
data6$por=ifelse(data6$Depth.cm>1, data6$por-(data6$por*0.2), data6$por)

#Calculate percentage of air-filled pores
data6$af.por=(data6$por-data6$VWC) 
#Give arbitrarily high value at surface
#data6$af.por=ifelse(data6$Depth.cm>1, data6$af.por, 85) 

#Calculate tortuosity with Moyes equation
data6$tor=0.95*(data6$af.por/100)^1.93 

#Air-filled porosity and tortuosity should be greater at surface
plot(af.por~Depth.cm, data6) 
plot(tor~Depth.cm, data6)

#Calculate diffusion if tortuosity is not an issue
data6$D.o=15.7*((data6$Temp+273)/293.15)^1.75*(101.3/(daymBar/10))
#15.7 mm^2/s is the diffusion of CO2 in air at 293.15K and 101.3 kPa (Vargas et al. 2010)

#Correct diffusion for tortuosity
data6$D=data6$tor*data6$D.o

plot(D~Depth.cm, data6) 

#Use Fick's law to multiply by diffusion coefficient and calculate flux density across a horizontal plane at each depth (negative value)
data6$Flux.d=-1*data6$D*data6$CO2md.per.m/(1000^2) 
#Divide by 1000^2 since D was in mm^2/s and units must be umol CO2/m^2/s



#Summarize F
data_avg=summaryBy(Flux.d~c(Depth.cm, Treatment), data=data6, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))

fluxpalette=c("blue", "red", "orange", "green")
#Graph CO2 flux density with depth
ggplot(data6, aes(y=Depth.cm, x=Flux.d, col=Treatment)) +
  geom_point() +
  theme_bw() +      #makes bg white
  xlab("CO2 Flux Density") +
  ylab("Depth") +
  scale_colour_manual(values=fluxpalette)+
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
        axis.line=element_line(colour="black"))+
  scale_y_reverse(breaks=seq(0, 100, 10))





#### Calculate CO2 Production by Depth in Meters ####

data7=reshape(data6, idvar=c("Plot", "Treatment"), timevar="Depth.cm", direction="wide")
names(data7)

#Take difference between layers (umol/m^3/s)
data7$CO2_prod.0=(data7$Flux.d.0-data7$Flux.d.4)/(0-.04)
data7$CO2_prod.4=(data7$Flux.d.4-data7$Flux.d.14)/(.04-.14)
data7$CO2_prod.14=(data7$Flux.d.14-data7$Flux.d.34)/(.14-.34)
data7$CO2_prod.34=(data7$Flux.d.34-data7$Flux.d.54)/(.34-.54)

colnums2 <- ncol(data7)
#Choose last 4 columns in wide format
data8=data7[,c(1,(colnums2-3):colnums2)]
#Melt variables individually so prod, Temp, and VWC in wide format
data8=melt(data8[c(1:5)], id="Plot")
data8$Plot=as.character(data8$Plot)
names(data8)=c("Plot", "variable", "CO2_prod")

data8$Depth.cm=revalue(data8$variable, c("CO2_prod.0"=0, "CO2_prod.4"=4, "CO2_prod.14"=14, "CO2_prod.34"=34))
data8$Depth.cm=as.numeric(as.character(data8$Depth.cm))

data9=merge(data3, data8, by=c("Plot", "Depth.cm")) 

#Get rid of negative production values
data9$CO2_prod=ifelse(data9$CO2_prod>0, data9$CO2_prod, 0)


#### Summarize and Graph All Points ####

fluxpalette=c("blue", "red", "orange", "green")
ggplot(data9, aes(y=Depth.cm, x=CO2_prod, col=Treatment)) + 
  geom_point(size=4) +
  theme_bw() +      #makes bg white
  xlab("CO2 Production(umol C m-3 s-1)") +
  ylab("Depth") +
  scale_colour_manual(values=fluxpalette)+
  theme_bw()+
  theme(axis.text.x=element_text(size=16, colour="black"),
        axis.text.y=element_text(size=16, colour="black"),
        axis.title.x=element_text(size=18, colour="black"),
        axis.title.y=element_text(size=18, colour="black"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.grid=element_blank(),
        panel.border=element_blank(), 
        legend.position="none",
        axis.line=element_line(colour="black"))+
  scale_y_reverse(breaks=seq(0, 100, 10))


#### Summarize and Graph by Treatment ####

#CO2 Production by Treatment
data_avg=summaryBy(CO2_prod ~c(Depth.cm, Treatment), data=data9, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))
#data_avg=summaryBy(CO2_prod ~c(Depth.cm, Treatment), data=subset(data8, data8$Plot!=12), FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))

fluxpalette=c("blue", "red", "orange", "green")
ggplot(data_avg, aes(y=Depth.cm, x=CO2_prod.m, col=Treatment)) + 
  geom_errorbarh(aes(xmin=CO2_prod.m-CO2_prod.se, xmax=CO2_prod.m+CO2_prod.se), color="black")+
  geom_point(size=4) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("CO2 Production"~"(umol C ",m^{-3},s^{-1},")")))+
  ylab("Depth (cm)") +
  scale_colour_manual(values=fluxpalette)+
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
        axis.line=element_line(colour="black"))+
  scale_y_reverse(breaks=seq(0, 50, 10))
#scale_x_continuous(limits = c(0, 10))

#ARQ by Treatment
data_avg2=summaryBy(ARQ ~c(Depth.cm, Treatment), data=data9, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))

fluxpalette=c("blue", "red", "orange", "green")
ggplot(data_avg2, aes(y=Depth.cm, x=ARQ.m, col=Treatment)) + 
  geom_errorbarh(aes(xmin=ARQ.m-ARQ.se, xmax=ARQ.m+ARQ.se), color="black")+
  geom_point(size=6) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("ARQ")))+
  ylab("Depth (cm)") +
  scale_colour_manual(values=fluxpalette)+
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
        axis.line=element_line(colour="black"))+
  scale_y_reverse(breaks=seq(0, 100, 10))


#Temperature by Treatment
data_avg3=summaryBy(Temp ~c(Depth.cm, Treatment), data=data9, FUN=function(x) c(m=mean(x,na.rm=T), se=sterr(x)))

fluxpalette=c("blue", "red", "orange", "green")
ggplot(data_avg3, aes(y=Depth.cm, x=Temp.m, col=Treatment)) + 
  geom_errorbarh(aes(xmin=Temp.m-Temp.se, xmax=Temp.m+Temp.se), color="black")+
  geom_point(size=6) +
  theme_bw() +      #makes bg white
  xlab(expression(paste("Temp")))+
  ylab("Depth (cm)") +
  scale_colour_manual(values=fluxpalette)+
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
        axis.line=element_line(colour="black"))+
  scale_y_reverse(breaks=seq(0, 100, 10))

write.csv(data9, "2019-06-27_CO2prod.csv")
