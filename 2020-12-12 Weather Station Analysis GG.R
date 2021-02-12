# # # # # # # # # # # # # # # # # # # # # #
#     Weather Station Processing Code     #
#          GG December 12, 2020           #
# # # # # # # # # # # # # # # # # # # # # #

install.packages("tidyverse")
install.packages("ggplot2")
library("ggplot2")
library("dplyr")

#Change as appropriate for computer in use
setwd("/Users/goeb_genevieve/Desktop/Thesis/Data/Data by Site/SWaN/Meteorology/")
getwd()

#### Fisher Weather Station ####
fisher15 <- read.csv("hf-fisher-15min-m.csv", header=TRUE)
fisherH <- read.csv("hf-fisher-hourly-m.csv", header=TRUE)
fisherD <- read.csv("hf-fisher-daily-m.csv", header=TRUE)

#Narrowing open source data to columns of interest
colnames(fisherD)
fisherHprecip <- subset(fisherD, select = c('date', 'airt', 'prec', 'bar', 'wspd', 'wdir'))
fisherHprecip$date <- as.Date(as.character(fisherHprecip$date))

#5 year dataset
fisherHprecip5y <- subset(fisherHprecip, date > "2015-01-01" & date < "2020-11-30")
str(fisherHprecip5y)

fisherHprecip5yzers <- filter(fisherHprecip5y, prec != 0.0)

non0prec <- ggplot(fisherHprecip5yzers, aes(x = date, y = prec)) +
  geom_point() +
  geom_line()
non0prec

#2 year dataset
fisherHprecip2y <- subset(fisherHprecip, date > "2019-01-01" & date < "2020-11-30")
str(fisherHprecip2y)

fisherHprecip2yzers <- filter(fisherHprecip2y, prec != 0.0)

non0prec <- ggplot(fisherHprecip2yzers, aes(x = date, y = prec)) +
  geom_point() +
  geom_line()
non0prec

#2019 dataset
fisherHprecip2019 <- subset(fisherHprecip, date > "2019-01-01" & date < "2019-12-30")
str(fisherHprecip2019)

fisherHprecip2019zers <- filter(fisherHprecip2019, prec != 0.0)

non0prec <- ggplot(fisherHprecip2019zers, aes(x = date, y = prec)) +
  geom_point() +
  geom_line()
non0prec

#### Gas Production Correlation Check ####
#Change as appropriate for computer in use
setwd("/Users/goeb_genevieve/Desktop/Thesis/Data/Data by Site/SWaN/Gas/2019/")
getwd()

arq2019 <- read.csv("2019 ARQ Compiled.csv", header=TRUE)
str(arq2019)

arq2019$Date <- as.Date(as.character(arq2019$Date))

coeff <- 10
arq2019prec <- ggplot() +
  geom_line(data = fisherHprecip2019zers, aes(x = date, y = prec, colour = "blue")) +
  geom_point(data = arq2019, aes(x = Date, y = ARQ.m*coeff, colour = "green")) +
  scale_y_continuous(name = "Precipitation", sec.axis = sec_axis(trans = ~./10, name = "ARQ"))
arq2019prec

#running average/summation precip (past 3-8 days)
#find 2018 ARQ data
#add logger data for moisture response to summed precip. (depthwise variation?)

