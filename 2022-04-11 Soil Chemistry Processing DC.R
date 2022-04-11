#Soil Chemistry Processing

#Written by Genevieve Goebel and Daysia Charles
#Edited March 8-10, 2022



install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("multcompView")
install.packages("RColorBrewer")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(multcompView)
library(RColorBrewer)

#Change as appropriate for computer in use
setwd("C:/Users/charl/Onedrive/Desktop")
getwd()




#### Polished Soil Processing Data ####
#Bring in logger key file (May be named "SensorID.csv")
procSoildat <- read.csv("2021 Corinth Soil Compiled Data.csv", header=TRUE)

procSoildat$ID <- as.factor(procSoildat$ID)
str(procSoildat)



#### CN Data Sheets ####
soilNdat <- read.csv("2021 Corinth Soil N 20220308.csv", header=TRUE)

soilNdat$Sample.ID <- as.factor(soilNdat$Sample.ID)
names(soilNdat)[names(soilNdat) == 'Sample.ID'] <- "ID"
soilNdat <- soilNdat[-c(3:4),-c(7:11)]
str(soilNdat)

soilCdat <- read.csv("2021 Corinth Soil C 20220308.csv", header=TRUE)

soilCdat$Sample.ID <- as.factor(soilCdat$Sample.ID)
names(soilCdat)[names(soilCdat) == 'Sample.ID'] <- "ID"
soilCdat <- soilCdat[-c(3:4),-c(7:11)]
str(soilCdat)



#### Extraction Data Sheets ####
soilH2Odat <- read.csv("2021 Corinth Soil Water Extracts 20220311.csv", header=TRUE)
soilH2Odat$ID <- as.factor(soilH2Odat$ID)
str(soilH2Odat)

soilAmmAcedat <- read.csv("2021 Corinth Soil Ammonium Acetate Extracts 20220310.csv", header=TRUE)
soilAmmAcedat$ID <- as.factor(soilAmmAcedat$ID)
str(soilAmmAcedat)


#### Combination ####
corinthSoil <- merge(procSoildat, soilCdat, by="ID")
corinthSoil <- merge(corinthSoil, soilNdat, by="ID")
corinthSoil <- merge(corinthSoil, soilH2Odat, by="ID")
corinthSoil <- merge(corinthSoil, soilAmmAcedat, by="ID")

write.csv(corinthSoil, "2021 Corinth Soil Compiled Data.csv")

corinthSoil <- read.csv("2021 Corinth Soil Compiled Data.csv", header=TRUE)
str(corinthSoil)

corinthSoil$ID <- as.factor(corinthSoil$ID)
corinthSoil$Plot <- as.factor(corinthSoil$Plot)
corinthSoil$Landscape <- as.factor(corinthSoil$Landscape) #Must create manually if building merged dataframe from scratch
corinthSoil$Depth <- as.factor(corinthSoil$Depth)



#### Calculations ####
corinthSoilmeans <- corinthSoil %>%
  group_by(ID, Landscape, Depth, Mid.Depth) %>%
  dplyr::summarise_at(.vars = vars(pH, Calculated.mg.N, Calculated.mg.C, Al.weighted.x, Ca.weighted.x, 
                                   Fe.weighted.x, K.weighted.x, Mg.weighted.x, Mn.weighted.x, Al.weighted.y, 
                                   Ca.weighted.y, Fe.weighted.y, K.weighted.y, Mg.weighted.y, Mn.weighted.y),
                      .funs = c(mean = "mean"))

corinthSoilmeans <- as.data.frame(corinthSoilmeans)
str(corinthSoilmeans)

corinthSoilmeans$CvN <- corinthSoilmeans$Calculated.mg.C_mean/corinthSoilmeans$Calculated.mg.N_mean

corinthLandSoilmeans <- corinthSoilmeans %>%
  mutate(Landscape = fct_relevel(Landscape, "Basin", "Shoulder", "Slope", "Hilltop"))

corinthDepthSoilmeans <- corinthSoilmeans %>%
  mutate(Depth = fct_relevel(Depth, "50", "30", "10"))

corinthSpaceSoilmeans <- corinthSoilmeans %>%
  mutate(Depth = fct_relevel(Depth, "50", "30", "10")) %>%
  mutate(Landscape = fct_relevel(Landscape, "Basin", "Shoulder", "Slope", "Hilltop"))

# Statistics ----
#Subset your data based on factor of interest
datahilltop <- dplyr::filter(corinthSoilmeans, Landscape == "Hilltop")
dataslope <- dplyr::filter(corinthSoilmeans, Landscape == "Slope")
datashoulder <- dplyr::filter(corinthSoilmeans, Landscape == "Shoulder")
databasin <- dplyr::filter(corinthSoilmeans, Landscape == "Basin")



## Landscape ANOVAs ----
pHANOVA <- aov(pH_mean ~ Landscape, data = corinthSoilmeans)
summary(pHANOVA)

pHerrorinfo <- dplyr::select(corinthSoilmeans, pH_mean, Landscape) %>%
  dplyr::mutate(resids=resid(pHANOVA))
pHerrorinfo$Landscape <- as.factor(pHerrorinfo$Landscape)

pHTukey <- TukeyHSD(pHANOVA)
pHTukey

summary.lm(pHANOVA)



nitrogenANOVA <- aov(Calculated.mg.N_mean ~ Landscape, data = corinthSoilmeans)
summary(nitrogenANOVA)

Nerrorinfo <- dplyr::select(corinthSoilmeans, Calculated.mg.N_mean, Landscape) %>%
  dplyr::mutate(resids=resid(nitrogenANOVA))
Nerrorinfo$Landscape <- as.factor(Nerrorinfo$Landscape)

NTukey <- TukeyHSD(nitrogenANOVA)
NTukey

summary.lm(nitrogenANOVA)



carbonANOVA <- aov(Calculated.mg.C_mean ~ Landscape, data = corinthSoilmeans)
summary(carbonANOVA)

Cerrorinfo <- dplyr::select(corinthSoilmeans, Calculated.mg.C_mean, Landscape) %>%
  dplyr::mutate(resids=resid(carbonANOVA))
Cerrorinfo$Landscape <- as.factor(Cerrorinfo$Landscape)

CTukey <- TukeyHSD(carbonANOVA)
CTukey

summary.lm(carbonANOVA)



cnANOVA <- aov(CvN ~ Landscape, data = corinthSoilmeans)
summary(cnANOVA)

CNerrorinfo <- dplyr::select(corinthSoilmeans, CvN, Landscape) %>%
  dplyr::mutate(resids=resid(cnANOVA))
CNerrorinfo$Landscape <- as.factor(CNerrorinfo$Landscape)

CNTukey <- TukeyHSD(cnANOVA)
CNTukey

summary.lm(cnANOVA)



alumWatANOVA <- aov(Al.weighted.x_mean ~ Landscape, data = corinthSoilmeans)
summary(alumWatANOVA)

alumWaterrorinfo <- dplyr::select(corinthSoilmeans, Al.weighted.x_mean, Landscape) %>%
  dplyr::mutate(resids=resid(alumWatANOVA))
alumWaterrorinfo$Landscape <- as.factor(alumWaterrorinfo$Landscape)

alumWatTukey <- TukeyHSD(alumWatANOVA)
alumWatTukey

summary.lm(alumWatANOVA)



ironWatANOVA <- aov(Fe.weighted.x_mean ~ Landscape, data = corinthSoilmeans)
summary(ironWatANOVA)

ironWaterrorinfo <- dplyr::select(corinthSoilmeans, Fe.weighted.x_mean, Landscape) %>%
  dplyr::mutate(resids=resid(ironWatANOVA))
ironWaterrorinfo$Landscape <- as.factor(ironWaterrorinfo$Landscape)

ironWatTukey <- TukeyHSD(ironWatANOVA)
ironWatTukey

summary.lm(ironWatANOVA)



mangWatANOVA <- aov(Mn.weighted.x_mean ~ Landscape, data = corinthSoilmeans)
summary(mangWatANOVA)

mangWaterrorinfo <- dplyr::select(corinthSoilmeans, Mn.weighted.x_mean, Landscape) %>%
  dplyr::mutate(resids=resid(mangWatANOVA))
mangWaterrorinfo$Landscape <- as.factor(mangWaterrorinfo$Landscape)

mangWatTukey <- TukeyHSD(mangWatANOVA)
mangWatTukey

summary.lm(mangWatANOVA)



calcWatANOVA <- aov(Ca.weighted.x_mean ~ Landscape, data = corinthSoilmeans)
summary(calcWatANOVA)

calcWaterrorinfo <- dplyr::select(corinthSoilmeans, Ca.weighted.x_mean, Landscape) %>%
  dplyr::mutate(resids=resid(calcWatANOVA))
calcWaterrorinfo$Landscape <- as.factor(calcWaterrorinfo$Landscape)

calcWatTukey <- TukeyHSD(calcWatANOVA)
calcWatTukey

summary.lm(calcWatANOVA)



alumAmmANOVA <- aov(Al.weighted.y_mean ~ Landscape, data = corinthSoilmeans)
summary(alumAmmANOVA)

alumAmmerrorinfo <- dplyr::select(corinthSoilmeans, Al.weighted.y_mean, Landscape) %>%
  dplyr::mutate(resids=resid(alumAmmANOVA))
alumAmmerrorinfo$Landscape <- as.factor(alumAmmerrorinfo$Landscape)

alumAmmTukey <- TukeyHSD(alumAmmANOVA)
alumAmmTukey

summary.lm(alumAmmANOVA)



ironAmmANOVA <- aov(Fe.weighted.y_mean ~ Landscape, data = corinthSoilmeans)
summary(ironAmmANOVA)

ironAmmerrorinfo <- dplyr::select(corinthSoilmeans, Fe.weighted.y_mean, Landscape) %>%
  dplyr::mutate(resids=resid(ironAmmANOVA))
ironAmmerrorinfo$Landscape <- as.factor(ironAmmerrorinfo$Landscape)

ironAmmTukey <- TukeyHSD(ironAmmANOVA)
ironAmmTukey

summary.lm(ironAmmANOVA)



mangAmmANOVA <- aov(Mn.weighted.y_mean ~ Landscape, data = corinthSoilmeans)
summary(mangAmmANOVA)

mangAmmerrorinfo <- dplyr::select(corinthSoilmeans, Mn.weighted.y_mean, Landscape) %>%
  dplyr::mutate(resids=resid(mangAmmANOVA))
mangAmmerrorinfo$Landscape <- as.factor(mangAmmerrorinfo$Landscape)

mangAmmTukey <- TukeyHSD(mangAmmANOVA)
mangAmmTukey

summary.lm(mangAmmANOVA)



calcAmmANOVA <- aov(Ca.weighted.y_mean ~ Landscape, data = corinthSoilmeans)
summary(calcAmmANOVA)

calcAmmerrorinfo <- dplyr::select(corinthSoilmeans, Ca.weighted.y_mean, Landscape) %>%
  dplyr::mutate(resids=resid(calcAmmANOVA))
calcAmmerrorinfo$Landscape <- as.factor(calcAmmerrorinfo$Landscape)

calcAmmTukey <- TukeyHSD(calcAmmANOVA)
calcAmmTukey

summary.lm(calcAmmANOVA)



#Preparing ANOVA graphs
letterspH <- multcompLetters4(pHANOVA, pHTukey)
print(letterspH)
lettersN <- multcompLetters4(nitrogenANOVA, NTukey)
print(lettersN)
lettersC <- multcompLetters4(carbonANOVA, CTukey)
print(lettersC)
lettersCN <- multcompLetters4(cnANOVA, CNTukey)
print(lettersCN)

lettersAlwat <- multcompLetters4(alumWatANOVA, alumWatTukey)
print(lettersAlwat)
lettersFewat <- multcompLetters4(ironWatANOVA, ironWatTukey)
print(lettersFewat)
lettersMnwat <- multcompLetters4(mangWatANOVA, mangWatTukey)
print(lettersMnwat)
lettersCawat <- multcompLetters4(calcWatANOVA, calcWatTukey)
print(lettersCawat)

lettersAlamm <- multcompLetters4(alumAmmANOVA, alumAmmTukey)
print(lettersAlamm)
lettersFeamm <- multcompLetters4(ironAmmANOVA, ironAmmTukey)
print(lettersFeamm)
lettersMnamm <- multcompLetters4(mangAmmANOVA, mangAmmTukey)
print(lettersMnamm)
lettersCaamm <- multcompLetters4(calcAmmANOVA, calcAmmTukey)
print(lettersCaamm)



pHTukLets <- group_by(corinthSoilmeans, Landscape) %>%
  summarise(mean=mean(pH_mean), quant = quantile(pH_mean, probs = 0.95)) %>%
  arrange(desc(mean))
NTukLets <- group_by(corinthSoilmeans, Landscape) %>%
  summarise(mean=mean(Calculated.mg.N_mean), quant = quantile(Calculated.mg.N_mean, probs = 0.95)) %>%
  arrange(desc(mean))
CTukLets <- group_by(corinthSoilmeans, Landscape) %>%
  summarise(mean=mean(Calculated.mg.C_mean), quant = quantile(Calculated.mg.C_mean, probs = 0.95)) %>%
  arrange(desc(mean))
CNTukLets <- group_by(corinthSoilmeans, Landscape) %>%
  summarise(mean=mean(CvN), quant = quantile(CvN, probs = 0.95)) %>%
  arrange(desc(mean))



# Extracting the compact letter display and adding to the Tk table
letterspH <- as.data.frame.list(letterspH$Landscape)
pHTukLets$letters <- letterspH$Letters

lettersN <- as.data.frame.list(lettersN$Landscape)
NTukLets$letters <- lettersN$Letters

lettersC <- as.data.frame.list(lettersC$Landscape)
CTukLets$letters <- lettersC$Letters

lettersCN <- as.data.frame.list(lettersCN$Landscape)
CNTukLets$letters <- lettersCN$Letters



basinpHmean <- pHTukLets$mean[2]
basinpHmean <- as.numeric(format(round(basinpHmean, 2), nsmall = 2))
hilltoppHmean <- pHTukLets$mean[1]
hilltoppHmean <- as.numeric(format(round(hilltoppHmean, 2), nsmall = 2))
shoulderpHmean <- pHTukLets$mean[3]
shoulderpHmean <- as.numeric(format(round(shoulderpHmean, 2), nsmall = 2))
slopepHmean <- pHTukLets$mean[4]
slopepHmean <- as.numeric(format(round(slopepHmean, 2), nsmall = 2))



basinNmean <- NTukLets$mean[1]
basinNmean <- as.numeric(format(round(basinNmean, 2), nsmall = 2))
hilltopNmean <- NTukLets$mean[4]
hilltopNmean <- as.numeric(format(round(hilltopNmean, 2), nsmall = 2))
shoulderNmean <- NTukLets$mean[2]
shoulderNmean <- as.numeric(format(round(shoulderNmean, 2), nsmall = 2))
slopeNmean <- NTukLets$mean[3]
slopeNmean <- as.numeric(format(round(slopeNmean, 2), nsmall = 2))



basinCmean <- CTukLets$mean[1]
basinCmean <- as.numeric(format(round(basinCmean, 2), nsmall = 2))
hilltopCmean <- CTukLets$mean[4]
hilltopCmean <- as.numeric(format(round(hilltopCmean, 2), nsmall = 2))
shoulderCmean <- CTukLets$mean[2]
shoulderCmean <- as.numeric(format(round(shoulderCmean, 2), nsmall = 2))
slopeCmean <- CTukLets$mean[3]
slopeCmean <- as.numeric(format(round(slopeCmean, 2), nsmall = 2))



basinCNmean <- CNTukLets$mean[4]
basinCNmean <- as.numeric(format(round(basinCNmean, 2), nsmall = 2))
hilltopCNmean <- CNTukLets$mean[3]
hilltopCNmean <- as.numeric(format(round(hilltopCNmean, 2), nsmall = 2))
shoulderCNmean <- CNTukLets$mean[2]
shoulderCNmean <- as.numeric(format(round(shoulderCNmean, 2), nsmall = 2))
slopeCNmean <- CNTukLets$mean[1]
slopeCNmean <- as.numeric(format(round(slopeCNmean, 2), nsmall = 2))

#Check sample size (N) of each group
summary(corinthSoilmeans$Landscape)
summary(corinthSoilmeans$Depth)



## Depth ANOVAs ----
pHANOVAdepth <- aov(pH_mean ~ Depth, data = corinthDepthSoilmeans)
summary(pHANOVAdepth)

pHerrorinfodepth <- dplyr::select(corinthDepthSoilmeans, pH_mean, Depth) %>%
  dplyr::mutate(resids=resid(pHANOVAdepth))
pHerrorinfodepth$Depth <- as.factor(pHerrorinfodepth$Depth)

pHTukeydepth <- TukeyHSD(pHANOVAdepth)
pHTukeydepth

summary.lm(pHANOVAdepth)



nitrogenANOVAdepth <- aov(Calculated.mg.N_mean ~ Depth, data = corinthDepthSoilmeans)
summary(nitrogenANOVAdepth)

Nerrorinfodepth <- dplyr::select(corinthDepthSoilmeans, Calculated.mg.N_mean, Depth) %>%
  dplyr::mutate(resids=resid(nitrogenANOVAdepth))
Nerrorinfodepth$Depth <- as.factor(Nerrorinfodepth$Depth)

NTukeydepth <- TukeyHSD(nitrogenANOVAdepth)
NTukeydepth

summary.lm(nitrogenANOVAdepth)



carbonANOVAdepth <- aov(Calculated.mg.C_mean ~ Depth, data = corinthDepthSoilmeans)
summary(carbonANOVAdepth)

Cerrorinfodepth <- dplyr::select(corinthDepthSoilmeans, Calculated.mg.C_mean, Depth) %>%
  dplyr::mutate(resids=resid(carbonANOVAdepth))
Cerrorinfodepth$Depth <- as.factor(Cerrorinfodepth$Depth)

CTukeydepth <- TukeyHSD(carbonANOVAdepth)
CTukeydepth

summary.lm(carbonANOVAdepth)



cnANOVAdepth <- aov(CvN ~ Depth, data = corinthDepthSoilmeans)
summary(cnANOVAdepth)

CNerrorinfodepth <- dplyr::select(corinthDepthSoilmeans, CvN, Depth) %>%
  dplyr::mutate(resids=resid(cnANOVAdepth))
CNerrorinfodepth$Depth <- as.factor(CNerrorinfodepth$Depth)

CNTukeydepth <- TukeyHSD(cnANOVAdepth)
CNTukeydepth

summary.lm(cnANOVAdepth)



#Preparing ANOVA graphs
letterspHdepth <- multcompLetters4(pHANOVAdepth, pHTukeydepth)
print(letterspHdepth)
lettersNdepth <- multcompLetters4(nitrogenANOVAdepth, NTukeydepth)
print(lettersNdepth)
lettersCdepth <- multcompLetters4(carbonANOVAdepth, CTukeydepth)
print(lettersCdepth)
lettersCNdepth <- multcompLetters4(cnANOVAdepth, CNTukeydepth)
print(lettersCNdepth)



pHTukLetsdepth <- group_by(corinthDepthSoilmeans, Depth) %>%
  summarise(mean=mean(pH_mean), quant = quantile(pH_mean, probs = 0.95))
NTukLetsdepth <- group_by(corinthDepthSoilmeans, Depth) %>%
  summarise(mean=mean(Calculated.mg.N_mean), quant = quantile(Calculated.mg.N_mean, probs = 0.95))
CTukLetsdepth <- group_by(corinthDepthSoilmeans, Depth) %>%
  summarise(mean=mean(Calculated.mg.C_mean), quant = quantile(Calculated.mg.C_mean, probs = 0.95))
CNTukLetsdepth <- group_by(corinthDepthSoilmeans, Depth) %>%
  summarise(mean=mean(CvN), quant = quantile(CvN, probs = 0.95))


# Extracting the compact letter display and adding to the Tk table
letterspHdepth <- as.data.frame.list(letterspHdepth$Depth)
pHTukLetsdepth$letters <- letterspHdepth$Letters

lettersNdepth <- as.data.frame.list(lettersNdepth$Depth)
NTukLetsdepth$letters <- lettersNdepth$Letters

lettersCdepth <- as.data.frame.list(lettersCdepth$Depth)
CTukLetsdepth$letters <- lettersCdepth$Letters

lettersCNdepth <- as.data.frame.list(lettersCNdepth$Depth)
CNTukLetsdepth$letters <- lettersCNdepth$Letters


tenpHmean <- pHTukLetsdepth$mean[3]
tenpHmean <- as.numeric(format(round(tenpHmean, 2), nsmall = 2))
thirtypHmean <- pHTukLetsdepth$mean[2]
thirtypHmean <- as.numeric(format(round(thirtypHmean, 2), nsmall = 2))
fiftypHmean <- pHTukLetsdepth$mean[1]
fiftypHmean <- as.numeric(format(round(fiftypHmean, 2), nsmall = 2))


tenNmean <- NTukLetsdepth$mean[3]
tenNmean <- as.numeric(format(round(tenNmean, 2), nsmall = 2))
thirtyNmean <- NTukLetsdepth$mean[2]
thirtyNmean <- as.numeric(format(round(thirtyNmean, 2), nsmall = 2))
fiftyNmean <- NTukLetsdepth$mean[1]
fiftyNmean <- as.numeric(format(round(fiftyNmean, 2), nsmall = 2))


tenCmean <- CTukLetsdepth$mean[3]
tenCmean <- as.numeric(format(round(tenCmean, 2), nsmall = 2))
thirtyCmean <- CTukLetsdepth$mean[2]
thirtyCmean <- as.numeric(format(round(thirtyCmean, 2), nsmall = 2))
fiftyCmean <- CTukLetsdepth$mean[1]
fiftyCmean <- as.numeric(format(round(fiftyCmean, 2), nsmall = 2))


tenCNmean <- CNTukLetsdepth$mean[3]
tenCNmean <- as.numeric(format(round(tenCNmean, 2), nsmall = 2))
thirtyCNmean <- CNTukLetsdepth$mean[2]
thirtyCNmean <- as.numeric(format(round(thirtyCNmean, 2), nsmall = 2))
fiftyCNmean <- CNTukLetsdepth$mean[1]
fiftyCNmean <- as.numeric(format(round(fiftyCNmean, 2), nsmall = 2))


#Check sample size (N) of each group
summary(corinthSoilmeans$Landscape)
summary(corinthSoilmeans$Depth)



# Univariate Visualization ----
#Make univariate plots for the variable for which you've hypothesized

##### pH Analysis ----
pHhist <- ggplot(corinthSoilmeans, aes(pH_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "pH", y = "Count")
pHhist

#Make boxplot to check for outliers
pHbox <- ggplot(corinthSoilmeans, aes(x = "", y = pH_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "pH")
pHbox
#Normal quantile plot (NQP)
pHNQP <- ggplot(data = pHerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "pH") 
pHNQP
ggarrange(pHhist, pHbox, pHNQP + rremove("x.text"),
          labels = c("pH Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)

###### pH Outlier Management ----
pH25quant <- quantile(corinthSoilmeans$pH_mean, 0.25)
pH75quant <- quantile(corinthSoilmeans$pH_mean, 0.75)

IQRpH <- pH75quant - pH25quant

corinthSoilmeans$pHoutliers <- (corinthSoilmeans$pH_mean < (pH25quant -  IQRpH*1.5)| corinthSoilmeans$pH_mean > (pH75quant +  IQRpH*1.5))
corinthSoilpHoutliers <- corinthSoilmeans[!(corinthSoilmeans$pHoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
pHoutANOVA <- aov(pH_mean ~ Landscape, data = corinthSoilpHoutliers)
summary(pHoutANOVA)



##### Nitrogen Analysis ----
Nhist <- ggplot(corinthSoilmeans, aes(Calculated.mg.N_mean)) +
  geom_histogram(binwidth = 0.025,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Nitrogen (mg)", y = "Count")

#Make boxplot to check for outliers
Nbox <- ggplot(corinthSoilmeans, aes(x = "", y = Calculated.mg.N_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Nitrogen (mg)")

#Normal quantile plot (NQP)
NNQP <- ggplot(data = Nerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Nitrogen (mg)") 

ggarrange(Nhist, Nbox, NNQP + rremove("x.text"),
          labels = c("Nitrogen Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)

###### Nitrogen Outlier Management ----
N25quant <- quantile(corinthSoilmeans$Calculated.mg.N_mean, 0.25)
N75quant <- quantile(corinthSoilmeans$Calculated.mg.N_mean, 0.75)

IQRN <- N75quant - N25quant

corinthSoilmeans$Noutliers <- (corinthSoilmeans$Calculated.mg.N_mean < (N25quant -  IQRN*1.5)| corinthSoilmeans$Calculated.mg.N_mean > (N75quant +  IQRN*1.5))
corinthSoilNoutliers <- corinthSoilmeans[!(corinthSoilmeans$Noutliers == TRUE),]

#Outliers drive the false significance of N
nitrogenoutANOVA <- aov(Calculated.mg.N_mean ~ Landscape, data = corinthSoilNoutliers)
summary(nitrogenoutANOVA)



##### Carbon Analysis ----
Chist <- ggplot(corinthSoilmeans, aes(Calculated.mg.C_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Carbon (mg)", y = "Count")

#Make boxplot to check for outliers
Cbox <- ggplot(corinthSoilmeans, aes(x = "", y = Calculated.mg.C_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Carbon (mg)")

#Normal quantile plot (NQP)
CNQP <- ggplot(data = Cerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Carbon (mg)") 

ggarrange(Chist, Cbox, CNQP + rremove("x.text"),
          labels = c("Carbon Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)

###### Carbon Outlier Management ----
C25quant <- quantile(corinthSoilmeans$Calculated.mg.C_mean, 0.25)
C75quant <- quantile(corinthSoilmeans$Calculated.mg.C_mean, 0.75)

IQRC <- C75quant - C25quant

corinthSoilmeans$Coutliers <- (corinthSoilmeans$Calculated.mg.C_mean < (C25quant -  IQRC*1.5)| corinthSoilmeans$Calculated.mg.C_mean > (C75quant +  IQRC*1.5))
corinthSoilCoutliers <- corinthSoilmeans[!(corinthSoilmeans$Coutliers == TRUE),]

#Outliers drive the false significance of C
carbonoutANOVA <- aov(Calculated.mg.C_mean ~ Landscape, data = corinthSoilCoutliers)
summary(carbonoutANOVA)



##### Carbon:Nitrogen Analysis ----
CNhist <- ggplot(corinthSoilmeans, aes(CvN)) +
  geom_histogram(binwidth = 2.5,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "C:N", y = "Count")

#Make boxplot to check for outliers
CNbox <- ggplot(corinthSoilmeans, aes(x = "", y = CvN)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Carbon:Nitrogen")

#Normal quantile plot (NQP)
CvNNQP <- ggplot(data = CNerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Carbon:Nitrogen") 

ggarrange(CNhist, CNbox, CvNNQP + rremove("x.text"),
          labels = c("C:N Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)

###### Carbon:Nitrogen Outlier Management ----
CN25quant <- quantile(corinthSoilmeans$CvN, 0.25)
CN75quant <- quantile(corinthSoilmeans$CvN, 0.75)

IQRCN <- CN75quant - CN25quant

corinthSoilmeans$CNoutliers <- (corinthSoilmeans$CvN < (CN25quant -  IQRCN*1.5)| corinthSoilmeans$CvN > (CN75quant +  IQRCN*1.5))
corinthSoilCNoutliers <- corinthSoilmeans[!(corinthSoilmeans$CNoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
cnoutANOVA <- aov(CvN ~ Landscape, data = corinthSoilCNoutliers)
summary(cnoutANOVA)

str(corinthSoilmeans)
















##### Water Aluminum Analysis ----
Alwathist <- ggplot(corinthSoilmeans, aes(Al.weighted.x_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Water-Extractable Al (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Alwatbox <- ggplot(corinthSoilmeans, aes(x = "", y = Al.weighted.x_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Water-Extractable Al (ug/mL)")

#Normal quantile plot (NQP)
AlwatNQP <- ggplot(data = alumWaterrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Water-Extractable Al (ug/mL)") 

ggarrange(Alwathist, Alwatbox, AlwatNQP + rremove("x.text"),
          labels = c("Water-Extr. Aluminum Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Water Aluminum Outlier Management ----
Alwat25quant <- quantile(corinthSoilmeans$Al.weighted.x_mean, 0.25)
Alwat75quant <- quantile(corinthSoilmeans$Al.weighted.x_mean, 0.75)

IQRAlwat <- Alwat75quant - Alwat25quant

corinthSoilmeans$Alwatoutliers <- (corinthSoilmeans$Al.weighted.x_mean < (Alwat25quant -  IQRAlwat*1.5)| corinthSoilmeans$Al.weighted.x_mean > (Alwat75quant +  IQRAlwat*1.5))
corinthSoilAlwatoutliers <- corinthSoilmeans[!(corinthSoilmeans$Alwatoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
AlwatoutANOVA <- aov(Al.weighted.x_mean ~ Landscape, data = corinthSoilAlwatoutliers)
summary(AlwatoutANOVA)

str(corinthSoilmeans)











##### Water Iron Analysis ----
Fewathist <- ggplot(corinthSoilmeans, aes(Fe.weighted.x_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Water-Extractable Fe (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Fewatbox <- ggplot(corinthSoilmeans, aes(x = "", y = Fe.weighted.x_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Water-Extractable Fe (ug/mL)")

#Normal quantile plot (NQP)
FewatNQP <- ggplot(data = ironWaterrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Water-Extractable Fe (ug/mL)") 

ggarrange(Fewathist, Fewatbox, FewatNQP + rremove("x.text"),
          labels = c("Water-Extr. Iron Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)

###### Water Iron Outlier Management ----
Fewat25quant <- quantile(corinthSoilmeans$Fe.weighted.x_mean, 0.25)
Fewat75quant <- quantile(corinthSoilmeans$Fe.weighted.x_mean, 0.75)

IQRFewat <- Fewat75quant - Fewat25quant

corinthSoilmeans$Fewatoutliers <- (corinthSoilmeans$Fe.weighted.x_mean < (Fewat25quant -  IQRFewat*1.5)| corinthSoilmeans$Fe.weighted.x_mean > (Fewat75quant +  IQRFewat*1.5))
corinthSoilFewatoutliers <- corinthSoilmeans[!(corinthSoilmeans$Fewatoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
FewatoutANOVA <- aov(Fe.weighted.x_mean ~ Landscape, data = corinthSoilFewatoutliers)
summary(FewatoutANOVA)

str(corinthSoilmeans)







##### Water Calcium Analysis ----
Cawathist <- ggplot(corinthSoilmeans, aes(Ca.weighted.x_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Water-Extractable Ca (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Cawatbox <- ggplot(corinthSoilmeans, aes(x = "", y = Ca.weighted.x_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Water-Extractable Ca (ug/mL)")

#Normal quantile plot (NQP)
CawatNQP <- ggplot(data = calcWaterrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Water-Extractable Ca (ug/mL)") 

ggarrange(Cawathist, Cawatbox, CawatNQP + rremove("x.text"),
          labels = c("Water-Extr. Calcium Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Water Calcium Outlier Management ----
Cawat25quant <- quantile(corinthSoilmeans$Ca.weighted.x_mean, 0.25)
Cawat75quant <- quantile(corinthSoilmeans$Ca.weighted.x_mean, 0.75)

IQRCawat <- Cawat75quant - Cawat25quant

corinthSoilmeans$Cawatoutliers <- (corinthSoilmeans$Ca.weighted.x_mean < (Cawat25quant -  IQRCawat*1.5)| corinthSoilmeans$Ca.weighted.x_mean > (Cawat75quant +  IQRCawat*1.5))
corinthSoilCawatoutliers <- corinthSoilmeans[!(corinthSoilmeans$Cawatoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
CawatoutANOVA <- aov(Ca.weighted.x_mean ~ Landscape, data = corinthSoilCawatoutliers)
summary(CawatoutANOVA)

str(corinthSoilmeans)











##### Water Manganese Analysis ----
Mnwathist <- ggplot(corinthSoilmeans, aes(Mn.weighted.x_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Water-Extractable Mn (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Mnwatbox <- ggplot(corinthSoilmeans, aes(x = "", y = Mn.weighted.x_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Water-Extractable Mn (ug/mL)")

#Normal quantile plot (NQP)
MnwatNQP <- ggplot(data = mangWaterrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Water-Extractable Mn (ug/mL)") 

ggarrange(Mnwathist, Mnwatbox, MnwatNQP + rremove("x.text"),
          labels = c("Water-Extr. Manganese Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Water Manganese Outlier Management ----
Mnwat25quant <- quantile(corinthSoilmeans$Mn.weighted.x_mean, 0.25)
Mnwat75quant <- quantile(corinthSoilmeans$Mn.weighted.x_mean, 0.75)

IQRMnwat <- Mnwat75quant - Mnwat25quant

corinthSoilmeans$Mnwatoutliers <- (corinthSoilmeans$Mn.weighted.x_mean < (Mnwat25quant -  IQRMnwat*1.5)| corinthSoilmeans$Mn.weighted.x_mean > (Mnwat75quant +  IQRMnwat*1.5))
corinthSoilMnwatoutliers <- corinthSoilmeans[!(corinthSoilmeans$Mnwatoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
MnwatoutANOVA <- aov(Mn.weighted.x_mean ~ Landscape, data = corinthSoilMnwatoutliers)
summary(MnwatoutANOVA)

str(corinthSoilmeans)











##### Amm. Acetate Aluminum Analysis QUERY ----
Alammhist <- ggplot(corinthSoilmeans, aes(Al.weighted.y_mean)) +
  geom_histogram(binwidth = 0.01,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Amm. Acetate-Extractable Al (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Alammbox <- ggplot(corinthSoilmeans, aes(x = "", y = Al.weighted.y_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Amm. Acetate-Extractable Al (ug/mL)")

#Normal quantile plot (NQP)
AlammNQP <- ggplot(data = alumAmmerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Amm. Acetate-Extractable Al (ug/mL)") 

ggarrange(Alammhist, Alammbox, AlammNQP + rremove("x.text"),
          labels = c("Amm. Acetate-Extr. Aluminum Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Amm. Acetate Aluminum Outlier Management ----
Alamm25quant <- quantile(corinthSoilmeans$Al.weighted.y_mean, 0.25)
Alamm75quant <- quantile(corinthSoilmeans$Al.weighted.y_mean, 0.75)

IQRAlamm <- Alamm75quant - Alamm25quant

corinthSoilmeans$Alammoutliers <- (corinthSoilmeans$Al.weighted.y_mean < (Alamm25quant -  IQRAlamm*1.5)| corinthSoilmeans$Al.weighted.y_mean > (Alamm75quant +  IQRAlamm*1.5))
corinthSoilAlammoutliers <- corinthSoilmeans[!(corinthSoilmeans$Alammoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
AlammoutANOVA <- aov(Al.weighted.y_mean ~ Landscape, data = corinthSoilAlammoutliers)
summary(AlammoutANOVA)

str(corinthSoilmeans)








##### Amm. Acetate Iron Analysis QUERY ----
Feammhist <- ggplot(corinthSoilmeans, aes(Fe.weighted.y_mean)) +
  geom_histogram(binwidth = 0.01,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Amm. Acetate-Extractable Fe (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Feammbox <- ggplot(corinthSoilmeans, aes(x = "", y = Fe.weighted.y_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Amm. Acetate-Extractable Fe (ug/mL)")

#Normal quantile plot (NQP)
FeammNQP <- ggplot(data = ironAmmerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Amm. Acetate-Extractable Al (ug/mL)") 

ggarrange(Feammhist, Feammbox, FeammNQP + rremove("x.text"),
          labels = c("Amm. Acetate-Extr. Iron Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Amm. Acetate Iron Outlier Management ----
Feamm25quant <- quantile(corinthSoilmeans$Fe.weighted.y_mean, 0.25)
Feamm75quant <- quantile(corinthSoilmeans$Fe.weighted.y_mean, 0.75)

IQRFeamm <- Feamm75quant - Feamm25quant

corinthSoilmeans$Feammoutliers <- (corinthSoilmeans$Fe.weighted.y_mean < (Feamm25quant -  IQRFeamm*1.5)| corinthSoilmeans$Fe.weighted.y_mean > (Feamm75quant +  IQRFeamm*1.5))
corinthSoilFeammoutliers <- corinthSoilmeans[!(corinthSoilmeans$Feammoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
FeammoutANOVA <- aov(Fe.weighted.y_mean ~ Landscape, data = corinthSoilFeammoutliers)
summary(FeammoutANOVA)

str(corinthSoilmeans)









##### Amm. Acetate Calcium Analysis QUERY ----
Caammhist <- ggplot(corinthSoilmeans, aes(Ca.weighted.y_mean)) +
  geom_histogram(binwidth = 0.5,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Amm. Acetate-Extractable Ca (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Caammbox <- ggplot(corinthSoilmeans, aes(x = "", y = Ca.weighted.y_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Amm. Acetate-Extractable Ca (ug/mL)")

#Normal quantile plot (NQP)
CaammNQP <- ggplot(data = calcAmmerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Amm. Acetate-Extractable Ca (ug/mL)") 

ggarrange(Caammhist, Caammbox, CaammNQP + rremove("x.text"),
          labels = c("Amm. Acetate-Extr. Calcium Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Amm. Acetate Calcium Outlier Management ----
Caamm25quant <- quantile(corinthSoilmeans$Ca.weighted.y_mean, 0.25)
Caamm75quant <- quantile(corinthSoilmeans$Ca.weighted.y_mean, 0.75)

IQRCaamm <- Caamm75quant - Caamm25quant

corinthSoilmeans$Caammoutliers <- (corinthSoilmeans$Ca.weighted.y_mean < (Caamm25quant -  IQRCaamm*1.5)| corinthSoilmeans$Ca.weighted.y_mean > (Caamm75quant +  IQRCaamm*1.5))
corinthSoilCaammoutliers <- corinthSoilmeans[!(corinthSoilmeans$Caammoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
CaammoutANOVA <- aov(Ca.weighted.y_mean ~ Landscape, data = corinthSoilCaammoutliers)
summary(CaammoutANOVA)

str(corinthSoilmeans)










##### Amm. Acetate Manganese Analysis QUERY ----
Mnammhist <- ggplot(corinthSoilmeans, aes(Mn.weighted.y_mean)) +
  geom_histogram(binwidth = 0.25,
                 col = "black",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "purple", high = "orange") +
  theme(legend.position = "none") +
  labs(x = "Amm. Acetate-Extractable Mn (ug/mL)", y = "Count")

#Make boxplot to check for outliers
Mnammbox <- ggplot(corinthSoilmeans, aes(x = "", y = Mn.weighted.y_mean)) +
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "", y = "Amm. Acetate-Extractable Mn (ug/mL)")

#Normal quantile plot (NQP)
MnammNQP <- ggplot(data = mangAmmerrorinfo, aes(sample = resids)) + 
  geom_qq( ) + geom_qq_line( ) + 
  labs(x = "Theoretical", y = "Amm. Acetate-Extractable Mn (ug/mL)") 

ggarrange(Mnammhist, Mnammbox, MnammNQP + rremove("x.text"),
          labels = c("Amm. Acetate-Extr. Manganese Histogram", "Boxplot", "Residual NQP"),
          ncol = 3, nrow = 1)


###### Amm. Acetate Manganese Outlier Management ----
Mnamm25quant <- quantile(corinthSoilmeans$Mn.weighted.y_mean, 0.25)
Mnamm75quant <- quantile(corinthSoilmeans$Mn.weighted.y_mean, 0.75)

IQRMnamm <- Mnamm75quant - Mnamm25quant

corinthSoilmeans$Mnammoutliers <- (corinthSoilmeans$Mn.weighted.y_mean < (Mnamm25quant -  IQRMnamm*1.5)| corinthSoilmeans$Mn.weighted.y_mean > (Mnamm75quant +  IQRMnamm*1.5))
corinthSoilMnammoutliers <- corinthSoilmeans[!(corinthSoilmeans$Mnammoutliers == TRUE),]

#Outliers do not change the results of the ANOVA
MnammoutANOVA <- aov(Mn.weighted.y_mean ~ Landscape, data = corinthSoilMnammoutliers)
summary(MnammoutANOVA)

str(corinthSoilmeans)











#### Landscape ANOVA Figures ####
##### Landscape pH ANOVA Figure ####
pHfigANOVA <- ggplot(corinthLandSoilmeans, aes(x = Landscape, y = pH_mean, color = Landscape, shape = Landscape)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.y=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, linetype = "solid") +
  geom_text(data = pHTukLets, aes(x = Landscape, y = quant, label = letters), size = 5, color = "black", vjust=-2, hjust =-1) +
  annotate("text", x = c(1.3, 2.3, 3.3, 4.3),
           y = c((basinpHmean + 0.1), (shoulderpHmean + 0.1), (slopepHmean + 0.1), (hilltoppHmean + 0.1)), 
           label = c(basinpHmean, shoulderpHmean, slopepHmean, hilltoppHmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Landscape Position", y = "pH")
pHfigANOVA



##### Landscape N ANOVA Figure ####
NfigANOVA <- ggplot(corinthLandSoilmeans, aes(x = Landscape, y = Calculated.mg.N_mean, color = Landscape, shape = Landscape)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.y=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, linetype = "solid") +
  geom_text(data = NTukLets, aes(x = Landscape, y = quant, label = letters), size = 5, color = "black", vjust=-2, hjust =-1) +
  annotate("text", x = c(1.3, 2.3, 3.3, 4.3),
           y = c((basinNmean + 0.01), (shoulderNmean + 0.01), (slopeNmean + 0.01), (hilltopNmean + 0.01)), 
           label = c(basinNmean, shoulderNmean, slopeNmean, hilltopNmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Landscape Position", y = "Nitrogen (mg)")
NfigANOVA



##### Landscape C ANOVA Figure ####
CfigANOVA <- ggplot(corinthLandSoilmeans, aes(x = Landscape, y = Calculated.mg.C_mean, color = Landscape, shape = Landscape)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.y=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, linetype = "solid") +
  geom_text(data = CTukLets, aes(x = Landscape, y = quant, label = letters), size = 5, color = "black", vjust=-2, hjust =-1) +
  annotate("text", x = c(1.3, 2.3, 3.3, 4.3),
           y = c((basinCmean + 0.1), (shoulderCmean + 0.1), (slopeCmean + 0.1), (hilltopCmean + 0.1)), 
           label = c(basinCmean, shoulderCmean, slopeCmean, hilltopCmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Landscape Position", y = "Carbon (mg)")
CfigANOVA



##### Landscape CN ANOVA Figure ####
CNfigANOVA <- ggplot(corinthLandSoilmeans, aes(x = Landscape, y = CvN, color = Landscape, shape = Landscape)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.y=mean, geom="errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, linetype = "solid") +
  geom_text(data = CNTukLets, aes(x = Landscape, y = quant, label = letters), size = 5, color = "black", vjust=-2, hjust =-1) +
  annotate("text", x = c(1.3, 2.3, 3.3, 4.3),
           y = c((basinCNmean + 2.5), (shoulderCNmean + 2.5), (slopeCNmean + 2.5), (hilltopCNmean + 2.5)), 
           label = c(basinCNmean, shoulderCNmean, slopeCNmean, hilltopCNmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Landscape Position", y = "Carbon:Nitrogen")
CNfigANOVA

























#### Depth ANOVA Figures ####
##### Depth pH ANOVA Figure ####
pHfigANOVAdepth <- ggplot(corinthDepthSoilmeans, aes(x = pH_mean, y = Depth, color = Depth, shape = Depth)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.x=mean, geom="errorbar", aes(xmax = ..x.., xmin = ..x..),
               width = 0.75, linetype = "solid") +
  geom_text(data = pHTukLetsdepth, aes(x = mean, y = Depth, label = letters), size = 5, color = "black", hjust = -30) +
  annotate("text", y = c(1, 2, 3),
           x = c((fiftypHmean + 0.1), (thirtypHmean + 0.1), (tenpHmean + 0.1)), 
           label = c(fiftypHmean, thirtypHmean, tenpHmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "pH", y = "Depth (cm)")
pHfigANOVAdepth



##### Depth N ANOVA Figure ####
NfigANOVAdepth <- ggplot(corinthDepthSoilmeans, aes(x = Calculated.mg.N_mean, y = Depth, color = Depth, shape = Depth)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.x=mean, geom="errorbar", aes(xmax = ..x.., xmin = ..x..),
               width = 0.75, linetype = "solid") +
  geom_text(data = NTukLetsdepth, aes(x = mean, y = Depth, label = letters), size = 5, color = "black", hjust =-30) +
  annotate("text", y = c(1, 2, 3),
           x = c((fiftyNmean + 0.025), (thirtyNmean + 0.025), (tenNmean + 0.025)), 
           label = c(fiftyNmean, thirtyNmean, tenNmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Nitrogen (mg)", y = "Depth (cm)")
NfigANOVAdepth



##### Depth C ANOVA Figure ####
CfigANOVAdepth <- ggplot(corinthDepthSoilmeans, aes(x = Calculated.mg.C_mean, y = Depth, color = Depth, shape = Depth)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.x=mean, geom="errorbar", aes(xmax = ..x.., xmin = ..x..),
               width = 0.75, linetype = "solid") +
  geom_text(data = CTukLetsdepth, aes(x = mean, y = Depth, label = letters), size = 5, color = "black", hjust =-30) +
  annotate("text", y = c(1, 2, 3),
           x = c((fiftyCmean + 0.1), (thirtyCmean + 0.1), (tenCmean + 0.1)), 
           label = c(fiftyCmean, thirtyCmean, tenCmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Carbon (mg)", y = "Depth (cm)")
CfigANOVAdepth



##### Depth CN ANOVA Figure ####
CNfigANOVAdepth <- ggplot(corinthDepthSoilmeans, aes(x = CvN, y = Depth, color = Depth, shape = Depth)) +
  geom_jitter(width = 0.25) +
  stat_summary(fun.x=mean, geom="errorbar", aes(xmax = ..x.., xmin = ..x..),
               width = 0.75, linetype = "solid") +
  geom_text(data = CNTukLetsdepth, aes(x = mean, y = Depth, label = letters), size = 5, color = "black", hjust =-30) +
  annotate("text", y = c(1, 2, 3),
           x = c((fiftyCNmean + 2.5), (thirtyCNmean + 2.5), (tenCNmean + 2.5)), 
           label = c(fiftyCNmean, thirtyCNmean, tenCNmean), 
           family = "", size=5) +
  scale_color_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Carbon:Nitrogen", y = "Depth (cm)")
CNfigANOVAdepth


#### Two-way ANOVA Figures ####
##### Two-Way pH ANOVA Figure ####
pHfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = pH_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "pH", y = "Depth (cm)")
pHfigANOVAtwo



##### Two-Way N ANOVA Figure ####
NfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Calculated.mg.N_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Nitrogen (mg)", y = "Depth (cm)")
NfigANOVAtwo



##### Two-Way C ANOVA Figure ####
CfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Calculated.mg.C_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Carbon (mg)", y = "Depth (cm)")
CfigANOVAtwo



##### Two-Way CN ANOVA Figure ####
CNfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = CvN, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Carbon:Nitrogen", y = "Depth (cm)")
CNfigANOVAtwo



##### Two-Way Water-Extr. Al ANOVA Figure ####
AlwatfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Al.weighted.x_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Water-Extr. Aluminum", y = "Depth (cm)")
AlwatfigANOVAtwo



##### Two-Way Water-Extr. Fe ANOVA Figure ####
FewatfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Fe.weighted.x_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Water-Extr. Iron", y = "Depth (cm)")
FewatfigANOVAtwo


##### Two-Way Water-Extr. Ca ANOVA Figure ####
CawatfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Ca.weighted.x_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Water-Extr. Calcium", y = "Depth (cm)")
CawatfigANOVAtwo


##### Two-Way Water-Extr. Mn ANOVA Figure ####
MnwatfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Mn.weighted.x_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Water-Extr. Manganese", y = "Depth (cm)")
MnwatfigANOVAtwo


##### Two-Way Amm. Acetate-Extr. Al ANOVA Figure ####
AlammfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Al.weighted.y_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Amm. Acetate-Extr. Aluminum", y = "Depth (cm)")
AlammfigANOVAtwo


##### Two-Way Amm. Acetate-Extr. Fe ANOVA Figure ####
FeammfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Fe.weighted.y_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Amm. Acetate-Extr. Iron", y = "Depth (cm)")
FeammfigANOVAtwo


##### Two-Way Amm. Acetate-Extr. Ca ANOVA Figure ####
CaammfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Ca.weighted.y_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Amm. Acetate-Extr. Calcium", y = "Depth (cm)")
CaammfigANOVAtwo


##### Two-Way Amm. Acetate-Extr. Mn ANOVA Figure ####
MnammfigANOVAtwo <- ggplot(corinthSpaceSoilmeans, aes(x = Mn.weighted.y_mean, y = Depth, fill = Landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu"))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12, colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title.x=element_text(size=14, colour="black"),
        axis.title.y=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        strip.background = element_rect(color = "black", fill = "white", size = 0.75),
        axis.line=element_line(colour="black")) +
  labs(x = "Amm. Acetate-Extr. Manganese", y = "Depth (cm)")
MnammfigANOVAtwo




#### Bivariate Analysis ####
##### Water-Extr. Fe vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
watFevCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Fe.weighted.x_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(watFevCreg) 
#Look into storage structure of linear regression model
str(summary(watFevCreg))
#Extract important values
rsquared <- round(summary(watFevCreg)$r.squared, 3)
slope <- round(summary(watFevCreg)$coefficients[2], 3)
intercept <- round(summary(watFevCreg)$coefficients[1], 3)

watFevCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Fe.weighted.x_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Fe.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
    y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Fe.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
    y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Water-Extr. Fe (ug/mL)", y = "Bulk C (mg)")
watFevCregfig



##### Water-Extr. Al vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
watAlvCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Al.weighted.x_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(watAlvCreg) 
#Look into storage structure of linear regression model
str(summary(watAlvCreg))
#Extract important values
rsquared <- round(summary(watAlvCreg)$r.squared, 3)
slope <- round(summary(watAlvCreg)$coefficients[2], 3)
intercept <- round(summary(watAlvCreg)$coefficients[1], 3)

watAlvCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Al.weighted.x_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Al.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Al.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Water-Extr. Al (ug/mL)", y = "Bulk C (mg)")
watAlvCregfig



##### Water-Extr. Ca vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
watCavCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Ca.weighted.x_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(watCavCreg) 
#Look into storage structure of linear regression model
str(summary(watCavCreg))
#Extract important values
rsquared <- round(summary(watCavCreg)$r.squared, 3)
slope <- round(summary(watCavCreg)$coefficients[2], 3)
intercept <- round(summary(watCavCreg)$coefficients[1], 3)

watCavCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Ca.weighted.x_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Ca.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Ca.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Water-Extr. Ca (ug/mL)", y = "Bulk C (mg)")
watCavCregfig


##### Water-Extr. Mn vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
watMnvCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Mn.weighted.x_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(watMnvCreg) 
#Look into storage structure of linear regression model
str(summary(watMnvCreg))
#Extract important values
rsquared <- round(summary(watMnvCreg)$r.squared, 3)
slope <- round(summary(watMnvCreg)$coefficients[2], 3)
intercept <- round(summary(watMnvCreg)$coefficients[1], 3)

watMnvCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Mn.weighted.x_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Mn.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Mn.weighted.x_mean)-0.1, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Water-Extr. Mn (ug/mL)", y = "Bulk C (mg)")
watMnvCregfig


##### Amm. Actate-Extr. Fe vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
ammFevCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Fe.weighted.y_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(ammFevCreg) 
#Look into storage structure of linear regression model
str(summary(ammFevCreg))
#Extract important values
rsquared <- round(summary(ammFevCreg)$r.squared, 3)
slope <- round(summary(ammFevCreg)$coefficients[2], 3)
intercept <- round(summary(ammFevCreg)$coefficients[1], 3)

ammFevCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Fe.weighted.y_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Fe.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Fe.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Ammonium Acetate-Extr. Fe (ug/mL)", y = "Bulk C (mg)")
ammFevCregfig



##### Amm. Actate-Extr. Al vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
ammAlvCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Al.weighted.y_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(ammAlvCreg) 
#Look into storage structure of linear regression model
str(summary(ammAlvCreg))
#Extract important values
rsquared <- round(summary(ammAlvCreg)$r.squared, 3)
slope <- round(summary(ammAlvCreg)$coefficients[2], 3)
intercept <- round(summary(ammAlvCreg)$coefficients[1], 3)

ammAlvCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Al.weighted.y_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Al.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Al.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Ammonium Acetate-Extr. Al (ug/mL)", y = "Bulk C (mg)")
ammAlvCregfig



##### Amm. Actate-Extr. Ca vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
ammCavCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Ca.weighted.y_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(ammCavCreg) 
#Look into storage structure of linear regression model
str(summary(ammCavCreg))
#Extract important values
rsquared <- round(summary(ammCavCreg)$r.squared, 3)
slope <- round(summary(ammCavCreg)$coefficients[2], 3)
intercept <- round(summary(ammCavCreg)$coefficients[1], 3)

ammCavCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Ca.weighted.y_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Ca.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Ca.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Ammonium Acetate-Extr. Ca (ug/mL)", y = "Bulk C (mg)")
ammCavCregfig


##### Amm. Actate-Extr. Mn vs. Bulk C Regression ####
#Fit linear model ([response variable] ~ [explanatory variable])
ammMnvCreg <- lm(corinthSoilmeans$Calculated.mg.C_mean ~ corinthSoilmeans$Mn.weighted.y_mean) 
#Summarize, info for regression equation is under 'Estimate', R-squared value is under 'Multiple R-squared', ignore other outputs until inferential statistics are introduced later in term
summary(ammMnvCreg) 
#Look into storage structure of linear regression model
str(summary(ammMnvCreg))
#Extract important values
rsquared <- round(summary(ammMnvCreg)$r.squared, 3)
slope <- round(summary(ammMnvCreg)$coefficients[2], 3)
intercept <- round(summary(ammMnvCreg)$coefficients[1], 3)

ammMnvCregfig <- corinthSoilmeans %>%
  ggplot(aes(x = Mn.weighted.y_mean, y = Calculated.mg.C_mean)) + 
  geom_point() +
  annotate("text", x = max(corinthSoilmeans$Mn.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 3, label = paste(c("y = ", slope, "x + ", intercept), collapse = "")) +
  annotate("text", x = max(corinthSoilmeans$Mn.weighted.y_mean)-0.005, #When X is log10 make # subtracted small
           y = 2.5, label = paste(c("r-squared = ", rsquared), collapse = "")) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  theme_bw() +
  labs(x = "Ammonium Acetate-Extr. Mn (ug/mL)", y = "Bulk C (mg)")
ammMnvCregfig