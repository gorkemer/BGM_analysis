# script for evaluting demographics of bgm experiment
# start date 25 Sept 2020
# gorkemer

# last edit: Late Agu 2022

#### 0- load libraries

# gorkemer
# 5 August 
# analyze on the cleaned data (output of the prepareData_01.R)
# extras: #colnames(group_mean) <- c("Mean Response AR", "CuedAR", "id")
### Load libraries ###
library(reshape2); library(rlang); library(data.table); library(plyr) # data formatting
library(psych); library(pastecs) # statistics
library(lme4); library(car); library(ggpubr); library(corrplot); library(magrittr) # regression
library(ggplot2); library(Hmisc) # graphing
library(dplyr) # data wrangling
library("reshape2");library("plot3D")library(extrafont)library("scatterplot3d")
library(fitdistrplus); library(MASS); library(survival); library(npsurv) ; library(lsei)
library(actuar)library(statmod)
library(Hmisc)library(broom)library(quickpsy)library(tidyverse)

# clear the workspace
rm(list=ls()) 

# set wd if needed setwd('~/Desktop/background-motion/Data Analysis'), check with getwd()
setwd("/Users/gorkem.er/Desktop/21Projects/background-motion/Data_analysis")
#### 0- Select Data ####

d = read.csv('demographics-bgm.csv', header=TRUE) ## same-different response test run without random motion and now it is just L-R
#remove width or height with value of 2414
#17952, 19101, 19198, 19210 are empty participants. The reall participant number is:
data = d
#change some column names
colnames(data)[which(names(data) == "Q4")] <- "age"
ageRange <- na.omit(as.numeric(data$age))
#get the age range
range(ageRange)
colnames(data)[which(names(data) == "Q5")] <- "gender"
bgm_demog <- data[,c("gender", "id")] #to-be used for the manuscript
head(bgm_demog)
bgm_demog <- bgm_demog[-1,] # remove first column-row from the table. 
bgm_demog$gender <- as.factor(bgm_demog$gender) 
# get the first row a duplicated ids
bgm_demog <- subset(bgm_demog,duplicated(bgm_demog$id)|!duplicated(bgm_demog$id,fromLast=TRUE))
head(bgm_demog)
#### copied from bgm_analyse.R script ####
# load bgm data 
bgmdata = read.csv("bgmdata.csv",header=TRUE, quote="\"") 
bgmdata.c = subset(bgmdata, bgmdata$randomTrialsR1C0 == 0)
bgmdata.r = subset(bgmdata, bgmdata$randomTrialsR1C0 == 1)
# load bgmn data 
bgmndata = read.csv("bgmndata.csv",header=TRUE, quote="\"") 
bgmndata.c = subset(bgmndata, bgmndata$randomTrialsR1C0 == 0)
bgmndata.r = subset(bgmndata, bgmndata$randomTrialsR1C0 == 1)
# fgm -> bgm
fgmdata <- bgmdata.c
# FOR RANDOM MOTION ANALYSES bgmdata.r
# fgmdata <- bgmdata.r
number_of_sub <- unique(fgmdata$sub)
fgmdata$cuedAR <- round(fgmdata$cuedAR, digits = 2)
fgmdata$responseAR <- round(fgmdata$responseAR, digits = 2)
fgmdata$cuedCat = ifelse(fgmdata$cuedAR < 0, -1, ifelse(fgmdata$cuedAR==-0, 0, 1))
fgmdata$respAcc <- ifelse( (fgmdata$responseAR > 0 & fgmdata$cuedCat == 1)  | (fgmdata$responseAR < 0 & fgmdata$cuedCat == -1) | (fgmdata$responseAR == 0 & fgmdata$cuedCat == 0), 1, 0)
table(fgmdata$respAcc)
# iterate all and get the beta coefficient
fgmdata.globalTrialN <- data.frame(matrix(ncol = 1, nrow = length(number_of_sub)))
for (s in 1:length(number_of_sub)){
  print(s)
  tmpdata <- fgmdata[fgmdata$sub == number_of_sub[s],]
  fgmdata.globalTrialN[s,1] <- nrow(tmpdata)# check the n of each participant
  fgmdata.globalTrialN[s,2] <- number_of_sub[s]
}
colnames(fgmdata.globalTrialN) <- c("trialN","id")
head(fgmdata.globalTrialN)
incompletedPeople_global <- fgmdata.globalTrialN$id[fgmdata.globalTrialN$trialN<241]
plot(fgmdata.globalTrialN[,2], fgmdata.globalTrialN[,1])
abline(h = 241)
incompletedPeople_global
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople_global)),]
fgmdata <- fgmdata[!(fgmdata$sub == 18667),] # because 0.53 corr\
length(unique(fgmdata$sub))
#matching demog and data used in the manuscript
#bgm_demog <- bgm_demog[-which(d$id == ""),]
# there is cell with empty id value, removing it
for (i in 1:length(unique(bgm_demog$id))){
  data_id  <- unique(fgmdata$sub)[i]
  demog_id <- as.numeric(unique(bgm_demog$id)[i])
  #search this id in the data and if there is one, put check
  bgm_demog[i,"exists_in_data"] = ifelse(demog_id %in% as.numeric(unique(fgmdata$sub)), "exists", "dontExists")
}
head(bgm_demog)
table(bgm_demog$exists_in_data)
# get only the exists ones
bgm_demog <- bgm_demog[bgm_demog$exists_in_data == "exists",]
table(bgm_demog$exists_in_data)
# counting blank people to "prefer not to say"
table(bgm_demog$gender)
