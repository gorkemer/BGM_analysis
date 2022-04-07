# script for evaluting demographics of bgm experiment
# start date 25 Sept 2020
# gorkemer


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

# set wd if needed setwd('~/Desktop/background-motion/Data Analysis'), check with getwd()
setwd('~/Desktop/background-motion/demog_analysis')
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


