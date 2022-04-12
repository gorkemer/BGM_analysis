# gorkemer
# 12 April
# adapted from bgm_09_popResp.R

# clear the workspace
rm(list=ls()) 

# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker")

require(x)
lapply(x, require, character.only = TRUE)
source("../../plot_functions.R")

# remove scientific notation in the entire R session
options(scipen = 100)

# load data
setwd("/Users/gorkem.er/Desktop/21Projects/background-motion/Data_analysis")
bgmndata = read.csv("bgmdata.csv",header=TRUE, quote="\"") 


# define the data frame to use
bgmndata.Same <- subset(bgmndata, bgmndata$pairSameARCat==1 & bgmndata$randomTrialsR1C0 == 0)
bgmndata.Diff <- subset(bgmndata, !(bgmndata$pairSameARCat==1))

# with id
bgmndata.Same_agg <- aggregate(responseAR ~ cuedAR + sub, data = bgmndata.Same, mean)
bgmndata.Diff_agg <- aggregate(responseAR ~ cuedAR + sub, data = bgmndata.Diff, mean)

# 1- find the number of people who have "same" trials for the full range of cuedAR ####
# I sum all the cuedAR for each sub, and found that all participants had same trial for full 
# range of cuedAR
data_agg_same_sum <- aggregate(cuedAR ~ sub, data = bgmndata.Same_agg,sum)
nrow(data_agg_same_sum) #104 for untouched data


# 2- find population and invidual response for same-AR trials
popResp_same <- aggregate(responseAR ~ cuedAR, data = bgmndata.Same, mean) #population response
indResp_same<- aggregate(responseAR ~ cuedAR + sub, data = bgmndata.Same, mean)#individual response 

# find the difference of pop and indv response to same AR-trials
indResp_same$popInvDiff <- NA

for (i in 1:nrow(popResp_same)){
  selectedCuedAR <- popResp_same$cuedAR[i]
  popResp <- popResp_same$responseAR[i]
  for (a in 1:nrow(indResp_same)){
    if (selectedCuedAR == indResp_same$cuedAR[a]){
      indResp_same$popInvDiff[a] <-  indResp_same$responseAR[a] - popResp
      indResp_same$popResp[a] <-  popResp
    }
  }
}

# let's normalize responses by substracting responses to same-AR trials for each 
# cued AR for each participant.

subject_IDs <- unique(bgmndata$sub)
cuedAR_IDs <- unique(bgmndata$cuedAR)

# Check if 'numeric(0)'
is.numeric0 <- function(x) {
  identical(x, numeric(0))
}

bgmndata$normedR_indv <- NA
for (i in 1:nrow(bgmndata)){
  row_sub_ID <- bgmndata$sub[i]
  row_cuedAR <- bgmndata$cuedAR[i]
  print(row_sub_ID)
  print(row_cuedAR)
  is.na(indResp_same$responseAR[indResp_same$sub == row_sub_ID & indResp_same$cuedAR == row_cuedAR])
  print( indResp_same$responseAR[indResp_same$sub == row_sub_ID & indResp_same$cuedAR == row_cuedAR])
  if (!(is.numeric0(indResp_same$responseAR[indResp_same$sub == row_sub_ID & indResp_same$cuedAR == row_cuedAR]))) {
    bgmndata$normedR_indv[i] <- bgmndata$responseAR[i] - indResp_same$responseAR[indResp_same$sub == row_sub_ID & indResp_same$cuedAR == row_cuedAR]
  }
  else{  # some participants has no individual response to some cued AR, e.g. 19234 has no 0.0000
    bgmndata$normedR_indv[i] <- popResp_same$responseAR[popResp_same$cuedAR == 0.0000] #for those people, I used population response 
  }
}

# let's count how many same-AR trials each participant has for every cuedAR 
n_of_sameAR_per_sub <- aggregate(pairSameARCat ~ sub + cuedAR, bgmndata, sum)
n_of_sameAR_per_sub <- n_of_sameAR_per_sub[order(n_of_sameAR_per_sub$sub),] # sort by sub to eye ball
head(n_of_sameAR_per_sub)

# let's see how many 0's there are
no_sameAR_trials <- n_of_sameAR_per_sub[n_of_sameAR_per_sub$pairSameARCat == 0,]
# only 3 people had 0 occurrence of same-AR trials. this might be okay. 
# 1) sub_19146, 2) sub_19234, 3) sub_19252 (all cuedAR == 0.000)

# what is the population response for those subjects (and 0.000)
popResp_same$responseAR[popResp_same$cuedAR == 0.0000]

# let's find how many <6 out there
less_sameAR_trials <- n_of_sameAR_per_sub[n_of_sameAR_per_sub$pairSameARCat < 6,]
less_sameAR_trials # many. We might need to use pop response for those.

# anyway, we got our normalized response AR now. 
# let's run basic regression analyses

lesser.m1 <- lm(normedR_indv~ cuedAR *uncuedAR * as.factor(sameDirection1S0D_R2), bgmndata)
summary(lesser.m1)
anova(lesser.m1)

#check only COHERENT data
bgmndata.c <- bgmndata[bgmndata$randomTrialsR1C0 == 0, ]
lesser.m1 <- lm(normedR_indv~ uncuedAR * sameDirection1S0D_R2, bgmndata.c)
summary(lesser.m1)
anova(lesser.m1)

#check only RANDOM data
bgmndata.r <- bgmndata[bgmndata$randomTrialsR1C0 == 1, ]
lesser.m1 <- lm(normedR_indv~ cuedAR *uncuedAR, bgmndata.r)
summary(lesser.m1)
anova(lesser.m1)

# let's change the order of same-diff-random
?relevel
bgmndata$sameDirection1S0D_R2 <-as.factor(bgmndata$sameDirection1S0D_R2)
# find the reference level of this variable
levels(bgmndata$sameDirection1S0D_R2)[1]
# change it to 2
bgmndata$sameDirection1S0D_R2 <- relevel(bgmndata$sameDirection1S0D_R2, "2")
levels(bgmndata$sameDirection1S0D_R2)[1]

lesser.m1 <- lm(normedR_indv~ cuedAR * uncuedAR * sameDirection1S0D_R2, bgmndata)
summary(lesser.m1)
anova(lesser.m1)
