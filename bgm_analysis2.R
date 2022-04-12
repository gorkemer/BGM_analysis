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
bgmndata.Same <- subset(bgmndata, bgmndata$pairSameARCat==1)
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

# let's change the order of same-diff-random
lesser.m1 <- lm(normedR_indv~ cuedAR *uncuedAR * as.factor(sameDirection1S0D_R2), bgmndata[(bgmndata$sameDirection1S0D_R2==1) | (bgmndata$sameDirection1S0D_R2==2)] )
summary(lesser.m1)
anova(lesser.m1)

# when I subset the data by coherence













# finding 2SD +- from the mean of the deviation #
mean_popResp <- mean(indResp_same$popResp)
sd_popResp <- sd(indResp_same$popResp)
indResp_same_outliers <- subset(indResp_same, indResp_same$popInvDiff <= mean_popResp - (2*sd_popResp) | indResp_same$popInvDiff >= mean_popResp + (2*sd_popResp))
nrow(indResp_same) #2119 row in total
nrow(indResp_same_outliers) # and 278 of the trial's responses are very different from the population response
unique(indResp_same_outliers$id) #almost everyone have some responseAR that deviates from the population response by a significant margin. 
# it might not be wise to go with the same-all approach. 
write.csv(indResp_same, "indResp_same_table.csv", row.names = FALSE)

#### 3- for each participant, for each cuedAR, how many same trials are there? ####

data.Same_arranged <- arrange(data.Same, id, cuedAR)
#table( data.Same_arranged[ , c("Color" , "Type") ] )

selectData <- data.Same_arranged[c("cuedAR", "uncuedAR", "pairAR_sameCondition", "id" )]
# find how many times they were faced with each cuedAR-uncuedAR identical pairs for each occurence
selectData.freq <- as.data.frame(table(selectData))
selectData.freq$sameCheck <- ifelse(selectData.freq$cuedAR == selectData.freq$uncuedAR, "same", "different")
selectData.freq <- subset(selectData.freq, selectData.freq$sameCheck=="same")
write.csv(selectData.freq, "selectData.freq.csv", row.names = FALSE)
# I am thinking about sharing this table in the supplementary section

test <- aggregate(Freq~id, data = selectData.freq, sum ) #this shows the same-trial number per participant. It varies, naturally.
write.csv(test, "sameTrialsPerID.csv", row.names = FALSE)
# we can even use this number per id, and cut at some point, and use population response for all of the responses of that particular participant. 
test2<- aggregate(pairAR_sameCondition~id, data = data.Same, sum) #these tables should be identical, and it is identical. Above method
#works. Now I will see if some AR are represented less than others. 
test3 <- aggregate(Freq~id + cuedAR, data = selectData.freq, sum ) #this shows the same-trial number per participant for each cuedAR occurences. It varies, naturally.
## test3 shows that the occurences, and it is not fairly distributed.
write.csv(test3, "sameTrialsPerID_andFreq.csv", row.names = FALSE)
## more importantly, I will identify Cued-UncuedAR pairs where participants lack an appropriate number of identical-pairs to be used as a matching value 
## (used for conducting the normalization process). For those trials, population response will be used for normalization operation. 

#### 8.a. - Population normalization approaches ####
#now with the "population responses" scores, calculate the normed response 
fullARList <- c(-0.46332, -0.41699,-0.37066, -0.32432, -0.27799, -0.23166, -0.18533, -0.13900, -0.09266, -0.04633,  0.00000,  0.04633,  0.09266,  0.13899,  0.18533,  0.23166,  0.27799, 0.32432,  0.37065,  0.41699,  0.46332)                                      

#get different trials
data.Diff_popResp <- subset(data, !(data$pairAR_sameCondition==1)) 

# aggregate to find the averaged response to CuedAR by id 
data_agg_same_popResp <- aggregate(responseAR ~ cuedAR, data = data.Same, mean) # this is the normalization with all participants (no-mixed approach, just all)
data_agg_same_indvResp <- aggregate(responseAR ~ cuedAR + id, data = data.Same, mean) # this is the normalization with all participants (no-mixed approach, just all)

#find the rows where cuedAR is -0.46332
totalData_popResp <- data[c(), ]
data.Diff_popResp$matchResponseAR_popResp <- NA
#go through the numbers and index them to add norm_response to those rows
for (i in 1:length(unique(data.Diff_popResp$id))){ #
  
  baseID <- unique(data.Diff_popResp$id)[i] #get the id to be worked on
  print(baseID)
  
  mainData_popResp <- subset(data.Diff_popResp, data.Diff_popResp$id ==baseID) #subset by id, do the analysis on the id-subsetted data frame only. 
  #sameResponses <- subset(selectData.freq_agg, selectData.freq_agg$id== baseID) 
  
  for (z in 1:length(fullARList)){ #go through each cuedAR for the selected baseID
    baseCuedAR <- fullARList[z]
    print(baseCuedAR)
    rowNumbers <- which(mainData_popResp$cuedAR==baseCuedAR)    #find the row numbers with that cuedAR
    print(rowNumbers)
    for (a in 1:length(rowNumbers)){ #go through each row number in the loop
      #use row numbers to find the row number with the normResponse to be included.
      mainData_popResp$matchResponseAR_popResp[rowNumbers[a]] <- data_agg_same_popResp$responseAR[which(data_agg_same_popResp$cuedAR==baseCuedAR)]#include normResponseAR_mix to those trials
    }
  }
  #totalData <- cbind(totalData, mainData)
  totalData_popResp <- rbind(totalData_popResp, mainData_popResp) #add id-subsetted data frame that was being worked on above lines to the "totalData"
}

#remove the first row, because it was generated when totalData matrix was initially created.
totalData_popResp = totalData_popResp[-1,]

### different trial datasini inceliyorsun az once uretilen totalData_popResp ile ### 

### running linear analysis ###
#first I will calculate the deviation from the response to the matched.
totalData_popResp$normedResp <- totalData_popResp$responseAR - totalData_popResp$matchResponseAR_popResp

lm_motion <- lm(normedResp~ uncuedAR * globalOrg, data = totalData_popResp)
summary(lm_motion)

lesser.m1 <- lm(normedResp~cuedAR, totalData_popResp)
main.m1 <- lm(normedResp ~ uncuedAR * motion_L + cuedAR, totalData_popResp)
main.m2 <- lm(normedResp ~ uncuedAR * motion_L * globalOrg + cuedAR, totalData_popResp)
summary(main.m1)
anova(main.m1)


#### 4- new analysis with the mixed approach ####
#can move this to a separate script

#now with the new matching scores, calculate the normed response 
data.Diff <- subset(data, !(data$pairAR_sameCondition==1)) #get the whole data set for different trials

# aggregate to find the averaged response to CuedAR by id 
selectData.freq_agg <- aggregate(respToMatch_mixedApp ~ cuedAR + id, selectData.freq, mean)
#data_agg_same <- aggregate(responseAR ~ cuedAR + id, data = data.Same, mean) # this is the normalization with all participants (no-mixed approach, just all)

#selectData.freq_agg.singleP <- subset(selectData.freq_agg, selectData.freq_agg$id=="o1" | selectData.freq_agg$id == "o2")
#data.Diff.singleP <- subset(data.Diff, data.Diff$id=="o1" | data.Diff$id=="o2")

#find the rows where cuedAR is -0.46332
data.Diff$matchResponseAR_mix <- NA
totalData <-matrix(nrow=10,ncol=34)
totalData <- data.frame(subNum=NA, trial_number =NA, cuedAR = NA,uncuedAR = NA, responseAR = NA,
                        response_error =NA, rt=NA, shape_org =NA, cueType = NA,e1_motion_dir = NA,
                        e2_motion_dir =NA, round_number=NA, coherence =NA, cued_motion_dir = NA,uncued_motion_dir = NA,
                        arDiff =NA, pairAvg=NA, cuedMotDir =NA, pairGlobalOrg = NA,pairMotDirSameness = NA,
                        pairMotDirSameness_wRandom =NA, pairMotSame=NA, pairMotDiff =NA, pairMotRandom = NA,pairSameARCat = NA,
                        pairAR_sameCondition =NA, motDirComp=NA, pairMotCoherent =NA, pairMotDiago = NA,id = NA,
                        corrCoeff =NA, corrPval=NA, slopeCoeff =NA, slopePval = NA, matchResponseAR_mix = NA)

#go through the numbers and index them to add norm_response to those rows
for (i in 1:length(unique(data.Diff$id))){ #
  
  baseID <- unique(data.Diff$id)[i] #get the id to be worked on
  print(baseID)
  
  mainData <- subset(data.Diff, data.Diff$id ==baseID) #subset by id, do the analysis on the id-subsetted data frame only. 
  sameResponses <- subset(selectData.freq_agg, selectData.freq_agg$id== baseID) #define the data sheet with the mixed approach same-responses
  
  for (z in 1:length(fullARList)){ #go through each cuedAR for the selected baseID
    baseCuedAR <- fullARList[z]
    print(baseCuedAR)
    rowNumbers <- which(mainData$cuedAR==baseCuedAR)    #find the row numbers with that cuedAR
    print(rowNumbers)
    for (a in 1:length(rowNumbers)){ #go through each row number in the loop
      #use row numbers to find the row number with the normResponse to be included.
      mainData$matchResponseAR_mix[rowNumbers[a]] <- sameResponses$respToMatch_mixedApp[which(sameResponses$cuedAR==baseCuedAR)]#include normResponseAR_mix to those trials
    }
  }
  #totalData <- cbind(totalData, mainData)
  totalData <- rbind(totalData, mainData) #add id-subsetted data frame that was being worked on above lines to the "totalData"
}

#remove the first row, because it was generated when totalData matrix was initially created.
totalData = totalData[-1,]

#test
data.Diff.test <- totalData[,c("id","cuedAR", "uncuedAR", "responseAR", "matchResponseAR_mix")]
#okay this method works. tested with 2 participants it also worked. Okay, test for all people: normed with mixed approach is applied to the main data successfully.

#### 5- running linear analysis ####
#first I will calculate the deviation from the response to the matched.
totalData$normedResponseUsingMixApp <- totalData$responseAR - totalData$matchResponseAR_mix


