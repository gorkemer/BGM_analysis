# April 5th, 2022.
# Analysis script for the foreground motion experiment - CLEANING DATA PART. 


# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table")

require(x)
lapply(x, require, character.only = TRUE)

# set wd if needed setwd('~/Desktop/background-motion/data'), check with getwd()
setwd('/Users/gorkem.er/Desktop/background-motion/Data_analysis')
d = read.csv("fgmdata_Ready.csv",header=TRUE, quote="\"") 


# sort the data by observer. Necessary because the next step assigns a new id when subNum changes.
data <- data[order(data$subNum),]


### Add subject IDs starting at "1" ###
# e.g., subIDs will be o1, o2, etc.

# remove na in r - remove rows - na.omit function / option
data <- na.omit(data)

sub = data$subNum[1]
number = 1

for (i in 1:nrow(data)) {
  if (data$subNum[i] != sub) {
    number = number+1
    sub = data$subNum[i]
  }
  data$id[i] = paste("o",number, sep = "")
}

singCorrs= array(data = NA,dim=c(length(unique(data$id)),3)) #empty 74x3 array
nS = length(unique(data$id)) #n iterations for the loop below. Should be 74 unique ids

for (s in 1:nS) {
  id = s
  name = paste("o",id,sep = "") 
  sing.Data <- subset(data, data$id==name) #pull up data from one observer. 
  res <- cor.test(sing.Data$cuedAR, sing.Data$responseAR, method = c("pearson")) #run correl on that person's data
  singCorrs[s,1] = name #add correlation results to array
  singCorrs[s,2] = res$estimate
  singCorrs[s,3] = res$p.value
}

# plot correlation coefficients and p-values
plot(singCorrs[,2], singCorrs[,3], main="Correlation values: Individuals",
     xlab="Corr Coeff", ylab="P-value", pch=19)

### Add individual correlation results to main data ###
# removed, see prepareData_01 for it

#### Data Clean with LM ####

# find the conditions where pairs had the identical AR 
singSlopes= array(data = NA,dim=c(length(unique(data$id)),3)) #empty 74x3 array
nS = length(unique(data$id)) #n iterations for the loop below. Should be 74 unique ids
data_onlyEqualPairs <- subset(data, data$pairAR_sameCondition==1)

#then look for regression btw response AR (DV) and cued AR (IV) for each id
for (s in 1:nS) {
  id = s
  name = paste("o",id,sep = "") 
  sing.Data_slopes <- subset(data_onlyEqualPairs, data_onlyEqualPairs$id==name) #pull up data from one observer. 
  res <- lm(sing.Data_slopes$responseAR ~ sing.Data_slopes$cuedAR, data = sing.Data_slopes) #run correl on that person's data
  singSlopes[s,1] = name #add correlation results to array
  singSlopes[s,2] = res$coefficients[2]              #adding the beta coefficient (the slope) of the regression
  singSlopes[s,3] = summary(res)$coefficients[,4][2] #adding the p-values of the regression coefficients
}

# plot slope coefficients and p-values
plot(singSlopes[,2], singSlopes[,3], main="Slope Coefficient values: Individuals",
     xlab="Slope Coeff", ylab="P-value", pch=19)

# Add individual slopes results to main data
for (i in 1:nrow(data)) {
  #find the row in correlation array with current number is subject
  id = data$id[i]
  r = which(singSlopes == id)
  data$slopeCoeff[i] <- singSlopes[r,2]
  data$slopePval[i] <- singSlopes[r,3]
}
subset(data, data$id=="o63") #check one person's data against correlation array

# New data set containing only Participants with significant reg slop btw response & cuedAR
data.Clean_lmP05 <- subset(data, data$slopePval< 0.05)

cleanScatter <- ggplot(data.Clean_lmP05, aes(x = cuedAR, y = responseAR)) + 
  geom_point(shape=19, size=1, alpha=0.1, show.legend = FALSE, aes(color= pairMotDirSameness) ) +
  geom_smooth(data = data.Clean_lmP05, method=lm, color = "red") +
  ylim(-.7,.7) +
  labs(x="Cued aspect ratio", y = "Reported aspect ratio") +
  ggtitle("Cleaned data for regression slope method")
cleanScatter + geom_density_2d(color="blue", alpha=0.2) + xlab((cleanScatter$mapping$x)) + ylab(cleanScatter$mapping$y)

#### asc b-bag ### 
cleanScatter <- ggplot(data.Clean_lmP05, aes(x = cuedAR, y = responseAR)) + 
  #geom_point(shape=19, size=1, alpha=0.1, show.legend = FALSE, aes(color= pairMotDirSameness) ) +
  geom_jitter(alpha=0.2, size=0.5)+
  geom_smooth(data = data.Clean_lmP05, method=lm, color = "red") +
  ylim(-.7,.7) +
  labs(x="Cued aspect ratio", y = "Reported aspect ratio") +
  ggtitle("Results of 78 participants")
cleanScatter + theme_classic() + facet_wrap(~id,nrow = 6) ## + geom_density_2d(color="blue", alpha=0.2) + xlab((cleanScatter$mapping$x)) + ylab(cleanScatter$mapping$y)

tiff("test.png", units="in", width=8, height=5, res=300)
# insert ggplot code
cleanScatter + theme_classic() #  + facet_wrap(~id,nrow = 6)
dev.off()

# 1- main analysis
lm_main <- lm(responseAR~ cuedAR, data = data)
summary(lm_main) #overall attractive influence of uncuedAR, but when shapes are localized into the same hemifield, uncuedAR played a repulsive role. 
# also, global organization influenced the responses (same as reported at Sweeny2011) 



### end of the organization of the data script ### 

### revealing the number of rounds each participant did the experiment ### 
roundNumber <- data %>% count(id, trial_number)
roundNumber_agg <- aggregate(n~id,data=roundNumber, mean)

#people with less than 3 rounds: 
peopleLess3Rounds <- subset(roundNumber_agg, roundNumber_agg$n<3)
#people with less than 3 rounds: 
peopleLess3Rounds$id
write.csv(roundNumber_agg, "roundNumber_agg.csv", row.names = FALSE)
#remove those people from the data.Clean_lmP05
data.Clean_lmP05_Round3Only<- data.Clean_lmP05[-c(which(data.Clean_lmP05$id== "o102"), 
                                                  which(data.Clean_lmP05$id=="o15"),
                                                  which(data.Clean_lmP05$id=="o43"),
                                                  which(data.Clean_lmP05$id=="o49"),
                                                  which(data.Clean_lmP05$id=="o51"),
                                                  which(data.Clean_lmP05$id=="o82"),
                                                  which(data.Clean_lmP05$id=="o86"),
                                                  which(data.Clean_lmP05$id=="o93"),
                                                  which(data.Clean_lmP05$id=="o97")),]
## resulting data has 74 participants
length(unique(data.Clean_lmP05_Round3Only$id)) #o86 already got eliminated. so 8 people eliminated by round number

# output the csv externally 
write.csv(data.Clean_lmP05, "data.Clean_lmP05.csv", row.names = FALSE)
write.csv(data.Clean_lmP05_Round3Only, "data.Clean_lmP05_Round3Only.csv", row.names = FALSE)
#write.csv(data.Clean_p05, "data.Clean_p05.csv", row.names = FALSE)
write.csv(data, "untouchedData.csv", row.names = FALSE)
#write.csv(data.Clean,"bgm_cleanData_cor.csv", row.names = FALSE)
#write.csv(data.Clean_lm,"bgm_cleanData_lm.csv", row.names = FALSE)