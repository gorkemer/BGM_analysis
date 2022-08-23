# April 6th, 2022.
# Analysis script for the background motion experiment. 
# last update: 16 Agu 22

# clear the workspace
rm(list=ls()) 
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker")
require(x)
lapply(x, require, character.only = TRUE)
source("~/Documents/GitHub/ger_R_functions/plot_functions.R")
#remove scientific notation in the entire R session
options(scipen = 100)
# locate the data and import it
setwd("/Users/gorkem.er/Desktop/21Projects/background-motion/Data_analysis")
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
table(fgmdata$sameDirection1S0D_R2)
fgmdata$responseAR_normed <- bgmndata.c$normedR_indv
# fgmdata$responseAR_normed <- bgmndata.r$normedR_indv # FOR RANDOM ANALYSES
fgmdata$response_error_normed <- bgmndata$normedR_indv - bgmndata$cuedAR
#fgmdata$response_error <- fgmdata$response_error_normed
#fgmdata <- fgmdata[fgmdata$randomTrialsR1C0==1,] ### ATTENTION! HERE TOGGLE ON/OFF random trials! 
# new meta variables
fgmdata$cuedAR <- round(fgmdata$cuedAR, digits = 2)
table(fgmdata$cuedAR)
fgmdata$uncuedAR <- round(fgmdata$uncuedAR, digits = 2)
fgmdata$responseAR <- round(fgmdata$responseAR, digits = 2)
table(fgmdata$responseAR)
fgmdata$uncuedCat = ifelse(fgmdata$uncuedAR < 0, -1, ifelse(fgmdata$uncuedAR==-0, 0, 1))
fgmdata$uncuedCat <- as.factor(fgmdata$uncuedCat)
fgmdata$cuedCat = ifelse(fgmdata$cuedAR < 0, -1, ifelse(fgmdata$cuedAR==-0, 0, 1))
table(fgmdata$uncuedCat)
table(fgmdata$cuedCat)
fgmdata$respAcc <- ifelse( (fgmdata$responseAR > 0 & fgmdata$cuedCat == 1)  | (fgmdata$responseAR < 0 & fgmdata$cuedCat == -1) | (fgmdata$responseAR == 0 & fgmdata$cuedCat == 0), 1, 0)
table(fgmdata$respAcc)
fgmdata$globalMotion <- as.factor(ifelse(fgmdata$cued_motion_dir == 90 | fgmdata$cued_motion_dir == 270, 1, -1))
# iterate all and get the beta coefficient
fgmdata.cleaning <- data.frame(matrix(ncol = 8, nrow = length(number_of_sub)))
colnames(fgmdata.cleaning) <- c("reg_p", "reg_beta", "meanRT", "corr","corr_p", "respAcc","trialN","id")
for (s in 1:length(number_of_sub)){
  tmpdata <- fgmdata[fgmdata$sub == number_of_sub[s],]
  lm_sub <- lm(formula = responseAR ~ cuedAR, data = tmpdata)
  fgmdata.cleaning[s,1] <- round(summary(lm_sub)$coefficients[2,4], digits = 3) #p-value
  fgmdata.cleaning[s,2] <- round(summary(lm_sub)$coefficients[2,1], digits= 3) #estimate
  fgmdata.cleaning[s,3] <- round(mean(tmpdata$rt), digits = 0)/1000
  fgmdata.cleaning[s,4] <- cor.test(tmpdata$cuedAR, tmpdata$responseAR, method = "pearson")$estimate
  fgmdata.cleaning[s,5] <- round(cor.test(tmpdata$cuedAR, tmpdata$responseAR, method = "pearson")$p.value, digits = 2)
  fgmdata.cleaning[s,6] <- mean(tmpdata$respAcc)
  fgmdata.cleaning[s,7] <- nrow(tmpdata)# check the n of each participant
  fgmdata.cleaning[s,8] <- number_of_sub[s]
}
head(fgmdata.cleaning)
plot(fgmdata.cleaning$reg_beta, fgmdata.cleaning$reg_p)
plot(fgmdata.cleaning$meanRT)
plot(fgmdata.cleaning$corr, fgmdata.cleaning$respAcc) # correlation and accuracy appears to be linked
below40P<-fgmdata.cleaning$id[fgmdata.cleaning$respAcc <= 0.40] # 3/74 showed below 0.40 accuracy
below40P
below50P<-fgmdata.cleaning$id[fgmdata.cleaning$respAcc <= 0.50] # 3/74 showed below 0.40 accuracy
below50P
testToBeCleaned = below50P[12]
fgmdata.cleaning[fgmdata.cleaning$id == testToBeCleaned,]
plot(fgmdata$cuedAR[fgmdata$sub == testToBeCleaned], fgmdata$responseAR[fgmdata$sub == testToBeCleaned])
# those below40P all show bad performance in all metrics (e.g. regression beta, p-value, correlation, reaction time). 
incompletedPeople <- fgmdata.cleaning$id[fgmdata.cleaning$trialN<241]
incompletedPeople
# for random analyses, I need to remove those people using the coherent analysis
# 21 agu, I found out that 19117 19142 19242 are incomplete
# incompletedPeople <- c(19117, 19142, 19242) # WARNING THIS IS FOR RANDOM ANALYSIS ONLY
#### CLEANED DATA ####
#fgmdata <- fgmdata[!( (fgmdata$sub %in% below50P)),]
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople)),]
# check, also remove id 18667
18667 %in% unique(fgmdata$sub) 
fgmdata <- fgmdata[!(fgmdata$sub == 18667),] # because 0.53 corr
incompletedPeople[1:length(incompletedPeople)] %in% unique(fgmdata$sub) 
below50P[1:length(below50P)] %in% unique(fgmdata$sub) # many of the below50 people are included in the analysis
# clear relationship between CUED vs RESPONSE
cols = rgb(red = 1, green = 0, blue = 0)
plot(fgmdata$responseAR, fgmdata$cuedAR, main= "Response AR as a function of cued AR (paired with same)", 
     ylab = "Response AR", xlab = "Cued AR")
# cleaning based on regression line
res <- cor.test(fgmdata$cuedAR, fgmdata$responseAR, 
                method = "pearson")
res # overall 0.66 correlation
# calculate aspect-ratio-repulsion-index
aggregate(responseAR~cuedAR + identicalShapesI1D0 + sub, fgmdata, mean)
findSameAR_trials <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("cuedAR", "identicalShapesI1D0", "sub"))
sameAR_trials <- findSameAR_trials[findSameAR_trials$identicalShapesI1D0==1,]
sameAR_trials #so many less-same trials per individuals
par(mfrow = c(2,2))
plot(findSameAR_trials$cuedAR[findSameAR_trials$identicalShapesI1D0==1], findSameAR_trials$responseAR[findSameAR_trials$identicalShapesI1D0==1], xlab = "Cued AR", ylab = "Response AR", main = "Same-AR Trials")
abline(coef = c(0,1),col="red", lwd=3, lty=2)
plot(findSameAR_trials$cuedAR[findSameAR_trials$identicalShapesI1D0==0], findSameAR_trials$responseAR[findSameAR_trials$identicalShapesI1D0==0], xlab = "Cued AR", ylab = "Response AR",main = "Different-AR Trials", ylim = c(-0.6, 0.6))
abline(coef = c(0,1),col="red", lwd=3, lty=2)
#uncuedAR
findSameAR_trials_uncued <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("uncuedAR", "identicalShapesI1D0", "sub"))
plot(findSameAR_trials_uncued$uncuedAR[findSameAR_trials_uncued$identicalShapesI1D0==1], findSameAR_trials_uncued$responseAR[findSameAR_trials_uncued$identicalShapesI1D0==1], xlab = "Uncued AR", ylab = "Response AR", main = "Same-AR Trials")
abline(coef = c(0,1),col="red", lwd=3, lty=2)
plot(findSameAR_trials_uncued$uncuedAR[findSameAR_trials_uncued$identicalShapesI1D0==0], findSameAR_trials_uncued$responseAR[findSameAR_trials_uncued$identicalShapesI1D0==0], xlab = "Uncued AR", ylab = "Response AR", main = "Different-AR Trials", ylim = c(-0.6, 0.6))
abline(coef = c(0,1),col="red", lwd=3, lty=2)
abline(coef = c(0,0),col="red", lwd=3, lty=2)
par(mfrow = c(1,1))
#response error by uncued Cat
tmpdata <- summarySE(fgmdata, measurevar="responseAR", groupvars=c("uncuedCat", "sub", "sameDirection1S0D"))
head(tmpdata)
plot(tmpdata$uncuedCat, tmpdata$responseAR)
tmpdata$sub <- as.factor(tmpdata$sub)
ggplot(tmpdata[tmpdata$uncuedCat==-1 | tmpdata$uncuedCat == 1,], aes(uncuedCat, responseAR, color = sub, group = sub)) + geom_point(aes(color = sub),shape=15, size=1.5, alpha = 30/60) +
  geom_line() + facet_grid(~sameDirection1S0D) # not really legible
#get the difference from same to notsame
head(tmpdata)
tmpdata2 <- aggregate(responseAR ~ uncuedCat + sameDirection1S0D + sub, data = fgmdata, mean)
head(tmpdata2)
require(dplyr)
tmpdata3 <- tmpdata2 %>%
  pivot_wider(
    names_from = c(sameDirection1S0D, uncuedCat),
    values_from = c(responseAR)
  )
colnames(tmpdata3) <- c("id", "NS_uncued_flat", "NS_uncued_circle", "NS_uncued_tall","S_uncued_flat","S_uncued_circle", "S_uncued_tall")
head(tmpdata3)
#find the difference in flat and tall (diff btw SAME - NS)
tmpdata3$diff_tall <- tmpdata3$S_uncued_tall - tmpdata3$NS_uncued_tall
tmpdata3$diff_flat <- tmpdata3$S_uncued_flat - tmpdata3$NS_uncued_flat
tmpdata3$diff_circle <- tmpdata3$S_uncued_circle - tmpdata3$NS_uncued_circle
head(tmpdata3)
par(mfrow = c(1,3)) # plotting diff_tall and diff_flat and diff_circle
# order the level of sub for x-axis
tmpdata3 <- tmpdata3[order(tmpdata3$diff_flat),]
plot(tmpdata3$diff_flat, ylim = c(-0.4, 0.4), xlab = "sub", ylab = "Grouping Effect (diff) when uncued is FLAT")
abline(c(0,0), lty = 2)
tmpdata3 <- tmpdata3[order(tmpdata3$diff_circle),]
plot(tmpdata3$diff_circle, ylim = c(-0.4, 0.4), xlab = "sub", ylab = "Grouping Effect (diff) when uncued is CIRCLE")
abline(c(0,0), lty = 2)
tmpdata3 <- tmpdata3[order(tmpdata3$diff_tall),]
plot(tmpdata3$diff_tall, ylim = c(-0.4, 0.4), xlab = "sub", ylab = "Grouping Effect (diff) when uncued is TALL")
abline(c(0,0), lty = 2)
# Mean +/- standard deviation
tmpdata <- aggregate(response_error~uncuedCat + sub, fgmdata, mean)
ggerrorplot(tmpdata, x = "uncuedCat", y = "response_error", 
            desc_stat = "mean_sd")
ggerrorplot(tmpdata, x = "uncuedCat", y = "response_error", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))
# Specify the comparisons you want
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggerrorplot(tmpdata, x = "uncuedCat", y = "response_error", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value
# DOING IT WITH PAIRED = TRUE
my_comparisons <- list( c("-1", "1"), c("0", "1"), c("-1", "0") )
ggerrorplot(tmpdata, x = "uncuedCat", y = "response_error", 
            desc_stat = "mean_sd", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 1)                  # Add global p-value
# Color by grouping variable "sameDirection1S0D"
tmpdata <- aggregate(response_error~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
ggerrorplot(tmpdata, x = "uncuedCat", y = "response_error", 
            desc_stat = "mean_sd", 
            color = "sameDirection1S0D", palette = "jco",
            position = position_dodge(0.3))
# Basic line plots of means +/- se with jittered points
ggline(tmpdata, x = "uncuedCat", y = "response_error", color = "sameDirection1S0D",
       add = c("mean_se", "jitter")) # INTERESTING! I see a difference especially at tall and circle, not at flat (similar to Tim's finding)
ggbarplot(tmpdata, x = "uncuedCat", y = "response_error", 
          add = c("mean_se", "jitter"),
          color = "sameDirection1S0D", palette = "jco",
          position = position_dodge(0.8)) # ADD THIS
ggline(tmpdata, x = "uncuedCat", y = "response_error",
       add = c("mean_se", "jitter"),
       color = "sameDirection1S0D", palette = "jco")
# flipping x-axis and grouping
ggbarplot(tmpdata, x = "sameDirection1S0D", y = "response_error", 
          add = c("mean_se", "jitter"),
          color = "uncuedCat", palette = "jco",
          position = position_dodge(0.8))
ggline(tmpdata, x = "sameDirection1S0D", y = "response_error", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")
# compare tall and circle
my_comparisons = list(c("0","1"))
ggline(tmpdata, x = "sameDirection1S0D", y = "response_error",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.5)+ # ADD THIS, works for both TALL and Circle, tiny effect though!
  facet_grid(~uncuedCat)
# checking anova results
ANOVA <- aov(response_error ~ sameDirection1S0D*uncuedCat + Error(as.factor(sub)/(sameDirection1S0D*uncuedCat)), data=tmpdata)
summary(ANOVA) # YES! YES! YES! Pretty neat interaction (no effect of uncued cat but only interacts if motion is the same), #57 people
# let's confirm this with lmer
tmpdata$sub <- as.factor(tmpdata$sub)
lmm <- lmer(response_error ~ sameDirection1S0D * uncuedCat + ( uncuedCat | sub), fgmdata)
summary(lmm) # categorical uncued Cat koymama gerek yok burda, continuous girebilirim, giriyorum altta.
# uncuedAR
lmm <- lmer(response_error ~ sameDirection1S0D * uncuedAR + ( sameDirection1S0D*uncuedAR | sub), fgmdata)
summary(lmm) # now I see a clear interaction, even when controlling for random effects of uncuedAR for each participant, around 2.4 percent change
# I also like this graph
my_comparisons = list(c("0","1"))
ggbarplot(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error", 
          add = c("mean_se", "jitter"),
          color = "sameDirection1S0D", palette = "jco",
          position = position_dodge(0.8))+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)+
  facet_grid(~uncuedCat) # ADD this
# doing the same with fgm data
table(fgmdata$sameDirection1S0D, fgmdata$uncuedCat)
ggline(fgmdata, x = "sameDirection1S0D", y = "response_error",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)+ # tall is significant! circle is marginal
  facet_grid(~uncuedCat) # facet_grid(~uncuedCat+global_org) global org eklenince, between'de daha cok averaging var.
# ttest
t.test(tmpdata$response_error[tmpdata$uncuedCat==1], mu = 0, alternative = "greater") #“two.sided” (default), “greater” or “less”.
# tall -> response error is greater than 0
#Homogeneity of variance
var(tmpdata$response_error[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==1])
var(tmpdata$response_error[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==0]) # equal?
t.test(tmpdata[tmpdata$sameDirection1S0D==1,"response_error"], tmpdata[tmpdata$sameDirection1S0D==0,"response_error"], paired = TRUE) 
# response errors are significantly different between group vs ungroup
ggline(tmpdata, x = "sameDirection1S0D", y = "response_error",
       add = c("mean_se", "jitter"))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)
# okay this means that having same direction introduces biases (more errors on responses)
# now grouping test, FLAT
t.test(tmpdata[tmpdata$sameDirection1S0D==0 & tmpdata$uncuedCat==-1,"response_error"], 
       tmpdata[tmpdata$sameDirection1S0D==1 & tmpdata$uncuedCat==-1,"response_error"], 
       paired = TRUE)  # flat not different
t.test(tmpdata[tmpdata$sameDirection1S0D==0 & tmpdata$uncuedCat==1,"response_error"], 
       tmpdata[tmpdata$sameDirection1S0D==1 & tmpdata$uncuedCat==1,"response_error"], 
       paired = TRUE)  # tall not different significantly, but there is a trend
# hmm this gives out 0.12, tall-group (-0.002) != tall-ungroup (0.01) (p=0.12)
tmpdata <- aggregate(responseAR~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
t.test(tmpdata$response_error[tmpdata$uncuedCat==0 & tmpdata$sameDirection1S0D==1], 
       tmpdata$response_error[tmpdata$uncuedCat==0 & tmpdata$sameDirection1S0D==0], paired = TRUE) 
# circle is significant, 0.03, marginal, significant with cleaner data
# testing on RESPONSE AR
t.test(tmpdata[tmpdata$sameDirection1S0D==0 & tmpdata$uncuedCat==1,"responseAR"], 
       tmpdata[tmpdata$sameDirection1S0D==1 & tmpdata$uncuedCat==1,"responseAR"], 
       paired = TRUE) # responses got taller with uncued tall and grouping (0.07)
# these t-tests are not great because we lose the magnitude of uncued cat (tall-so-tall etc)
# GLOBAL ORG, this has to have a role in the integration - 
fgmdata$global_org <- fgmdata$global_org1W0B
tmpdata <- aggregate(response_error~uncuedCat + sub + sameDirection1S0D + global_org, fgmdata, mean)
tmpdata$sameDirection1S0D<- factor(tmpdata$sameDirection1S0D, levels = c(0:1), labels= c("Non-grouped", "Grouped"))
tmpdata$global_org<- factor(tmpdata$global_org, levels = c(0:1), labels= c("between", "within"))
tmpdata$sub <- as.factor(tmpdata$sub)
tmpdata$uncuedCat <- factor(tmpdata$uncuedCat, levels = c(-1,0,1), labels= c("Flat", "Circle", "Tall"))
tmpdata$response_error <- round(tmpdata$response_error, digits =3)
ggline(tmpdata, x = "sameDirection1S0D", y = "response_error", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+facet_grid(~global_org) # between has more noise though
# stats
my_comparisons <- list(c("Non-grouped", "Grouped"))
table(tmpdata$sameDirection1S0D,tmpdata$uncuedCat)
ggline(tmpdata, x = "sameDirection1S0D", y = "response_error", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+facet_grid(~uncuedCat+global_org)+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 1) # nice! nice! nice! more averaging in between (interesting)
ANOVA <- aov(response_error ~ sameDirection1S0D*global_org*uncuedCat + Error(sub/(sameDirection1S0D*global_org)), data=tmpdata)
summary(ANOVA) # not sure but there might not be an interaction
lmm <- lmer(response_error ~ 1  + sameDirection1S0D*uncuedCat*global_org + (sameDirection1S0D|sub), tmpdata)
summary(lmm)
anova(lmm)# Okay, I guess there is no interaction with global organization :/ 
# analysis on global org # ADDED TO MANUSCRIPT!
# 1) with all interactions
summary(lmer(response_error ~ uncuedAR * sameDirection1S0D * global_org  + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR) + (1 | sub:global_org), data = fgmdata,  REML = FALSE))
# 1) relevant interactions
summary(lmer(response_error ~ uncuedAR + uncuedAR:sameDirection1S0D * global_org  + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR) + (1 | sub:global_org), data = fgmdata,  REML = FALSE))
# analysis on global org # end of ADDED TO MANUSCRIPT!
# checking anova results, on fgmdata
#lmm <- lmer(response_error ~ 1  + sameDirection1S0D*global_org*uncuedAR + (global_org*sameDirection1S0D|sub), fgmdata)
#summary(lmm) # again I confirm this: 1) sig interaction of uncuedAR and grouping, 2) main effect of global org
#lmm <- lmer(response_error ~ 1  + sameDirection1S0D*global_org*uncuedAR + (global_org*sameDirection1S0D*uncuedAR|sub), fgmdata)
#summary(lmm) # added uncuedAR to the random effects
# continuous variable
m <- lmer(response_error ~ as.factor(sameDirection1S0D)*uncuedAR + (1 + as.factor(sameDirection1S0D) | sub), data=fgmdata, REML=F)
summary(m) # wow I see a main effect of uncuedAR, repulsion when they are not grouped, ADD THIS
# global_org
m <- lmer(response_error ~ as.factor(sameDirection1S0D)*uncuedAR*as.factor(global_org) + (1 + as.factor(sameDirection1S0D)*as.factor(global_org) | sub), data=fgmdata, REML=F)
summary(m) # 1) main effect of global org and 2) attraction to uncued in the same motion condition
# again same findings! in fact uncued repulsion is almost there. 
# fgmdata - same results
ANOVA <- aov(response_error ~ sameDirection1S0D*uncuedCat + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) # again I see an interaction yeah? within section shows 0.06?
# coherence
ANOVA <- aov(response_error ~ sameDirection1S0D*uncuedCat*as.factor(fgmdata$coherence) + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) # coherence didnt do much
ANOVA <- aov(response_error ~ sameDirection1S0D*uncuedCat*global_org + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) # again main effect of global org, but no 3-way interaction
# global org with regression again
m <- lmer(response_error ~ as.factor(sameDirection1S0D)*as.factor(uncuedCat)*as.factor(global_org) + (1 + as.factor(sameDirection1S0D)*as.factor(global_org) | sub), data=fgmdata, REML=F)
summary(m) # again same results, tiny effect but significant (0.02) falan bir efekt
# CHANGE OF TOPIC - globalMotion!
tmpdata <- aggregate(response_error~uncuedCat + sub + sameDirection1S0D + global_org + globalMotion, fgmdata, mean)
tmpdata$sameDirection1S0D<- factor(tmpdata$sameDirection1S0D, levels = c(0:1), labels= c("Non-grouped", "Grouped"))
tmpdata$global_org<- factor(tmpdata$global_org, levels = c(0:1), labels= c("between", "within"))
tmpdata$uncuedCat <- factor(tmpdata$uncuedCat, levels = c(-1,0,1), labels= c("Flat", "Circle", "Tall"))
tmpdata$sub <- as.factor(tmpdata$sub)
my_comparisons <- list(c("-1", "1"))
ggline(tmpdata, x = "globalMotion", y = "response_error",
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons)+
  stat_compare_means(label.y = 1)+ 
  facet_grid(~sameDirection1S0D+global_org) # globalMotion might make this taller, yeah
# global motion anova
ANOVA <- aov(response_error ~ as.factor(sameDirection1S0D)*uncuedCat*as.factor(fgmdata$coherence)*as.factor(globalMotion) + Error(as.factor(sub)/(as.factor(sameDirection1S0D))), data=fgmdata)
summary(ANOVA)
#### END #### 
#### final ####
tmpdata <- aggregate(response_error~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
my_comparisons <- list(c("0", "1"))
ggbarplot(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error", 
          add = c("mean_ci", "jitter"),
          color = "sameDirection1S0D", palette = "jco",
          position = position_dodge(0.8))+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)+
  facet_grid(~uncuedCat)
ggline(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)+
  facet_grid(~uncuedCat)
t.test(tmpdata$response_error[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==1], 
       tmpdata$response_error[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==0], paired = TRUE, alternative = "greater") 
t.test(tmpdata$response_error[tmpdata$uncuedCat==-1 & tmpdata$sameDirection1S0D==1], 
       tmpdata$response_error[tmpdata$uncuedCat==-1 & tmpdata$sameDirection1S0D==0], paired = TRUE, var.equal = TRUE, alternative = "less") 
ggpaired(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error",
         color = "uncuedCat", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)+facet_grid(~uncuedCat)
#real g
realG <- ggpaired(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error",
                  color = "sameDirection1S0D", line.color = "gray", line.size = 0.4, position = position_dodge(0.5))+
  stat_compare_means(paired = TRUE, label.y = 0.15, comparisons = my_comparisons)+
  facet_grid(~uncuedCat)+
  scale_color_grey(start = 0.0, end = 0.5)+
  coord_cartesian(ylim = c(-0.15, 0.15))+
  stat_boxplot(notch = FALSE, outlier.shape=8)+
  geom_point(shape=16, position=position_jitter(0.01))+
  stat_summary(fun.y=mean, shape=25, size=0.4, col = "darkred", fill="red")
#stat_boxplot(width = 0.5, notch = FALSE, outlier.shape=8) # geom ='errorbar' #         stat_summary(fun.y=mean, geom="circle", shape=23, size=4)+
summary(realG)
realG
realG$layers <- realG$layers[-3]
realG
realG$layers <- realG$layers[-2]
realG
# 
ggline(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error", 
       add = c("mean_se", "jitter"),
       color = "uncuedCat", palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)
ggpairs(tmpdata)+theme_bw()
ggpairs(tmpdata[!(tmpdata$uncuedCat==0),], columns = 1:4, ggplot2::aes(colour=sameDirection1S0D))
compare_means(response_error ~ sameDirection1S0D, data = tmpdata[(tmpdata$uncuedCat==1),], paired = TRUE, alternative = "greater", method = "t.test")
compare_means(response_error ~ sameDirection1S0D, data = tmpdata[(tmpdata$uncuedCat==1),], paired = TRUE, alternative = "greater", method = "wilcox.test")
compare_means(response_error ~ sameDirection1S0D, data = tmpdata[(tmpdata$uncuedCat==-1),], paired = TRUE, alternative = "less")
compare_means(response_error ~ sameDirection1S0D, data = tmpdata, paired = TRUE, alternative = "greater", method = "kruskal.test")
compare_means(response_error ~ sameDirection1S0D, data = tmpdata, paired = TRUE, alternative = "greater", method = "anova")
# Subset weight data before treatment
ungrouped <- subset(tmpdata[(tmpdata$uncuedCat==1),],  sameDirection1S0D == "0", response_error,
                    drop = TRUE)
# subset weight data after treatment
grouped <- subset(tmpdata[(tmpdata$uncuedCat==1),],  sameDirection1S0D == "1", response_error,
                  drop = TRUE)
# Plot paired data
library(PairedData)
pd <- paired(ungrouped, grouped)
plot(pd, type = "profile") + theme_bw()
ungrouped <- subset(tmpdata[(tmpdata$uncuedCat==-1),],  sameDirection1S0D == "0", response_error,
                    drop = TRUE)
# subset weight data after treatment
grouped <- subset(tmpdata[(tmpdata$uncuedCat==-1),],  sameDirection1S0D == "1", response_error,
                  drop = TRUE)
# check assumptions of t-test paired
# our sample size is large enough (more than 30), 
# no need to check whether the differences of the pairs follow a normal distribution Central Limit Theorem
# Shapiro-Wilk normality test for the differences
d <- with(tmpdata[(tmpdata$uncuedCat==-1),], 
          response_error[sameDirection1S0D == "0"] - response_error[sameDirection1S0D == "1"])
shapiro.test(d) # => FLAT -> p-value = 0.1305 # we assume the normality
d <- with(tmpdata[(tmpdata$uncuedCat==1),], 
          response_error[sameDirection1S0D == "0"] - response_error[sameDirection1S0D == "1"])
shapiro.test(d) # => TALL -> p-value = 0.003 # we do not assume the normality, need to 
# do Wilcoxon:  non-normality is less important in large samples (at least in respect of significance level, though power might still be an issue if you need to find small effects
#Zimmerman and Zumbo (1993)[1] suggest a Welch-t-test on the ranks which they say performs better that the Wilcoxon-Mann-Whitney in cases where the variances are unequal.
ggqqplot(d) #almost all the points fall approximately along this reference line, we can assume normality.
x <- tmpdata$response_error[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==1]
y <- tmpdata$response_error[tmpdata$uncuedCat==1 & tmpdata$sameDirection1S0D==0]
wilcox.test(x, y, paired = TRUE, 
            alternative = "greater")
t.test(x,y, paired = TRUE, alternative = "greater")
# doing mixed effects models
fit.fgm <- lmer(response_error ~ (1 | sub), data = fgmdata)
summary(fit.fgm)
par(mfrow = c(1, 2))
qqnorm(ranef(fit.fgm)$sub[,"(Intercept)"], 
       main = "Random effects")
qqnorm(resid(fit.fgm), main = "Residuals")
par(mfrow = c(1, 1))
with(tmpdata, interaction.plot(x.factor = uncuedCat, 
                               trace.factor = sub, 
                               response = response_error))
fit.fgm <- lmer(response_error ~ (1 | sub) + (1 | uncuedCat) + 
                  (1 | sub:uncuedCat), data = tmpdata)
summary(fit.fgm) # hot much of a variance is going on
fit.fgm <- lmer(response_error ~ sameDirection1S0D*uncuedCat + (1 | sub) + 
                  (1 | sub:sameDirection1S0D) + (1 | sub:uncuedCat), data = tmpdata)
summary(fit.fgm)
anova(fit.fgm) # anova for the fixed effects
# probably this is driven by circle condition. I might not need to run an ANOVA actually since there 
# also I'm moving away from uncuedCat analysis
tmpdata <- aggregate(response_error~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
ggplot(tmpdata, aes(x = uncuedAR, y = response_error, color=as.factor(sameDirection1S0D))) + 
  geom_point(size=2, shape=23, alpha = 1/2)+
  geom_smooth(method=lm, aes(fill=as.factor(sameDirection1S0D)), fullrange=TRUE)+
  theme_classic()
summary(lmer(response_error ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = tmpdata))
# nice, this model works! ADD this
# get each people's beta estimate
number_of_sub <- unique(tmpdata$sub)
fgmdata.indv_beta <- data.frame(matrix(ncol = 2, nrow = length(number_of_sub)))
# array(NA, dim= c(length(subject_IDs), 3))
for (r in 1:length(number_of_sub)){
  tmpdata_sub <- tmpdata[tmpdata$sub==number_of_sub[r],]
  #run a regression model on individual sub
  lm_sub <- lm(response_error ~ uncuedAR * sameDirection1S0D, data = tmpdata_sub)
  lm_beta <- summary(lm_sub)$coefficients[4]
  fgmdata.indv_beta[r,1] <- lm_beta
  fgmdata.indv_beta[r,2] = number_of_sub[r]
}
head(fgmdata.indv_beta)
plot(fgmdata.indv_beta$X1)
#doing it with simple regression motion seperately
tmpdata <- aggregate(response_error~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
fgmdata.indv_beta <- data.frame(matrix(ncol = 3, nrow = length(number_of_sub)))
for (r in 1:length(number_of_sub)){ 
  tmpdata_sub <- tmpdata[tmpdata$sub==number_of_sub[r],]
  #run a regression model on individual sub
  lm_sub_diff <- lm(response_error ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==0,])
  lm_beta_diff <- summary(lm_sub_diff)$coefficients[2]
  lm_sub_same <- lm(response_error ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==1,])
  lm_beta_same <- summary(lm_sub_same)$coefficients[2]
  fgmdata.indv_beta[r,1] <- lm_beta_diff
  fgmdata.indv_beta[r,2] <- lm_beta_same
  fgmdata.indv_beta[r,3] = number_of_sub[r]
}
head(fgmdata.indv_beta)
plot(fgmdata.indv_beta$X1, fgmdata.indv_beta$X2)
abline(c(0,1)) # more points lie left of the abline, same has higher response errors
#long to wide format
meltData <- melt(fgmdata.indv_beta[1:2])
head(meltData)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")+ theme_classic() +
  geom_point()
my_comparisons = list(c("X1","X2"))
ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
         line.size = 0.2, position = position_dodge(0.5))+
  stat_compare_means(paired = TRUE, label.y = 0.35, comparisons = my_comparisons)
gglinePlot <- ggline(meltData, x = "variable", y = "value", 
                     add = c("mean_ci", "jitter"), palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)
gglinePlot
summary(gglinePlot)
compare_means(value ~ variable, data = meltData, paired = TRUE,  method = "t.test")# alternative = "greater", method = "t.test"
t.test(meltData$value[meltData$variable == "X1"], meltData$value[meltData$variable == "X2"], paired = T)
ggerrorplot(meltData, x = "variable", y = "value", 
            desc_stat = "mean_ci", color = "black",
            add = "jitter", add.params = list(color = "darkgray"))+
  stat_compare_means(comparisons = my_comparisons, paired = TRUE)+
  stat_compare_means(label.y = 0.4)
#another good graph
ggline(meltData, x = "variable", y = "value",
       add = c("mean_ci", "jitter"), add.params = list(color = "darkgray"))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 1) #stat_summary(fun.y=mean, shape=25, size=0.4, col = "darkred", fill="red")
ANOVA <- aov(response_error ~ sameDirection1S0D*uncuedCat + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA) #
# strong repulsive influence overall, but grouping was able to break that enough. Probably
# due to stimuli visibility being harder.
# replication of experiment 1, works as usual, grouping flips influence
summary(lmer(response_error ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata))
# accounting cuedAR on RE
summary(lmer(response_error ~ cuedAR + uncuedAR * sameDirection1S0D + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata))
# maybe there is no need for normalization? 
# analyses of normalization
# ADDED TO MANUSCRIPT #
normAnalysis <- lmer(responseAR_normed ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE)
summary(normAnalysis)
anova(normAnalysis)
# end of ADDED TO MANUSCRIPT #
# also show the same story
# analyses of random motion
# ADDED TO MANUSCRIPT #
# first go up and toggle off the first subetting
#fgmdata <- fgmdata[fgmdata$randomTrialsR1C0==1,]
fgmdata$sameDirection1S0D_R2 <- as.factor(fgmdata$sameDirection1S0D_R2)
summary(lmer(response_error ~ uncuedAR + (1 | sub) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE))
summary(lmer(response_error ~ uncuedAR * global_org1W0B + (1 | sub) + (1 | sub:uncuedAR) + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE))
summary(lmer(responseAR_normed ~ uncuedAR + (1 | sub) + (1 | sub:uncuedAR), data = fgmdata))
summary(lmer(responseAR_normed ~ uncuedAR * global_org1W0B + (1 | sub) + (1 | sub:uncuedAR) + (1 | sub:global_org1W0B), data = fgmdata))
# end of ADDED TO MANUSCRIPT #
# random motion makes it closer to 0, and there is no interaction
# how about normalized response?
summary(lmer(responseAR_normed ~ uncuedAR * sameDirection1S0D_R2 + (1 | sub) + 
               (1 | sub:sameDirection1S0D_R2) + (1 | sub:uncuedAR), data = fgmdata))
# same story. 
# NormResponseAR with global organization
summary(lmer(responseAR_normed ~ uncuedAR + uncuedAR:sameDirection1S0D_R2 * global_org1W0B + (1 | sub) + (1 | sub:uncuedAR) + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE))
 # end of final # 
#### FINAL ADDING TO THIS SCRIPT #### 
# ADDING TO MANUSCRIPT #
# 1) regression plot stats
fullModel <- lmer(response_error ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
                     (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE)
summary(fullModel)
anova(fullModel)
# 2) uncued beta coeff stats
#doing it with simple regression motion separately
number_of_sub <- unique(fgmdata$sub)
tmpdata <- aggregate(response_error~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
fgmdata.indv_beta <- data.frame(matrix(ncol = 3, nrow = length(number_of_sub)))
for (r in 1:length(number_of_sub)){ 
  tmpdata_sub <- tmpdata[tmpdata$sub==number_of_sub[r],]
  #run a regression model on individual sub
  lm_sub_diff <- lm(response_error ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==0,])
  lm_beta_diff <- summary(lm_sub_diff)$coefficients[2]
  lm_sub_same <- lm(response_error ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==1,])
  lm_beta_same <- summary(lm_sub_same)$coefficients[2]
  fgmdata.indv_beta[r,1] <- lm_beta_diff
  fgmdata.indv_beta[r,2] <- lm_beta_same
  fgmdata.indv_beta[r,3] = number_of_sub[r]
}
head(fgmdata.indv_beta)
plot(fgmdata.indv_beta$X1, fgmdata.indv_beta$X2)
abline(c(0,1)) # more points lie left of the abline, same has higher response errors
#long to wide format
meltData <- melt(fgmdata.indv_beta[1:2])
gglinePlot <- ggline(meltData, x = "variable", y = "value", 
                     add = c("mean_ci", "jitter"), palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  stat_compare_means(label.y = 0.3)
gglinePlot
summary(gglinePlot)
compare_means(value ~ variable, data = meltData, paired = TRUE,  method = "t.test")# alternative = "greater", method = "t.test"
t.test(meltData$value[meltData$variable == "X1"], meltData$value[meltData$variable == "X2"], paired = T)
# 3) global organization stats
# a) with all interactions
summary(lmer(response_error ~ uncuedAR * sameDirection1S0D * global_org  + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR) + (1 | sub:global_org), data = fgmdata,  REML = FALSE))
# b) relevant interactions
summary(lmer(response_error ~ uncuedAR + uncuedAR:sameDirection1S0D * global_org  + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR) + (1 | sub:global_org), data = fgmdata,  REML = FALSE))
# doing it same with the normed response AR
# 1) regression
summary(lmer(responseAR_normed ~ uncuedAR * sameDirection1S0D_R2 + (1 | sub) + 
               (1 | sub:sameDirection1S0D_R2) + (1 | sub:uncuedAR), data = fgmdata))
# 2) global org
summary(lmer(responseAR_normed ~ uncuedAR + uncuedAR:sameDirection1S0D_R2 * global_org1W0B + (1 | sub) + (1 | sub:uncuedAR) + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE))
# 3) beta plot response normed
number_of_sub <- unique(fgmdata$sub)
tmpdata <- aggregate(responseAR_normed~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
fgmdata.indv_beta_response_normed <- data.frame(matrix(ncol = 3, nrow = length(number_of_sub)))
for (r in 1:length(number_of_sub)){ 
  tmpdata_sub <- tmpdata[tmpdata$sub==number_of_sub[r],]
  #run a regression model on individual sub
  lm_sub_diff <- lm(responseAR_normed ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==0,])
  lm_beta_diff <- summary(lm_sub_diff)$coefficients[2]
  lm_sub_same <- lm(responseAR_normed ~ uncuedAR, data = tmpdata_sub[tmpdata_sub$sameDirection1S0D==1,])
  lm_beta_same <- summary(lm_sub_same)$coefficients[2]
  fgmdata.indv_beta_response_normed[r,1] <- lm_beta_diff
  fgmdata.indv_beta_response_normed[r,2] <- lm_beta_same
  fgmdata.indv_beta_response_normed[r,3] = number_of_sub[r]
}
head(fgmdata.indv_beta_response_normed)
plot(fgmdata.indv_beta_response_normed$X1, fgmdata.indv_beta_response_normed$X2)
abline(c(0,1)) # more points lie left of the abline, same has higher response errors
#### MELT DATA ####
#long to wide format
meltData <- melt(fgmdata.indv_beta_response_normed[1:2])
head(meltData)
my_comparisons = list(c("X1","X2"))
beta_plot_response_normed <- ggline(meltData, x = "variable", y = "value",
                                    add = c("mean_ci", "jitter"), add.params = list(color ="variable", size = 2, alpha = 0.5))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)
beta_plot_response_normed
compare_means(value ~ variable, data = meltData, paired = TRUE,  method = "t.test")# alternative = "greater", method = "t.test"
t.test(meltData$value[meltData$variable == "X1"], meltData$value[meltData$variable == "X2"], paired = T)
# random motion
# toggle off things at the top
# fgm -> bgm
fgmdata <- bgmdata.r
fgmdata$responseAR_normed <- bgmndata.r$normedR_indv
fgmdata <- fgmdata[fgmdata$randomTrialsR1C0==1,] #random trials! 
# iterate all and get the beta coefficient
fgmdata.cleaning <- data.frame(matrix(ncol = 8, nrow = length(number_of_sub)))
colnames(fgmdata.cleaning) <- c("reg_p", "reg_beta", "meanRT", "corr","corr_p", "respAcc","trialN","id")
for (s in 1:length(number_of_sub)){
  tmpdata <- fgmdata[fgmdata$sub == number_of_sub[s],]
  lm_sub <- lm(formula = responseAR ~ cuedAR, data = tmpdata)
  fgmdata.cleaning[s,1] <- round(summary(lm_sub)$coefficients[2,4], digits = 3) #p-value
  fgmdata.cleaning[s,2] <- round(summary(lm_sub)$coefficients[2,1], digits= 3) #estimate
  fgmdata.cleaning[s,3] <- round(mean(tmpdata$rt), digits = 0)/1000
  fgmdata.cleaning[s,4] <- cor.test(tmpdata$cuedAR, tmpdata$responseAR, method = "pearson")$estimate
  fgmdata.cleaning[s,5] <- round(cor.test(tmpdata$cuedAR, tmpdata$responseAR, method = "pearson")$p.value, digits = 2)
  fgmdata.cleaning[s,6] <- mean(tmpdata$respAcc)
  fgmdata.cleaning[s,7] <- nrow(tmpdata)# check the n of each participant
  fgmdata.cleaning[s,8] <- number_of_sub[s]
}
head(fgmdata.cleaning)
# 21 agu, I found out that 19117 19142 19242 are incomplete, also remove id 18667
incompletedPeople <- c(19117, 19142, 19242, 18667) # WARNING THIS IS FOR RANDOM ANALYSIS ONLY
incompletedPeople
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople)),]
# random analysis start
fgmdata$sameDirection1S0D_R2 <- as.factor(fgmdata$sameDirection1S0D_R2)
# response error x uncued AR 
summary(lmer(response_error ~ uncuedAR + (1 | sub) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE))
# global org
summary(lmer(response_error ~ uncuedAR * global_org1W0B + (1 | sub) + (1 | sub:uncuedAR) + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE))
# normed response
summary(lmer(responseAR_normed ~ uncuedAR + (1 | sub) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE))
# global org
summary(lmer(responseAR_normed ~ uncuedAR * global_org1W0B + (1 | sub) + (1 | sub:uncuedAR) + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE))
# THE END. # 




# BELOW is analyses scripts used for CNS SF 22, before 16 Agu #
# replicate fgm analyses, response error

# normal multiple regression model
m1 <- lm(response_error ~ cuedAR, data = bgmdata)
summary(m1)

m2 <- lm(response_error ~ cuedAR + uncuedAR * as.factor(sameDirection1S0D_R2), data = bgmdata[bgmdata$randomTrialsR1C0==0,])
summary(m2)
anova(m2)

m3 <- lm(response_error ~ cuedAR + uncuedAR * sameDirection1S0D *global_org1W0B, data = bgmdata)
summary(m3)
anova(m3)

m4 <- lm(response_error ~ cuedAR + uncuedAR * sameDirection1S0D *global_org1W0B *sameCatS1D0, data = bgmdata)
summary(m4)

# response AR - coherent
m1_coherent <- lm(responseAR ~ cuedAR * uncuedAR * as.factor(sameDirection1S0D_R2), data = bgmdata[bgmdata$randomTrialsR1C0==0,])
summary(m1_coherent)
#anova(m1_coherent)

names(m1_coherent$coefficients) <- c('Intercept','Cued AR','Uncued AR', "Same Direction", "Cued AR x Uncued AR", "Cued AR x Same Direction", "Uncued AR x Same Direction",
                                            "Cued AR x Uncued AR x Same Direction")
summary(m1_coherent)

m1_coherent_global <- lm(responseAR ~ cuedAR * uncuedAR * as.factor(sameDirection1S0D_R2) * as.factor(global_org1W0B), data = bgmdata[bgmdata$randomTrialsR1C0==0,])
summary(m1_coherent_global)
anova(m1_coherent_global)

# response AR - random
m1_random <- lm(responseAR ~ cuedAR * uncuedAR, data = bgmdata[bgmdata$randomTrialsR1C0==1,])
summary(m1_random)
anova(m1_random)

# NORMED response AR - coherent
m1_coherent_normed <- lm(normedR_indv ~ cuedAR * uncuedAR * as.factor(sameDirection1S0D_R2), data = bgmndata[bgmndata$randomTrialsR1C0==0,])
summary(m1_coherent_normed)


names(m1_coherent_normed$coefficients) <- c('Intercept','Cued AR','Uncued AR', "Same Direction", "Cued AR x Uncued AR", "Cued AR x Same Direction", "Uncued AR x Same Direction",
                             "Cued AR x Uncued AR x Same Direction")
summary(m1_coherent_normed)

m1_coherent_normed_global <- lm(normedR_indv ~ cuedAR * uncuedAR * as.factor(sameDirection1S0D_R2) * as.factor(global_org1W0B), data = bgmndata[bgmndata$randomTrialsR1C0==0,])
summary(m1_coherent_normed_global)
#anova(m1_coherent_global)

# NORMED response AR - random
m1_random <- lm(normedR_indv ~ cuedAR * uncuedAR, data = bgmdata[bgmndata$randomTrialsR1C0==1,])
summary(m1_random)
#anova(m1_random)

##### test --- simple/targeted regression models ### 
# works! I'll use this to plot
m1_coherent_targeted <- lm(responseAR ~ cuedAR + uncuedAR + uncuedAR * as.factor(sameDirection1S0D_R2), data = bgmdata[bgmdata$randomTrialsR1C0==0,])
summary(m1_coherent_targeted)
names(m1_coherent_targeted$coefficients) <- c('Intercept','Cued AR','Uncued AR', "Same Direction", "Uncued AR x Same Direction")
summary(m1_coherent_targeted)

# for normalized
m1_coherent_norm_targeted <- lm(normedR_indv ~ cuedAR + uncuedAR + uncuedAR * as.factor(sameDirection1S0D_R2), data = bgmndata[bgmndata$randomTrialsR1C0==0,])
summary(m1_coherent_norm_targeted)
names(m1_coherent_norm_targeted$coefficients) <- c('Intercept','Cued AR','Uncued AR', "Same Direction", "Uncued AR x Same Direction")
summary(m1_coherent_norm_targeted)

#####


plotREF_GO(bgmdata, bgmdata$response_error, bgmdata$uncuedAR, bgmdata$sameDirection1S0D)

scatterPlot <- ggplot(bgmdata, aes(x = uncuedAR, y = response_error, colour=as.factor(sameDirection1S0D))) + 
  geom_point(shape=11, size=0.5, alpha=0.05, show.legend = FALSE) +
  #geom_density_2d(color="gray", alpha=0.3) + 
  coord_cartesian(ylim=c(-0.5, 0.5))+
  theme(legend.key.size = unit(0.2, "cm")) + 
  geom_smooth(method = "lm", span = 0.1, alpha= 0.2,
              aes(color = as.factor(sameDirection1S0D))) +
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  # scale_color_manual(name="Shared Box"
  #                    #labels=c("cued (1/2)","Neither inside", "Shared")
  #                    ,values=c("black",
  #                              "red","orange", "blue", "slateblue3" ))+
  scale_color_manual(name="Motion Direction",
                     labels=c("Unshared","Shared")
                     ,values=c("red",
                               "black"))+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) AR Response Error (taller->)") +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic()#+
#ggtitle("Response Error Fluctuations")
#facet_grid(~data$cuedAR)
scatterPlot


library(dotwhisker)
#plot lm results
dwplot(list(m1_coherent_targeted, m1_coherent_norm_targeted, m1), 
       vline = geom_vline(
         xintercept = 0,
         colour = "grey40",
         linetype = 2,
         ),
vars_order = rev(c('Intercept','Cued AR','Uncued AR', "Same Direction", "Uncued AR x Same Direction")),
model_order = c("Model 1", "Model 2", "Model 3")
)  + 
  theme(
    axis.text.x = element_text(angle = 45, face="bold")
  ) + xlab("Regression Coefficient Estimate \n") + coord_flip() + 
  scale_color_manual(
    name="Outcome Variable",
    values=c("gray60", "black", "red"),  #
    labels = c("Normalized Response AR","Response AR", "Experiment 1 - Response AR" )
    ) +
  theme_minimal()

ggsave(filename = "dotwhisker_bothExp.png",  width = 10, height = 5, units = "in", device='png', dpi=700) 





#### BELOW IS THE HISTORICAL ANALYSES SCRIPTS ####
test = data
# run ANOVA for multiple groups #
test.aov <- aov(respError ~ motion_L, data = test)
summary(test.aov)

library(multcomp)

tukey <- cld(glht(test.aov, linfct = mcp(motion_L = "Tukey")), decreasing = TRUE)

tukey$mcletters$Letters

# tuckey test representation: 
plot(tukey , las=1 , col="brown")

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=test.aov, 'data$motion_L', conf.level=0.95)

# reg anova: does it produce the same results as the .aov

test2.lm <- lm(responseAR ~ cuedAR + uncuedAR * motion_L, data = test)
aov(test2.lm)
summary(test2.lm)

#compute summary statistics of motion
library(dplyr)
group_by(data, motion_L) %>%
  summarise(
    count = n(),
    mean = mean(respError, na.rm = TRUE),
    sd = sd(respError, na.rm = TRUE)
  )

# Box plots
# ++++++++++++++++++++
# Plot respError by motion_L and color by motion_L
library("ggpubr")
ggboxplot(data, x = "motion", y = "respError", 
          color = "motion", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "respError", xlab = "motion")

#burdayim: http://www.sthda.com/english/wiki/one-way-anova-test-in-r

#testing seperate cued AR plots # 
agg <- aggregate(responseAR ~ cuedAR + uncuedAR + motion_L, data, mean)

agg1 <- subset(agg, agg$cuedAR==-0.46332)
agg2 <- subset(agg, agg$cuedAR==-0.41699)
agg3 <- subset(agg, agg$cuedAR==-0.37066)
agg4 <- subset(agg, agg$cuedAR==-0.32432)
agg5 <- subset(agg, agg$cuedAR==-0.27799)
agg6 <- subset(agg, agg$cuedAR==-0.23166)
agg7 <- subset(agg, agg$cuedAR==-0.18533)
agg8 <- subset(agg, agg$cuedAR==-0.13900)
agg9 <- subset(agg, agg$cuedAR==-0.09266)
agg10 <- subset(agg, agg$cuedAR==-0.04633)
agg11 <- subset(agg, agg$cuedAR==-0.00000)
agg12 <- subset(agg, agg$cuedAR==0.04633)
agg13 <- subset(agg, agg$cuedAR==0.09266)
agg14 <- subset(agg, agg$cuedAR==0.13899)
agg15 <- subset(agg, agg$cuedAR==0.18533)
agg16 <- subset(agg, agg$cuedAR==0.23166)
agg17 <- subset(agg, agg$cuedAR==0.27799)
agg18 <- subset(agg, agg$cuedAR==0.32432)
agg19 <- subset(agg, agg$cuedAR==0.37065)
agg20 <- subset(agg, agg$cuedAR==0.41699)
agg21 <- subset(agg, agg$cuedAR==0.46332)



#add barkod to each agg lists, e.g. agg1 = agg = 1
agg1$barkod <- 1
agg2$barkod <- 2
agg3$barkod <- 3
agg4$barkod <- 4
agg5$barkod <- 5
agg6$barkod <- 6
agg7$barkod <- 7
agg8$barkod <- 8
agg9$barkod <- 9
agg10$barkod <- 10
agg11$barkod <- 11
agg12$barkod <- 12
agg13$barkod <- 13
agg14$barkod <- 14
agg15$barkod <- 15
agg16$barkod <- 16
agg17$barkod <- 17
agg18$barkod <- 18
agg19$barkod <- 19
agg20$barkod <- 20
agg21$barkod <- 21

aggToget <- rbind(agg1,agg2, agg3, agg4, agg5, agg6, agg7, agg8, agg9, agg10, agg11, agg12, agg13, agg14, agg15, agg16, agg17, agg18, agg19, agg20, agg21)

######################################################
# write csv to do further curve fitting in python #
######################################################

write.csv(aggToget, "aggToget.csv", row.names = FALSE)
write.csv(data, "aggTogetDATA.csv", row.names = FALSE)
######################################################
# end write csv to do further curve fitting in python #
######################################################

testPlots1 <- ggplot(data, aes(x = uncuedAR, y =responseAR )) + 
  geom_point(shape=19, size=0.2, alpha=0.05, show.legend = FALSE) +
   geom_smooth(method = "lm", 
               aes(color = factor(cuedAR)), alpha = 0.1) +
  #xlim(-.5, .5) +
  #ylim(-.7,.7) +
  # labs(x="arDiff", y = "Reported aspect ratio") +
  ggtitle("Grid by Motion Condition & pairSameARCat'", subtitle = "positive slope for same, negative-ish for diff") 
testPlots1 + scale_colour_grey() + theme_bw() + facet_grid(c(~pairSameARCat, ~motion_L)) + facet_grid(c(~motion_L, ~cued_motion_dir)) + facet_grid(c(~globalOrg, ~motion_L)) + facet_grid(~motion_L) 

# do regression analyses with groups of barkod

aggToget = subset(aggToget, !(aggToget$motion_L=="random")) # look only 1-2 motion
regCued <- lm(responseAR ~ uncuedAR * cuedAR * motion_L * cuedMotDir, data)
summary(regCued)
aov(regCued)