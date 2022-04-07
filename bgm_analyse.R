# April 6th, 2022.
# Analysis script for the background motion experiment. 

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
bgmdata = read.csv("bgmdata.csv",header=TRUE, quote="\"") 

# replicate fgm analyses, response error

# normal multiple regression model
m1 <- lm(response_error ~ cuedAR, data = bgmdata)
summary(m1)

m2 <- lm(response_error ~ cuedAR + uncuedAR * as.factor(sameDirection1S0D_R2), data = bgmdata)
summary(m2)
anova(m2)

m3 <- lm(response_error ~ cuedAR + uncuedAR * sameDirection1S0D *global_org1W0B, data = bgmdata)
summary(m3)
anova(m3)

m4 <- lm(response_error ~ cuedAR + uncuedAR * sameDirection1S0D *global_org1W0B *sameCatS1D0, data = bgmdata)
summary(m4)




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