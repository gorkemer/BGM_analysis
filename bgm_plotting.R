# April 12th, 2022.
# Plotting script for the background motion experiment. 
# Using bgm_analyses.R and bgm_analysis2.R scripts for plotting 
# bgmdata and bgmndata.

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

# remove scientific notation in the entire R session
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

#plotREF_GO(bgmdata, bgmdata$response_error, bgmdata$uncuedAR, bgmdata$sameDirection1S0D)

interactionPlot <- ggplot(bgmndata.c, aes(x = uncuedAR, y = normedR_indv, colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=11, size=0.5, alpha=0.01, show.legend = FALSE) +
  geom_jitter(shape=11, size=0.5, alpha=0.01, show.legend = FALSE) +
  #geom_density_2d(color="gray", alpha=0.3) + 
  coord_cartesian(ylim=c(-0.05, 0.05)) +
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
  theme_classic() #+
#ggtitle("Response Error Fluctuations")
#facet_grid(~data$cuedAR)
interactionPlot

basicPlot <- ggplot(bgmndata.c, aes(x = cuedAR, y = responseAR)) + 
  geom_jitter(shape=11, size=0.5, alpha=0.02, show.legend = FALSE) +
  #geom_density_2d(color="gray", alpha=0.3) + 
  #coord_cartesian(ylim=c(-0.05, 0.05)) +
  theme(legend.key.size = unit(0.2, "cm")) + 
  geom_smooth(method = "lm", span = 0.1, alpha= 0.2) +
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  # scale_color_manual(name="Shared Box"
  #                    #labels=c("cued (1/2)","Neither inside", "Shared")
  #                    ,values=c("black",
  #                              "red","orange", "blue", "slateblue3" ))+
  # scale_color_manual(name="Motion Direction",
  #                    labels=c("Unshared","Shared")
  #                    ,values=c("red",
  #                              "black"))+
  labs(x="(<-flatter)   Cued AR   (taller->)", y = "(<-flatter) AR Response Error (taller->)") +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic() #+
#ggtitle("Response Error Fluctuations")
#facet_grid(~data$cuedAR)
basicPlot

interactionPlot_id <- ggplot(bgmndata.c, aes(x = uncuedAR, y = normedR_indv, colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=11, size=0.5, alpha=0.01, show.legend = FALSE) +
  geom_jitter(shape=11, size=0.5, alpha=0.01, show.legend = FALSE) +
  geom_density_2d(color="gray", alpha=0.3) + 
  coord_cartesian(ylim=c(-0.15, 0.15)) +
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
  theme_classic() #+
#ggtitle("Response Error Fluctuations")
#facet_grid(~data$cuedAR)
interactionPlot_id + facet_wrap(~sub, nrow = 8)


agg <- aggregate(responseAR ~ cuedAR + uncuedAR + sameDirection1S0D, bgmndata.c, mean)

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

barkodPlot <- ggplot(bgmndata.c, aes(x = uncuedAR, y =normedR_indv )) + 
  #geom_point(shape=19, size=0.2, alpha=0.05, show.legend = FALSE) +
  coord_cartesian(ylim=c(-0.55, 0.55)) +
  geom_jitter(shape=19, size=0.2, alpha=0.01, show.legend = FALSE) +
  geom_density_2d(color="gray", alpha=0.3) + 
  geom_smooth(method = "lm", 
              aes(color = factor(cuedAR)), alpha = 0.1) +
  #xlim(-.5, .5) +
  #ylim(-.7,.7) +
  # labs(x="arDiff", y = "Reported aspect ratio") +
  ggtitle("Grid by Motion Condition", subtitle = "positive slope for same, negative-ish for diff") 
barkodPlot + scale_colour_grey() + theme_bw() + facet_grid(~sameDirection1S0D)

