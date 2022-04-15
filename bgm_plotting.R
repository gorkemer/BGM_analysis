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

#### locate the data and import it ####
setwd("/Users/gorkem.er/Desktop/21Projects/background-motion/Data_analysis")

# load bgm data 
bgmdata = read.csv("bgmdata.csv",header=TRUE, quote="\"") 
bgmdata.c = subset(bgmdata, bgmdata$randomTrialsR1C0 == 0)
bgmdata.r = subset(bgmdata, bgmdata$randomTrialsR1C0 == 1)

# load bgmn data 
bgmndata = read.csv("bgmndata.csv",header=TRUE, quote="\"") 
bgmndata.c = subset(bgmndata, bgmndata$randomTrialsR1C0 == 0)
bgmndata.r = subset(bgmndata, bgmndata$randomTrialsR1C0 == 1)

#### identify/rename variables for plotting ####

# plot by cued vector vs uncued vector
bgmndata.c$cued_vector <- ifelse(bgmndata.c$cueType == 1, bgmndata.c$e1_motion_dir, bgmndata.c$e2_motion_dir)
bgmndata.c$uncued_vector <- ifelse(bgmndata.c$cueType == 1, bgmndata.c$e2_motion_dir, bgmndata.c$e1_motion_dir)

# reorder levels
#bgmndata.c$cued_vector <- factor(bgmndata.c$cued_vector, levels = c(0, 180, 90, 270))
#bgmndata.c$uncued_vector <- factor(bgmndata.c$uncued_vector, levels = c(0, 180, 90, 270))

# rename levels
#bgmndata.c$cued_vector <- factor(bgmndata.c$cued_vector, labels = c( sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'), sprintf('\u2193')))
#bgmndata.c$uncued_vector <- factor(bgmndata.c$uncued_vector, labels = c(sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'),sprintf('\u2193')))

bgmndata.c$global_org <- factor(bgmndata.c$global_org, labels = c("between", "within" ))

#### plotting variables ####
yaxisLim <- 0.1
densityAlpha <- 0.1
densityColor <- "blue"
jitterAlpha <- 0.1
identicalMotion_label <- "Same"
differentMotion_label <- "Different"
identicalMotion_color <- "red"
differentMotion_color <- "black"
lmAlpha <- 0.3

# for plotting on the aggregated data 
bgmndata.c <- aggregate(response_error ~ uncuedAR  + sameDirection1S0D + sub, bgmndata.c, mean)

#### bgm plot 01 ####

bgmplot01 <- ggplot(bgmndata.c, aes(x = uncuedAR, y = normedR_indv, colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  coord_cartesian(ylim=c(-yaxisLim, yaxisLim)) +
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(color = as.factor(sameDirection1S0D))) +
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) Response Error (taller->)") +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic()
bgmplot01
ggsave(filename = "bgmplot01.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 

#### fgm plot 02 ####

bgmndata.ca <- aggregate(normedR_indv ~ uncuedAR  + sameDirection1S0D + sub + global_org+
                         cued_vector + uncued_vector, bgmndata.c, mean)
jitterAlpha <- 0.05
bgmplot02 <- ggplot(bgmndata.ca, aes(x = uncuedAR, y = normedR_indv, colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=11, size=0.5, alpha=0.5, show.legend = FALSE) +
  geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  geom_density_2d(color="blue", alpha=densityAlpha) + 
  coord_cartesian(ylim=c(-0.2, 0.2)) +
  #theme(legend.key.size = unit(0.2, "cm")) + 
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(color = as.factor(sameDirection1S0D))) +
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) Response Error (taller->)") +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic()+ 
  facet_wrap(~global_org*cued_vector*uncued_vector, nrow = 2)+
  theme_classic() +
  theme(panel.spacing.x = unit(1, "lines"))

bgmplot02
jitterAlpha <- 0.1
ggsave(filename = "bgmplot02.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 


#### save plots ####

#ggsave(filename = "fgm_plot2.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 
#savePlot(interactionPlotVectors)

#### fgm plot 03 ####
agg <- aggregate(responseAR ~ cuedAR + uncuedAR + sameDirection1S0D, bgmndata.c, mean) # or normedR_indv

agg1 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[1])
agg2 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[2])
agg3 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[3])
agg4 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[4])
agg5 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[5])
agg6 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[6])
agg7 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[7])
agg8 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[8])
agg9 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[9])
agg10 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[10])
agg11 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[11])
agg12 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[12])
agg13 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[13])
agg14 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[14])
agg15 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[15])
agg16 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[16])
agg17 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[17])
agg18 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[18])
agg19 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[19])
agg20 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[20])
agg21 <- subset(agg, agg$cuedAR==sort(unique(bgmndata.c$cuedAR))[21])

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

aggToget$sameDirection1S0D <- factor(aggToget$sameDirection1S0D, labels = c("Ungrouped", "Grouped"))

bgmplot03 <- ggplot(aggToget, aes(x = uncuedAR, y =responseAR )) + 
  geom_point(shape=1, size=0.2, alpha=0.5, show.legend = FALSE, aes(color = as.factor(cuedAR))) +
  coord_cartesian(ylim=c(-0.4, 0.4)) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha + 0.03, show.legend = FALSE) +
  #geom_line(data = aggToget, aes(group = barkod, colour = as.factor(barkod))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  geom_smooth(method = "lm",
              aes(color = factor(cuedAR)), alpha = lmAlpha-0.3)+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) Response (taller->)")+ 
  facet_grid(~sameDirection1S0D) + 
  scale_colour_grey() + 
  theme_classic() + 
  theme(panel.spacing.x = unit(1.5, "lines"))
bgmplot03
ggsave(filename = "bgmplot03.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 


