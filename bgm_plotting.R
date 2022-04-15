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
# April 12th, 2022.
# Plotting script for the foreground motion experiment. 
# Same/similar to bgm_plotting.R script. Only doing it for the fgm plots
# plots to be used for the manuscript
# good representative participant: 939045

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

savePlot <- function(myPlot) {
  pdf("myPlot.pdf")
  print(myPlot)
  dev.off()
}

#### locate the data and import it ####
# loading data
setwd('~/Desktop/21Projects/Single_FG_Motion')
fgmdata = read.csv('fgmdata.csv', header = TRUE)

#### identify/rename variables for plotting ####

# plot by cued vector vs uncued vector
fgmdata$cued_vector <- ifelse(fgmdata$cued_ellipse == 1, fgmdata$e1_motion_dir, fgmdata$e2_motion_dir)
fgmdata$uncued_vector <- ifelse(fgmdata$cued_ellipse == 1, fgmdata$e2_motion_dir, fgmdata$e1_motion_dir)

# reorder levels
fgmdata$cued_vector <- factor(fgmdata$cued_vector, levels = c(0, 180, 90, 270))
fgmdata$uncued_vector <- factor(fgmdata$uncued_vector, levels = c(0, 180, 90, 270))

# rename levels
fgmdata$cued_vector <- factor(fgmdata$cued_vector, labels = c( sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'), sprintf('\u2193')))
fgmdata$uncued_vector <- factor(fgmdata$uncued_vector, labels = c(sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'),sprintf('\u2193')))

fgmdata$global_org <- factor(fgmdata$global_org, labels = c("between", "within" ))

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

# remove scientific notation in the entire R session
options(scipen = 100)

# for plotting on the aggregated data 
fgmdata.a <- aggregate(responseError ~ uncuedAR  + sameDirection1S0D + sub, fgmdata, mean)

#### fgm plot 01 ####

bgmplot03 <- ggplot(fgmdata, aes(x = uncuedAR, y = responseError, colour=as.factor(sameDirection1S0D))) + 
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
bgmplot03
ggsave(filename = "bgmplot03.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 

#### fgm plot 02 ####

fgmdata.a <- aggregate(responseError ~ uncuedAR  + sameDirection1S0D + sub + global_org+
                         cued_vector + uncued_vector, fgmdata, mean)
jitterAlpha <- 0.05
fgmplot02 <- ggplot(fgmdata.a, aes(x = uncuedAR, y = responseError, colour=as.factor(sameDirection1S0D))) + 
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
agg <- aggregate(responseAR ~ cuedAR + uncuedAR + sameDirection1S0D, fgmdata, mean)

agg1 <- subset(agg, agg$cuedAR==-0.46331866)
agg2 <- subset(agg, agg$cuedAR==-0.41698684)
agg3 <- subset(agg, agg$cuedAR==-0.37065503)
agg4 <- subset(agg, agg$cuedAR==-0.32432321)
agg5 <- subset(agg, agg$cuedAR==-0.27799139)
agg6 <- subset(agg, agg$cuedAR==-0.23165957)
agg7 <- subset(agg, agg$cuedAR==-0.18532775)
agg8 <- subset(agg, agg$cuedAR==-0.13899593)
agg9 <- subset(agg, agg$cuedAR==-0.09266412)
agg10 <- subset(agg, agg$cuedAR==-0.04633230)
agg11 <- subset(agg, agg$cuedAR==-0.00000048)
agg12 <- subset(agg, agg$cuedAR==0.04633134)
agg13 <- subset(agg, agg$cuedAR==0.09266316)
agg14 <- subset(agg, agg$cuedAR==0.13899498)
agg15 <- subset(agg, agg$cuedAR==0.18532679)
agg16 <- subset(agg, agg$cuedAR==0.23165861)
agg17 <- subset(agg, agg$cuedAR==0.27799043)
agg18 <- subset(agg, agg$cuedAR==0.32432225)
agg19 <- subset(agg, agg$cuedAR==0.37065407)
agg20 <- subset(agg, agg$cuedAR==0.41698588)
agg21 <- subset(agg, agg$cuedAR==0.46331770)

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


