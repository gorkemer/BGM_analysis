# April 12th, 2022.
# Plotting script for the background motion experiment. 
# Using bgm_analyses.R and bgm_analysis2.R scripts for plotting 
# bgmdata and bgmndata.

# last update: 16 Agu 22

# clear the workspace
rm(list=ls()) 
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker", "lmerTest")
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
number_of_sub <- unique(fgmdata$sub)
table(fgmdata$sameDirection1S0D_R2)
fgmdata$responseAR_normed <- bgmndata.c$normedR_indv
fgmdata$cuedCat = ifelse(fgmdata$cuedAR < 0, -1, ifelse(fgmdata$cuedAR==-0, 0, 1))
fgmdata$uncuedCat = ifelse(fgmdata$uncuedAR < 0, -1, ifelse(fgmdata$uncuedAR==-0, 0, 1))
fgmdata$uncuedCat <- as.factor(fgmdata$uncuedCat)
fgmdata$respAcc <- ifelse( (fgmdata$responseAR > 0 & fgmdata$cuedCat == 1)  | (fgmdata$responseAR < 0 & fgmdata$cuedCat == -1) | (fgmdata$responseAR == 0 & fgmdata$cuedCat == 0), 1, 0)
table(fgmdata$respAcc)
number_of_sub <- unique(fgmdata$sub)
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
testToBeCleaned = below50P[1]
fgmdata.cleaning[fgmdata.cleaning$id == 18667,] #18667
plot(fgmdata$cuedAR[fgmdata$sub == testToBeCleaned], fgmdata$responseAR[fgmdata$sub == testToBeCleaned])
# those below40P all show bad performance in all metrics (e.g. regression beta, p-value, correlation, reaction time). 
incompletedPeople <- fgmdata.cleaning$id[fgmdata.cleaning$trialN<241]
incompletedPeople
#### CLEANED DATA ####
#fgmdata <- fgmdata[!( (fgmdata$sub %in% below50P)),]
fgmdata <- fgmdata[!( (fgmdata$sub %in% incompletedPeople)),]
# check, also remove id 18667
18667 %in% unique(fgmdata$sub) 
fgmdata <- fgmdata[!(fgmdata$sub == 18667),] # because 0.53 corr
#### plotting variables ####
yaxisLim <- 0.05
densityAlpha <- 0.2
densityColor <- "blue"
jitterAlpha <- 0.1
identicalMotion_label <- "Same"
differentMotion_label <- "Different"
identicalMotion_color <- "black"
differentMotion_color <- "darkgray"
lmAlpha <- 0.1
# plotting regression plot
tmpdata <- aggregate(response_error~ cuedAR + uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
regression_plot <- ggplot(tmpdata, aes(x = uncuedAR, y = response_error, color=as.factor(sameDirection1S0D))) + 
  geom_smooth(method=lm, aes(fill=as.factor(sameDirection1S0D)), fullrange=FALSE, alpha= lmAlpha)+
  coord_cartesian(ylim=c(-0.02, 0.04)) +
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray50") + 
  #geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                             identicalMotion_color))+
  labs(x="(< flatter) Uncued AR (taller >)", y = "(< err on flatter) Mean Response Error ( err on taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    #axis.text.x = element_text(size=12,color="black"),
    #axis.text.y = element_text(size=12,color="black"),
    #legend.title = element_text(size=14),
    #legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    #axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    #axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
  )
regression_plot
# anova # ADDED TO THE MANUSCRIPT
plot01_reg <- lmer(response_error ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
                     (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE)
summary(plot01_reg)
anova(plot01_reg)
plot01_reg <- lmer(response_error ~ uncuedAR + uncuedAR:sameDirection1S0D + (1 | sub) + 
                     (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata, REML = FALSE)
summary(plot01_reg)
# end of ADDED TO THE MANUSCRIPT
# start of ADDED TO THE MANUSCRIPT # GLOBAL ORG
globalOrg_reg <- lmer(response_error ~ uncuedAR * sameDirection1S0D * global_org1W0B + (1 | sub) + 
                     (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR) + + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE)
summary(globalOrg_reg)
globalOrg_reg <- lmer(response_error ~ uncuedAR + uncuedAR:sameDirection1S0D * global_org1W0B + (1 | sub) + 
                        (1 | sub:sameDirection1S0D:uncuedAR) + (1 | sub:uncuedAR) + + (1 | sub:global_org1W0B), data = fgmdata, REML = FALSE)
summary(globalOrg_reg)
# end of ADDED TO THE MANUSCRIPT # GLOBAL ORG
# anova with cuedAR included # 
plot01_reg <- lmer(response_error ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
                     (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = tmpdata)
summary(plot01_reg)
anova(plot01_reg)
#doing it with simple regression motion seperately
number_of_sub <- unique(fgmdata$sub)
tmpdata <- aggregate(response_error~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
tmpdata_cued <- aggregate(response_error~ cuedAR + uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
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
#### MELT DATA ####
#long to wide format
meltData <- melt(fgmdata.indv_beta[1:2])
head(meltData)
my_comparisons = list(c("X1","X2"))
ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
         line.size = 0.2, position = position_dodge(0.5))+
  stat_compare_means(paired = TRUE, label.y = 0.35, comparisons = my_comparisons)
gglinePlot <- ggline(meltData, x = "variable", y = "value", 
                     add = c("mean_ci", "jitter"), palette = "jco")+ 
  stat_compare_means(paired = TRUE, comparisons = my_comparisons, label.y = 0.3)+
  geom_violin(alpha = 1/60)
gglinePlot
ggpaired(meltData, x = "variable", y = "value", line.color = "gray",
         line.size = 0.2)+
  stat_compare_means(paired = TRUE, label.y = 0.35, comparisons = my_comparisons)
compare_means(value ~ variable, data = meltData, paired = TRUE,  method = "t.test")# alternative = "greater", method = "t.test"
ggerrorplot(meltData, x = "variable", y = "value", 
            desc_stat = "mean_ci", color = "black",
            add = "jitter", add.params = list(color = "variable"))+
  stat_compare_means(comparisons = my_comparisons, paired = TRUE)+
  stat_compare_means(label.y = 0.5)+
  geom_violin(alpha = 1/60)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                             identicalMotion_color))
# ADDED TO MANUSCRIPT #
t.test(meltData$value[meltData$variable == "X1"], meltData$value[meltData$variable == "X2"], paired = T)
# END OF ADDED TO THE MANUSCRIPT # 
#another good graph
beta_plot <- ggline(meltData, x = "variable", y = "value",
                    add = c("mean_ci", "jitter"), add.params = list(size = 2, alpha = 0.5, color ="variable"))+
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c("darkgray",
                               "black"))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30") + 
  coord_cartesian(ylim=c(-0.6, 0.6)) 
beta_plot
ANOVA <- aov(response_error ~ sameDirection1S0D*uncuedAR + Error(as.factor(sub)/(sameDirection1S0D)), data=fgmdata)
summary(ANOVA)
# alternative to beta_plot
beta_plot2 <- ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
                       line.size = 0.2)+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
#beta_plot2$layers <- beta_plot2$layers[-3]
beta_plot2
beta_plot3 <- beta_plot2
beta_plot3$layers <- beta_plot3$layers[-2]
beta_plot3
# uncuedCat plot
tmpdata <- aggregate(response_error~ uncuedCat + sub + sameDirection1S0D, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
my_comparisons = list(c("0","1"))
table(tmpdata$uncuedCat)
table(tmpdata$uncuedCat, tmpdata$sameDirection1S0D)
uncuedCat_plot <- ggline(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error",
                         add = c("mean_se", "jitter"),
                         color = "uncuedCat", palette = "jco", add.params = list(size = 2, alpha = 0.2))+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Uncued Shape Category",
                     labels=c("Flat -","Tall |")
                     ,values=c("red",
                               "black"))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
uncuedCat_plot
uncuedCat_plot2 <- uncuedCat_plot + facet_grid(~uncuedCat)
# global org plot
tmpdata <- aggregate(response_error~ uncuedCat + sub + sameDirection1S0D + global_org1W0B, fgmdata, mean)
tmpdata$sameDirection1S0D <- as.factor(tmpdata$sameDirection1S0D)
tmpdata$uncuedCat <- as.factor(tmpdata$uncuedCat)
tmpdata$global_org1W0B <- as.factor(tmpdata$global_org1W0B)
my_comparisons = list(c("0","1"))
global_org_plot <- ggline(tmpdata[!(tmpdata$uncuedCat==0),], x = "sameDirection1S0D", y = "response_error",
                          add = c("mean_se", "jitter"),
                          color = "uncuedCat", palette = "jco")+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
global_org_plot
global_org_plot + facet_grid(~uncuedCat+global_org1W0B)
# responseAR_normed
tmpdata <- aggregate(responseAR_normed~ uncuedAR + sub + sameDirection1S0D, fgmdata, mean)
regression_plot_response_normed <- ggplot(tmpdata, aes(x = uncuedAR, y = responseAR_normed, color=as.factor(sameDirection1S0D))) + 
  geom_smooth(method=lm, aes(fill=as.factor(sameDirection1S0D)), fullrange=FALSE, alpha= lmAlpha)+
  coord_cartesian(ylim=c(-0.02, 0.04)) +
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray50") + 
  #geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                             identicalMotion_color))+
  labs(x="(< flatter) Uncued AR (taller >)", y = "(< err on flatter) Mean Response Error ( err on taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic() +
  theme(legend.position = "none")
regression_plot_response_normed
# beta plot response normed
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
  stat_compare_means(paired= TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30") + 
  coord_cartesian(ylim=c(-0.6, 0.6))
beta_plot_response_normed
beta_plot2_response_normed <- ggpaired(meltData, x = "variable", y = "value", line.color = "gray", 
                       line.size = 0.2)+
  stat_compare_means(paired = TRUE, comparisons = my_comparisons)+
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  geom_hline(yintercept=0.0,linetype="longdash", color = "gray30")
#beta_plot2$layers <- beta_plot2$layers[-3]
beta_plot2_response_normed
beta_plot3_response_normed <- beta_plot2_response_normed
beta_plot3_response_normed$layers <- beta_plot3_response_normed$layers[-2]
beta_plot3_response_normed
########## end ########## 
ylabel_beta <- 2
ylabel_uncued <- 2
regression_plot + theme(legend.position = "none") + coord_cartesian(ylim=c(-0.03, 0.04)) 
ggsave(filename = "bgm_regression_plot.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "bgm_beta_plot.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot2 + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "bgm_beta_plot2.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot3 + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "bgm_beta_plot3.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
uncuedCat_plot + coord_cartesian(ylim=c(-0.3, 0.3)) + stat_compare_means(label.y = ylabel_uncued) + theme(legend.position = "none")
ggsave(filename = "bgm_uncuedCat_plot.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
uncuedCat_plot + geom_violin(alpha=1/60) + coord_cartesian(ylim=c(-0.3, 0.3)) + stat_compare_means(label.y = ylabel_uncued) + theme(legend.position = "none")
ggsave(filename = "bgm_uncuedCat_plot_violin.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
uncuedCat_plot2 + coord_cartesian(ylim=c(-0.3, 0.3)) + stat_compare_means(label.y = ylabel_uncued) + theme(legend.position = "none")
ggsave(filename = "bgm_uncuedCat_plot2.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

regression_plot_response_normed + theme(legend.position = "none") + coord_cartesian(ylim=c(-0.03, 0.04))
ggsave(filename = "bgm_regression_plot_response_normed.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot_response_normed + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "bgm_beta_plot_response_normed.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot2_response_normed + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "bgm_beta_plot2_response_normed2.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
beta_plot3_response_normed + coord_cartesian(ylim=c(-0.5, 0.5)) + stat_compare_means(label.y = ylabel_beta) + theme(legend.position = "none")
ggsave(filename = "bgm_beta_plot3_response_normed.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 
# analyses of normalization
summary(lmer(responseAR_normed ~ uncuedAR * sameDirection1S0D + (1 | sub) + 
               (1 | sub:sameDirection1S0D) + (1 | sub:uncuedAR), data = fgmdata))


# no global organization plot global_org1W0B1W0B_plot + facet_grid(~uncuedCat+global_org1W0B)
###################################################################



### BELOW IS before 16, used for CNS SF 22 ###


#### identify/rename variables for plotting ####

# plot by cued vector vs uncued vector
bgmndata.c$cued_vector <- ifelse(bgmndata.c$cueType == 1, bgmndata.c$e1_motion_dir, bgmndata.c$e2_motion_dir)
bgmndata.c$uncued_vector <- ifelse(bgmndata.c$cueType == 1, bgmndata.c$e2_motion_dir, bgmndata.c$e1_motion_dir)

# reorder levels
bgmndata.c$cued_vector <- factor(bgmndata.c$cued_vector, levels = c(0, 180, 90, 270))
bgmndata.c$uncued_vector <- factor(bgmndata.c$uncued_vector, levels = c(0, 180, 90, 270))

# rename levels
bgmndata.c$cued_vector <- factor(bgmndata.c$cued_vector, labels = c( sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'), sprintf('\u2193')))
bgmndata.c$uncued_vector <- factor(bgmndata.c$uncued_vector, labels = c(sprintf('\u2192'), sprintf('\u2190'), sprintf('\u2191'),sprintf('\u2193')))

bgmndata.c$global_org1W0B <- factor(bgmndata.c$global_org1W0B, labels = c("between", "within" ))

#### plotting variables ####
yaxisLim <- 0.05
densityAlpha <- 0.1
densityColor <- "blue"
jitterAlpha <- 0.1
identicalMotion_label <- "Same"
differentMotion_label <- "Different"
identicalMotion_color <- "red"
differentMotion_color <- "black"
lmAlpha <- 0.1

# for plotting on the aggregated data 
bgmndata.ca <- aggregate(response_error ~ uncuedAR  + sameDirection1S0D + sub, bgmndata.c, mean)

#### bgm plot 01 ####

bgmplot01 <- ggplot(bgmndata.c,cex=3, aes(x = uncuedAR, y = normedR_indv, fill=as.factor(sameDirection1S0D) , colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_density_2d(color=densityColor, alpha=densityAlpha) + 
  coord_cartesian(ylim=c(-0.02, 0.04)) +
  #coord_cartesian(ylim=c(-yaxisLim/4, yaxisLim/1.5)) +
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(fill = as.factor(sameDirection1S0D))) +
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  scale_fill_manual(values=c(differentMotion_color,
                             identicalMotion_color))+
  labs(x="(< flatter) Uncued AR (taller >)", y = "(< flatter) Mean Normalized AR Response ( taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
  )

bgmplot01
ggsave(filename = "bgmplot01.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

#ggsave(filename = "bgmplot01.pdf", width = 5, height = 5, units = "in", device='pdf', dpi=700) 

#### fgm plot 02 ####

bgmndata.ca <- aggregate(normedR_indv ~ uncuedAR  + sameDirection1S0D + sub + global_org1W0B+
                         cued_vector + uncued_vector, bgmndata.c, mean)
jitterAlpha <- 0.05
bgmplot02 <- ggplot(bgmndata.c, aes(x = uncuedAR, y = normedR_indv, colour=as.factor(sameDirection1S0D))) + 
  #geom_point(shape=11, size=0.5, alpha=0.5, show.legend = FALSE) +
  #geom_jitter(shape=1, size=0.5, alpha=jitterAlpha, show.legend = FALSE) +
  #geom_density_2d(color="blue", alpha=densityAlpha) + 
  coord_cartesian(ylim=c(-0.05, 0.05)) +
  #theme(legend.key.size = unit(0.2, "cm")) + 
  geom_smooth(method = "lm", span = 1, alpha= lmAlpha,
              aes(color = as.factor(sameDirection1S0D))) +
  geom_segment(aes(x=min(unique(uncuedAR)),xend=max(unique(uncuedAR)),y=0,yend=0), linetype="longdash",  color="gray50")+ 
  scale_color_manual(name="Motion Direction",
                     labels=c(differentMotion_label,identicalMotion_label)
                     ,values=c(differentMotion_color,
                               identicalMotion_color))+
  labs(x="(<-flatter)   Uncued AR   (taller->)", y = "(<-flatter) Normalized AR Response (taller->)") +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  theme_classic()+ 
  facet_wrap(~cued_vector+uncued_vector, nrow = 4)+
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=8,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5),
    panel.spacing.x = unit(1, "lines"),
    #strip.text.x = element_blank(),
    strip.background = element_blank()
  )

bgmplot02
jitterAlpha <- 0.1
ggsave(filename = "bgmplot02.png", width =5 , height = 6, units = "in", device='png', dpi=700) 


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
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  geom_smooth(method = "lm",
              aes(color = factor(cuedAR)), alpha = lmAlpha-0.3)+
  labs(x="(<-flatter)   Uncued AR (taller->)", y = "(<-flatter) AR Response (taller->)")+ 
  facet_grid(~sameDirection1S0D) + 
  scale_colour_grey() + 
  theme_classic() + 
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=10,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5),
    panel.spacing.x = unit(1, "lines")
  )
  theme(panel.spacing.x = unit(1.5, "lines"))
bgmplot03
ggsave(filename = "bgmplot03_nonNorm.png", width = 5, height = 5, units = "in", device='png', dpi=700) 

#ggsave(filename = "bgmplot03.pdf", width = 14, height = 8, units = "in", device='pdf', dpi=700) 


#### bgm plot 04 ####


#fgmdata$responseAR_norm <- (fgmdata$responseAR-min(fgmdata$responseAR))/(max(fgmdata$responseAR)-min(fgmdata$responseAR))
bgmdata.submean <- aggregate(responseAR ~ cuedAR + sub, bgmdata, mean)
bgmplot04 <- ggplot(bgmdata.submean, aes(x = cuedAR, y = responseAR)) +
  geom_jitter(alpha = 1/4, aes(color = as.factor(sub))) +
  geom_density_2d(color=densityColor, alpha=densityAlpha) +
  # geom_abline(linetype = 11,  color="gray50") +
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),y=0,yend=0), linetype = 11,  color="gray30")+ 
  geom_segment(aes(x=min(unique(cuedAR)),xend=max(unique(cuedAR)),
                   y=min(unique(cuedAR)),yend=max(unique(cuedAR))), 
               linetype = 11,  color="gray30")+ 
  geom_smooth(method = "lm", color = "firebrick2") +
  # geom_smooth(method="nls",
  #             formula= y ~ x * a * w *  sqrt(2)/exp(-(0.5)) * exp(-(w*x^2)), #y ~ a + b*x, # this is an nls argument
  #             start = list(a = 0.39, w = 0.63),
  #             se=FALSE, algorithm="port",
  #             color = "firebrick2") + #aes(color = as.factor(sameDirection1S0D))
  labs(x="(< flatter) Cued AR (taller >)", y = "(< flatter) Mean AR Response ( taller >)", size = 10.5) +
  labs(title="", subtitle=" ")+
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.5))+
  coord_cartesian(ylim=c(-0.5, 0.5)) +
  theme_classic() +
  theme(
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black"),
    legend.title = element_text(size=14),
    legend.text = element_text(size=12),
    legend.position = "none",#c(0.8, 0.14),
    axis.title.y = element_text(size = rel(1.5), angle = 90, hjust = -0.5),
    axis.title.x = element_text(size = rel(1.5), angle = 0,  vjust = -0.5)
  )


bgmplot04 #+ facet_wrap(~sub) + #c(0.8, 0.14),
ggsave(filename = "bgmplot04.png",  width = 5, height = 5, units = "in", device='png', dpi=700) 

