# fitting cumulative distribution data
# clear the workspace
rm(list=ls()) 
# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table", "lme4", "magrittr", "fitdistrplus",
     "gridExtra", "statmod", "dotwhisker","lmerTest")
require(x)
lapply(x, require, character.only = TRUE)
source("~/Documents/GitHub/ger_R_functions/plot_functions.R")
#remove scientific notation in the entire R session
options(scipen = 100)
# locate the data and import it
setwd("/Users/gorkem.er/Desktop/21Projects/background-motion/Data_analysis")
fgmdata = read.csv('bgmdata_cleaned.csv', header = TRUE)


