# April 6th, 2022.
# Analysis script for the background motion experiment. 

# clear the workspace
rm(list=ls()) 

# load libraries in bulk
x<-c("ggpubr", "ggplot2", "multcomp", "pastecs", "tidyr","dplyr", "ggiraph", "ggiraphExtra", "plyr", 
     "covreg", "plot3D", "Hmisc", "corrplot", "psych", "tidyverse", "hrbrthemes", "viridis", "gapminder",
     "ggExtra", "scatterplot3d", "reshape2", "rlang", "plyr", "data.table")

require(x)
lapply(x, require, character.only = TRUE)


# set wd if needed setwd('~/Desktop/background-motion/data'), check with getwd()
setwd('/Users/gorkem.er/Desktop/background-motion/Data_analysis')
bgmdata = read.csv('background-motion22June.csv', header=TRUE)

# get only response trials
bgmdata <- bgmdata[which(bgmdata$GE_trial_type == "response-trial"),]

# there are some duplicates in the data by mistake, removing them.
dupped.bgmdata <- unique( bgmdata[ ,] )
bgmdata <- dupped.bgmdata

#Update: below is not needed because they are not in the response-trial subsetted data frame anymore.
# remove id 57746, 141906, because those are my test-data, #17952, 19101, 19198, 19210 are empty participants.
#bgmdata = subset(bgmdata, !(bgmdata$participant_ID==17952 | bgmdata$participant_ID==19101 |
#                            bgmdata$participant_ID==19198 | bgmdata$participant_ID==19210))

subject_IDs <- unique(bgmdata$participant_ID)

# take only the relevant pieces of data from the original/main data
bgmdata <- bgmdata[c("rt","shapeOrganizationNumber", "cueType","ellipse1_move_direction",
               "ellipse2_move_direction", "selected_ellipse_logAR", "cued_ellipse_logAR" ,
                "uncued_ellipse_logAR","coherence_level", "cuedMotionDirection","unCuedMotionDirection", 
                "differenceBetweenCuedAndReported", "trial_num","round_num","participant_ID")]

# change column names
colnames(bgmdata) <- c("rt", "shape_org", "cueType", "e1_motion_dir", "e2_motion_dir", "responseAR","cuedAR", "uncuedAR", "coherence", "cued_motion_dir", "uncued_motion_dir", "response_error", "trial_number", "round_number", "sub")

# convert characters into numeric
bgmdata <- mutate_all(bgmdata, function(x) as.numeric(as.character(x)))

# what is the difference between uncued and cued?
bgmdata$arDiff <- bgmdata$uncuedAR - bgmdata$cuedAR

# what is the pair average? Pair Average can be used as an index for perceptual averaging (DV)
# because it can tell how much the response were closer to the pair average
bgmdata$pairAvg <- (bgmdata$uncuedAR + bgmdata$cuedAR)/2

# what is the motion direction of the cued shape? Vertical:1 vs Horizontal:0 vs Random:2
bgmdata$cuedMotDir1V0H2R <- ifelse(bgmdata$cued_motion_dir == 90 | bgmdata$cued_motion_dir == 270, "1", ifelse(bgmdata$cued_motion_dir == 0 | bgmdata$cued_motion_dir == 180, "0", "2"))

# is the pair organized vertically (within) or horizontally (across)? [shape_org]: 1 & 2 (horiz) or 3 & 4 (verti) 
# 13.August.2021: double checked: 1 horizontal-top, 2: horizontal-bottom; 3: vertical-left, 4: vertical-right
bgmdata$global_org1W0B <- ifelse(bgmdata$shape_org == 3 | bgmdata$shape_org == 4, 1, 0)

# what is the shared-motion-ness? CAUTION: This takes the random motion as Same. We need to seperate out the random motion. 
bgmdata$sameDirection1S0D <- ifelse(bgmdata$cued_motion_dir == bgmdata$uncued_motion_dir, 1, 0)
bgmdata$sameDirection1S0D_R2 <- ifelse(bgmdata$sameDirection1S0D == 1 & bgmdata$cued_motion_dir == 9999, 2, bgmdata$sameDirection1S0D)

# Are the two shapes in the same category of aspect ratio? i.e., both tall?
bgmdata$pairSameARCat <- ifelse(bgmdata$cuedAR < -0.00000000  & bgmdata$uncuedAR < -0.00000000 | bgmdata$cuedAR > -0.00000000 & bgmdata$uncuedAR > -0.00000000 | bgmdata$cuedAR == bgmdata$uncuedAR, "1", "0")

# finding the trials where pairs have the same AR
bgmdata$identicalShapesI1D0 <- ifelse(bgmdata$cuedAR == bgmdata$uncuedAR, 1, 0) 

#define trials where motion was coherent (1) or random (0)
bgmdata$randomTrialsR1C0 <- ifelse(bgmdata$cued_motion_dir == 9999, 1, 0) 

#define trials where the motion is diagonal (1) or not (transverse; 0)
bgmdata$motionType1Diag0Trans <- ifelse(abs(bgmdata$e1_motion_dir - bgmdata$e2_motion_dir) == 90 | 
                                       abs(bgmdata$e1_motion_dir - bgmdata$e2_motion_dir) == 180, 0, 1)
# output the csv externally 
write.csv(bgmdata, "bgmdata.csv", row.names = FALSE)
