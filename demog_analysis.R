
d_demog = read.csv('demographics-bgm.csv', header=TRUE) ## same-different response test run without random motion and now it is just L-R
#

#### 0- demog analysis ####

#find the people who both filled the survey and completed the bgm experiment
idBgm<-d$participant_ID #this is the unique id numbers on qualtrics
idQualtrics<-d_demog$id#this is the unique id numbers on bgm
sharedIds<-Reduce(intersect, list(idBgm, idQualtrics)) #get the shared id numbers

#get only the shared IDs from the demographic list 
d_demog_filteredByID<-filter(d_demog, id %in% sharedIds)

#change some column names
colnames(d_demog_filteredByID)[which(names(d_demog_filteredByID) == "Q4")] <- "age"
colnames(d_demog_filteredByID)[which(names(d_demog_filteredByID) == "Q5")] <- "gender"

#Deleting rows that are duplicated in one column based on the conditions of another column
d_demog_filteredByID<- d_demog_filteredByID[!duplicated(d_demog_filteredByID$id),]

#removing genders of 17952, 19101, 19198, 19210 because they appear to do the task but they didn't
#remove them from the demog list
d_demog_filteredByID<- d_demog_filteredByID[-c(which(d_demog_filteredByID$id==17952), 
                                               which(d_demog_filteredByID$id==19101),
                                               which(d_demog_filteredByID$id==19198),
                                               which(d_demog_filteredByID$id==19210)),]
#get the age range
ageRange <- na.omit(as.numeric(d_demog_filteredByID$age))
range(ageRange)
#get the gender count
genderTable <- as.data.frame(table(d_demog_filteredByID$gender))#count unique strings
genderTable #


