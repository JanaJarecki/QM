#### clean data from COBE lab experiment
#### 62 people on Monday 2017-05-29 (39 people completed the study)
#### 30 people on Thursday 2017-06-01
#### 34 on Friday 2017-06-02 (one person did not complete the study)
#### total of 126 (new:  98?) participants

### Change the global variables at the beginning!

library(dplyr)
library(ggplot2)
library(fasttime) # this is faster than parsedate
library(lubridate)
library(stringr)
library(data.table)

# Global variables
setwd("") # CHANGE THIS to your current directory
dat.dir <- c("../../data/raw") #  relative path

# load data
gdata <- fread(file.path(dat.dir, 'lab_game.csv')) # data since 2017-05-29T00:00:00.736Z (constaint set in query for this dataset
nastrings <- c("NA","-99","-66", "-77") # these are missing values
qdata1 <- fread(file.path(dat.dir, 'lab_study1.csv'), na.strings = nastrings) # not time-constained
qdata2 <- fread(file.path(dat.dir, 'lab_study2.csv'), na.strings = nastrings) # not time-constained

# set keys for faster processing
setkey(gdata, userID)
setkey(qdata1, ID)
setkey(qdata2, ID)

# clean variables and variable names
setnames(gdata, gsub("_", "", names(gdata))) # clean column names
qdata1[, ID := str_trim(qdata1$ID)] # clear whitespecaes
ol <- c("QuantumThoughtsQMCogScileveL1",
    "QuantumThoughtsQMCogSciLeve1.2.New",
    "QuantumThoughtsQMCogSciLevel2.2",
    "QuantumThoughtsQMCogSciWiggle" ) # game level names in order
gdata[, levelName := factor(levelName, levels=ol, labels=paste0("L",1:4))] # rename levels
setnames(qdata2, c("v_47", "flow8", "MoceThroughPoints"), c("flow8", "flow9", "MoveThroughPoints")) #rename one variable
gdata[, createdat := fastPOSIXct(createdat)] # parse dates
qdata1[, datetime := fastPOSIXct(datetime)]
qdata2[, datetime := fastPOSIXct(datetime)]

# keep only data from 29.05., 01.06., 02.06.
days_to_keep <- date(c("2017-05-29", "2017-06-01", "2017-06-02"))
gdata  <-  gdata[date(createdat) %in% days_to_keep]
qdata1 <- qdata1[date(datetime) %in% days_to_keep] # 136 people
qdata2 <- qdata2[date(datetime) %in% days_to_keep] # 103 people

# exclude tester ID and one ID with only missing values in survey 1
# was the only-missings-person a subject in our lab??
tester_ids <- c("DESKTOP-PN172OA_1545152017", "YLGW028110_1625562017")
gdata  <-  gdata[!userID %in% tester_ids]
qdata1 <- qdata1[!ID %in% tester_ids] # 135 people
qdata2 <- qdata2[!ID %in% tester_ids] # 103 people (still)

# keep only columns we will analyze later
cols_to_keep1 <- c("ID","datetime","lastpage","nfc_1","nfc_2","nfc_3R","nfc_4R","nfc_5R","nfc_6","nfc_7R","nfc_8R","nfc_9R","nfc_10","nfc_11","nfc_12R","nfc_13","nfc_14","nfc_15","nfc_16R","nfc_17R","nfc_18","big5_1R_extraversion","big5_2_agreeableness","big_3R_conscientiouseness","big5_4R_neuroticism","big5_5R_openness","big5_6_extraversion","big5_7R_agreeableness","big5_8_conscientiousness","big5_9_neuroticism","big5_10_openness",
    "check1","check2","check3","check4","check5","check6","check7",
    "page_history") # columns to keep
qdata1 <- qdata1[, cols_to_keep1, with=FALSE]
cols_to_keep2 <- c("ID", "datetime", "lastpage","flow1","flow2","flow3","flow4","flow5","flow6","flow7","flow8","flow9","waveStable","makeInTime","releaseCursor","planMovement","executeMotion", "avoidObstacleL4", "avoidObstacleL3", "learnedOther", "learnedOtherTxt", "passL4", "aspectsChanged","MoveThroughPoints", "speedCrucial", "learnedControlWave", "gender", "genderOther", "age", "playesGames", "playesGamesPerWeek","subjectOfStudy","handedness","handednessOther") # columns to keep
qdata2 <- qdata2[, cols_to_keep2, with=FALSE]

# match IDs
idg <- as.character(unique(gdata$userID))
id1 <- as.character(unique(qdata1$ID))
id2 <- as.character(unique(qdata2$ID))
bothSurveys <- intersect(id1, id2) # 103 people
fullStudy <- intersect(idg, bothSurveys) # 103 people

# check which participants aborted the study (= survey 1)
ida <- qdata1[lastpage != 2177820, ID] # started s1, didnt finish: 7
idb <- qdata1[ID %in% setdiff(id1,idg), ID] # started s1, no gamedata: 7
idc <- qdata2[lastpage != 2187671, ID] # started s2, but didnt finish: 1
intersect(ida, idb) # all
intersect(ida, idc) # none
qdata1 <- qdata1[!ID %in% ida] # 7 IDs did not finish the survey 1, exclude them
id1 <- as.character(unique(qdata1$ID))
# this makes 127 people, what happened to the remaining 24 people who are in survey 1 but not in fullStudy
idd <- setdiff(id1,fullStudy) # 24 people
# their data looks legit
qdata1[ID %in% idd]

# Should we report that we excluded 8 people because of missing values and 24 people because they did not play the game. Does this make sense?



# Keep only full study data
gdata <- gdata[userID %in% fullStudy]
qdata1 <- qdata1[ID %in% fullStudy]
qdata2 <- qdata2[ID %in% fullStudy]


# sanity checks
table(as.Date(qdata1$datetime))
table(as.Date(qdata2$datetime))
# 2017-05-29 2017-06-01 2017-06-02 
#         40         30         33
# one extra on Monday (testing?!), four tests on 31st

# check the one extra for monday
mondayData <- filter(gdata, createdat < "2017-05-30 00:00:00 CEST")
mondayParticipants2 <- as.character(unique(mondayData$userID))
load('mondayParticipants.Rda') # from mon_analysis.R
setdiff(mondayParticipants2, mondayParticipants) # "YLGW036460_1346292017"
checkg <- filter(gdata, userID == "YLGW036460_1346292017") # played level 4 over 10 times
check2 <- filter(qdata2, ID == "YLGW036460_1346292017") # but has filled in survey 2
check1 <- filter(qdata1, ID == "YLGW036460_1346292017") # KEEP THIS PERSON?!

# Disregard the person, because only NAs in survey 2
gdata  <-  gdata[userID != "YLGW036460_1346292017"]
qdata1 <- qdata1[ID != "YLGW036460_1346292017"]
qdata2 <- qdata2[ID != "YLGW036460_1346292017"]

# check for "hackers"
trials_per_level_per_user <- gdata %>% group_by(userID, levelName) %>% summarise(count = n())
attempts_per_level_per_user <- gdata %>% group_by(userID, levelName) %>% summarise(lastAttempt = last(attempts))
check <- merge(trials_per_level_per_user, attempts_per_level_per_user)
filter(check, count != lastAttempt) # 37 participants with some data missing
table(check$lastAttempt-check$count) # in most cases there are only a few points missing

# identify and remove players who have more than 3 missing datapoints >>> 98 players
hackers <- filter(check, lastAttempt - count > 3) %>% select(userID) # 5 people fit this criterion
gdata  <-  gdata[!(userID %in% hackers$userID)]
qdata1 <- qdata1[!(ID %in% hackers$userID)]
qdata2 <- qdata2[!(ID %in% hackers$userID)] # 98 participants

# Check for people playing more than 100 times
trials_per_level_per_user %>% group_by(levelName) %>% summarize(sum(count > 100)) # 13 in level 4
attempts_per_level_per_user %>% group_by(levelName) %>% summarize(sum(lastAttempt > 100)) # 14 in level 4
# SHOULD WE REMOVE THE attempts that go beyond 100?


# Merge data from questionnaires
qdata <- merge(qdata1, qdata2, by = "ID", suffix = c("","2"))

# Different file or deletion of ensitive data
# Data not to be published by law, maybe we have to add gender and age here too if we need to be conservative
sensitive_vars <- c("datetime", "datetime2")
qdata <- qdata[, !sensitive_vars, with = F]

# Transform numeric columns into numeris format
txtCols <- c("handednessOther","handednessOther","subjectOfStudy","MoveThroughPoints","aspectsChanged","page_history","datetime","ID","learnedOther","genderOther","learnedOtherTxt")
numCols <- setdiff(names(qdata), txtCols) # columns that have numeric formats
qdata[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]

# ID to use in analysis
participants <- qdata$ID

# Rename userID to ID in gdata
setnames(gdata, "userID", "ID")

# save the clean data
# store it as csv because we may want to publish them plattform-independently and Rda files are R-dependent
write.table(gdata, file.path(dat.dir,'preprocessed/gameplay.csv'), sep=";", row.names = F)
write.table(qdata, file.path(dat.dir,'processed/questionnaire.csv'), sep=";", row.names = F)
write.table(participants, file.path(dat.dir,"processed/participants.csv"), sep=";", row.names = F)