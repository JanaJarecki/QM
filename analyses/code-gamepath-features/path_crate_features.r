### Pinja Haikka, Jana Jarecki
### 17/07/2017
### Feature extraction of Quantum Minds Level 4 paths
### Dataset of COBE lab participants
### Feature extraction loosely based on "Revealing the physics of movement" by Dodge et al.

library(data.table) # speeds up reading the data

### CHANGE THESE
setwd(getwd()) #"~/Dropbox/QuantumMinds_COBE_2017/Path analysis")
ddir <- "../4-Data" # data directory

# raw path data, formatted from JSON to csv in MatLab script 'json2csv—pdata.m'
# this part takes some time, be patient
pdata <- fread(file.path(ddir,'lab_pdata.csv'), header = FALSE)
setnames(pdata, c('hitID', 'createdAt', 'userID', 'levelName', 'lastPointFidelity', 'totalTime', 'physicsX', 'physicsAmp', 'fidelity'))
# list of COBE participants who completed the whole experiment (two surveys and gameplay), created in script 'clean_data.R'
load('clean_participants.Rda')

#im <- readPNG('wiggle_orto.png') # image of game window
jana_features <- fread(file.path(ddir,'lab_fdata_relangle.csv'))

# uncommented these lines because the data has correct format for me
# jana_features[, 2:5] <- sapply(jana_features[, 2:5], as.character)
# jana_features[, 2:5] <- sapply(jana_features[, 2:5], as.numeric)

### clean data
ol <- c("QuantumThoughtsQMCogScileveL1", "QuantumThoughtsQMCogSciLeve1.2.New", "QuantumThoughtsQMCogSciLevel2.2", "QuantumThoughtsQMCogSciWiggle")
pdata[, levelName := factor(levelName, levels=ol, labels=paste0("L",1:4))]
# levels(pdata$levelName) <- c("", "L2", "L3", "L4", "L1")  # rename levels
# pdata$levelName <- factor(pdata$levelName, levels(pdata$levelName)[order(levels(pdata$levelName))]) # order levels from L1...L4
# pdata$levelName <- reorder(pdata$levelName, order(pdata$levelName))

# filter by clean participants
pdata <- filter(pdata, userID %in% participants) # ~ 6 million hits
L4pdata <- filter(pdata, levelName == 'L4') # extract Level 4 (L4) data for further analysis
length(unique(L4pdata$hitID)) # 8125 hits for L4
length(unique(L4pdata$userID)) # 98 unique users

### GLOBAL DESCRIPTORS (VELOCITY AND ACCELERATION, MEAN AND SD, IN X AND A DIRECTIONS)
global <- L4pdata %>% group_by(hitID) %>% summarise(mean_vel_x = mean(diff(physicsX)), # mean velocity in x direction
                                                    mean_vel_A = mean(diff(physicsAmp)), # mean velocity in y direction (in amplitude)
                                                    var_vel_x = var(diff(physicsX)), # variation in velocity in x direction
                                                    var_vel_A = var(diff(physicsAmp)), # variation in velocity in y direction
                                                    mean_acc_x = mean(diff(physicsX, difference = 2)), # mean acceleration in x direction
                                                    mean_acc_A = mean(diff(physicsAmp, difference = 2)), # mean acceleration in y direction
                                                    var_acc_x = var(diff(physicsX, difference = 2)), # variation in acceleration in x direction
                                                    var_acc_A = var(diff(physicsAmp, difference = 2))) #variation in acceleration in y direction
global <- merge(global, jana_features)

### include success variable
success <- L4pdata %>% group_by(hitID) %>% summarise(fidelity = last(lastPointFidelity))
DF <- merge(global, success) # attach success to features

### save the df
save(DF, file="df4ml.Rda")