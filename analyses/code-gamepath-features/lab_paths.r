library(data.table)
ddir <- "../4-Data"


# Load data
pdata <- fread(file.path(ddir,"lab_pdata.csv"))
setnames(pdata, c('hitID', 'createdAt', 'userID', 'levelName', 'lastPointFidelity', 'totalTime', 'physicsX', 'physicsAmp', 'fidelity'))
participants <- fread(file.path(ddir,"clean_participants.csv"))$x
pdata <- pdata[userID %in% participants]

