# -----------------------------------------------------------------------------
# Demographics
# -----------------------------------------------------------------------------
library(data.table)
devtools::install_github("janajarecki/cogsciutils") # participants() function
library(cogsciutils)

#load data
qdata <- fread("../../data/processed/questionnaire.csv")
qdata[, gender := factor(gender, labels = c("female", "male"))]
# add start and end dates
qdata[, datetime := "2017-05-30 00:00:00"]
qdata[2, datetime := "2017-06-02 00:00:00"]

# print demographics
participants(qdata, id = "ID", age = "age", gender = "gender", excl = 28, collectedat = "the COBE laboratory (Aarhus, DK)", approvedby = "the ethics comittee at Aarhus University", date = "datetime", more = list("number computer games played weekly was" = c("playesGamesPerWeek", "hour")))
# In total 97 participants completed the study (zero were excluded), 51 females and 46 males (53% and 47%, respectively), mean age 25 years (med = 24, sd = 6, range 18-74 years), data were collected at the COBE laboratory (Aarhus, DK) from May to June 2017, the study was approved by the ethics comittee at Aarhus University. Mean number computer games played weekly was 3.2 hours (med = 3.0, sd = 1.8, range 1.0-7.0 hours; 69 missing). 

# Exclusion for the following reasons:
# missing data, failing to take part in the game, aborting the study.