###############################################################################
# Table 1
# Author: Jana B. Jarcki
# N = 98 people after cleaning
# see clean_data.R for how the data was cleaned
###############################################################################
library(data.table)
library(psych)

gdata <- fread("../../data/preprocessed/questionnaire.csv") # Game play

# Descriptive statistics by level (levelName)
# Fidelity by level
f <- gdata[, round(describe(lastPointFidelity), 3), by = levelName]
# Number of attempts/trials by level
a <- gdata[, max(attempts), by = .(ID, levelName)][, round(describe(V1), 3), by = levelName]
# Table
tab <- cbind(a,f)[, c('levelName', 'mean', 'median', 'sd', 'min', 'max', 'mean', 'median', 'sd', 'min', 'max')]
print(tab, digits = 3)