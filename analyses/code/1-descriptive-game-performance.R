# -----------------------------------------------------------------------------
# analyse data from COBE lab experiment
# Author: Jana B. Jarcki
# N = 98 people after cleaning
# see clean_data.R for how the data was cleaned
# -----------------------------------------------------------------------------
library(data.table) # fastest package to load data
library(multcomp) # Statistical modeling
library(lme4) # Stat. modeling: GLM/hierarchical linear models
library(lmerTest) #    ""
library(afex) #    ""


#load data
qdata <- fread("../../data/processed/gameplay.csv") # Qurestionnaire
gdata <- fread("../../data/processed/questionnaire.csv") # Game play
gdata[, createdat := as.POSIXct(createdat)] # make time-format


# -----------------------------------------------------------------------------
# Test if level 4 was more difficult in terms of Number of attempts
attempts_perLevel_perID <- gdata[, list(lastAttempt = max(attempts)), by = list(ID,levelName)]
anova <- aov(lastAttempt ~ levelName, attempts_perLevel_perID) # 1-way ANOVA
summary(anova)
TukeyHSD(anova, conf.level=0.95) # all higher levels need more trials than the previous, except L1 compared to L2
# We can also analyze this with an LM, taking the repeated measures into acoutn (violations of sphericity)
# library(glm)
# result <- lmer(lastAttempt ~ levelName + (1|ID), attempts_perLevel_perID)
# summary(result)


# -----------------------------------------------------------------------------
# Performance in the critical 4th level 4 of the game
# Overall successes in level 3 and 4
gdata[levelName %in% c("L3","L4"),
  .(
    num_suc = sum(lastPointFidelity > 0.5),
    num = .N,
    per_suc = percent(sum(lastPointFidelity > 0.5)/.N)
  ),
  by = levelName]
# 3:        L3     910 6033   15.1%
# 4:        L4     651 8015   8.12%

# People that reached the learning criterion in level 3 and level 4
success_by_individual <- gdata[levelName %in% c("L3","L4"),
  .(three_suc = as.numeric(any(currentCompletions == 3))),
  by = "ID,levelName"]
success_by_individual[, .(
    num_suc = sum(three_suc),
    num     = .N,
    per_suc = percent(sum(three_suc)/.N)), by = levelName]
#    levelName num_suc num per_suc
# 1:        L3      68  97   70.1%
# 2:        L4      35  97   36.1%

# Generalized linear modeling (regression)
result <- glmer(three_suc ~ levelName + (1 | ID), data = success_by_individual, family = binomial)
summary(result)
summary(glht(result, linfct = mcp(levelName = "Tukey")))
coef(result)
#  mixed(success ~ levelName + (1 | ID), data = success_by_individual, family = binomial, method = "PB")

# How fast people could make it in level 4
time_per_success <- gdata[levelName == "L4", 
  .(suc = as.numeric(lastPointFidelity > 0.5), totalTime = totalTime, lastPointFidelity = lastPointFidelity),
  by = ID]
time_per_success[,
  round(describe(totalTime), 2)[to_report],
  by = suc]
#    suc mean median   sd  min   max
# 1:   0 4.90   4.50 2.96 0.08 10.02
# 2:   1 8.47   8.71 1.16 4.41 10.0
