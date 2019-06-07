# -----------------------------------------------------------------------------
# Manuscript Section: Cognitive Abilities vs. Motivational Factors as Drivers behind Human Performance
# Author: Jana B. Jarcki
# See 0-clean_data.R for how the data was cleaned/preprocessed
# -----------------------------------------------------------------------------
library(data.table) # fastest package to load data
library(multcomp) # Statistical modeling
library(lme4) # Stat. modeling: GLM/hierarchical linear models
library(lmerTest) #    ""
library(afex) #    ""


#load data
qdata <- fread("../../data/processed/gameplay.csv") # Questionnaire
gdata <- fread("../../data/processed/questionnaire.csv") # Game play

# -----------------------------------------------------------------------------
# Aggregate data
# 1. Independent variables: Agregate the survey data
indepVar <- qdata[!duplicated(ID), list( 
  needForCognition = as.numeric(mean(c(nfc_1, nfc_2, 6-nfc_3R, 6-nfc_4R, 6-nfc_5R, nfc_6, 6-nfc_7R, 6-nfc_8R, 6-nfc_9R, nfc_10, nfc_11, 6-nfc_12R, nfc_13, nfc_14, nfc_15, 6-nfc_16R, 6- nfc_17R, nfc_18))), 
  flow = mean(c(flow1, flow2, flow3, flow4, flow5, flow6, flow7, flow8, flow9)),
  extraversion = mean(c( 6 - big5_1R_extraversion, big5_6_extraversion)), 
  agreeableness = mean(c( big5_2_agreeableness, 6 - big5_7R_agreeableness)), 
  conscientiousness = mean(c( 6 - big_3R_conscientiouseness, big5_8_conscientiousness)), 
  neuroticism = mean(c( 6 - big5_4R_neuroticism, big5_9_neuroticism)), 
  openness = mean(c( 6 - big5_5R_openness, big5_10_openness)),
  playesGamesPerWeek = as.numeric(ifelse(is.na(playesGamesPerWeek), 0, playesGamesPerWeek))), 
  by = ID]
# Dependent variable: Aggregate performance data
depVar <- gdata[levelName == 'L4', 
                .(attemps = .N,
                  meanFid = mean(lastPointFidelity),
                  bestFid = max(lastPointFidelity),
                  successRate = sum(lastPointFidelity >= 0.5)/.N),
                by = ID]
# Merge them together
Var <- merge(depVar, indepVar, by = "ID")


# -----------------------------------------------------------------------------
# Run some Checks: OK
# Distribution of independent variables
# Need for cognition
# hist(indepVar$needForCognition, 25) # Single-peaked
# describe(indepVar$needForCognition) # pretty Gaussian, mean 3.59
# range(indepVar$needForCognition) # 2.3 to 4.8
# # Flow
# hist(indepVar$flow, 25) # Double-peaked, maybe needs transformation?
# describe(indepVar$flow) # Seems also pretty Gaussian, mean 3.38


# -----------------------------------------------------------------------------
# Inter-correlation between flow and need for cognition
apa_print(cor.test(indepVar$flow, indepVar$needForCognition))

# Statistical tests for the flow and NFC hypotheses
reg <- lm(meanFid ~ needForCognition + flow + playesGamesPerWeek, data = Var)
summary(reg)
