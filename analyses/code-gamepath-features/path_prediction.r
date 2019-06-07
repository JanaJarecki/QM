### Jana Jarecki
### 17. July 2017
### Predicting whether individual pass Quantum Minds Level 4
### from the change in features in the first half of Level 4

library(data.table) # fast data loading and manipulation
library(usdm) # checking for multicollinearity

# CHANGE THESE
setwd("C:\\Users\\Jana Jarecki\\Google Drive\\QG\\5-Code")
ddir <- "../4-Data" # replace by "" if data is in same folder as code

# Load data
participants <- fread(file.path(ddir,"clean_participants.csv"))$x
pdata <- fread(file.path(ddir,"lab_pdata.csv"), select = 1:4, col.names =  c('hitID', 'createdAt', "ID", "levelName")) # pathIDs w/ timestamps
fdata <- fread(file.path(ddir, "lab_features.csv")) # features
gdata <- fread(file.path(ddir, "clean_gdata.csv")) # game success data

# Clean data, create variables
pdata <- pdata[ID %in% participants & levelName == "QuantumThoughtsQMCogSciWiggle"] # only L4
pdata <- unique(pdata) # only 1 data point per hitID

# Merge path timestamps and userIDs with features
check <- all(pdata[, sort(hitID)] == fdata[, sort(hitID)])
check # TRUE
fdata <- merge(pdata, fdata, by = "hitID") # 8015 rows

# New variable
setkey(fdata, createdAt) # order by time
fdata[, hitNr := 1:.N, by = ID] # counts from 1 to N per user

# Define training data as 50 % of trials in L4
fdata[, training := ifelse(hitNr <= 0.5 * .N, 1, 0), by = ID]

# Improvement in the features with linear regression
beta_byID_byFeature <- fdata[training == 1, list(
    b_mean_vel_A = coef(lm(mean_vel_A ~ hitNr))[2],
    b_mean_relangle = coef(lm(mean_relangle ~ hitNr))[2],
    b_var_acc_x = coef(lm(var_acc_x ~ hitNr))[2],
    b_mean_vel_x = coef(lm(mean_vel_x ~ hitNr))[2],
    b_mean_acc_x = coef(lm(mean_acc_x ~ hitNr))[2]), by = ID]

# Merge with the success data
success_by_individual <- gdata[levelName %in% c("L4"),
  .(three_suc = as.numeric(any(currentCompletions == 3))),
  by = "ID"]
check <- all(sort(beta_byID_byFeature[, unique(ID)]) == sort(success_by_individual[, unique(ID)]))
check # TRUE
pdata <- merge(beta_byID_byFeature, success_by_individual, by = "ID")

# Predict passing level 4 (1 or 0) using a binomial regression
# Check for multicollinearity in the predictors
predictor_names <- names(pdata)[2:6]
pdata[, cor(.SD, use = "pair"), .SD = predictor_names]
pdata[, vifcor(.SD, th = 0.4), .SD = predictor_names]
# do not use b_mean_vel_x because it is too correlated with other variables

model <- pdata[, glm(three_suc ~ b_mean_vel_A + b_mean_relangle + b_var_acc_x + b_mean_acc_x, family = "binomial")]
summary(model)
# Call:
# glm(formula = three_suc ~ b_mean_vel_A + b_mean_relangle + b_var_acc_x + 
#     b_mean_acc_x, family = "binomial")

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.3067  -0.9353  -0.8319   1.2135   2.2228  

# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)     -5.771e-01  2.255e-01  -2.560   0.0105 *
# b_mean_vel_A     1.301e+01  1.658e+01   0.785   0.4325  
# b_mean_relangle  3.434e+02  1.672e+02   2.053   0.0401 *
# b_var_acc_x     -9.780e+03  1.894e+04  -0.516   0.6057  
# b_mean_acc_x     2.421e+05  1.964e+05   1.233   0.2177  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 126.85  on 96  degrees of freedom
# Residual deviance: 119.91  on 92  degrees of freedom
# AIC: 129.91

# Number of Fisher Scoring iterations: 5

library(ggplot2)
library(themejj); theme_set(themejj()) # UNCOMMENT THIS

pdata_long <- melt(pdata, id.vars = c("ID","three_suc"), value.name = "beta", variable.name = "predictor")
pd <- position_jitterdodge()
ggplot(pdata_long, aes(x=beta, fill = factor(three_suc, labels = c("no", "yes")), color = factor(three_suc, labels = c("no", "yes")))) +geom_vline(xintercept = 0) +geom_density(alpha = .2) +facet_wrap(~predictor, scales = "free") +guides(fill = guide_legend("Success"), color = guide_legend("Success"))
ggsave(file.path("../8-Img","features_beta_L4.jpg"))