# Exploratory Analysos
# Correlates with personality variaobles

# This creates a dataset "Var"
source('2-need-for-cognition-and-flow.R')
head(Var)



# Correlations to test personality effects
source("fig_setup.R") # images, exploratory
per1 <- ggplot(Var, aes(x = openness, y = meanFid)) + geom_point() + geom_smooth(method = 'lm')
summary(lm(needForCognition ~ meanFid, Var))

per2 <- ggplot(Var, aes(x = extraversion, y = meanFid)) + geom_point() + geom_smooth(method = 'lm')
summary(lm(needForCognition ~ meanFid, Var))

per3 <- ggplot(Var, aes(x = conscientiousness, y = meanFid)) + geom_point() + geom_smooth(method = 'lm')
summary(lm(needForCognition ~ meanFid, Var))

per4 <- ggplot(Var, aes(x = agreeableness, y = meanFid)) + geom_point() + geom_smooth(method = 'lm')
summary(lm(needForCognition ~ meanFid, Var))

per5 <- ggplot(Var, aes(x = neuroticism, y = meanFid)) + geom_point() + geom_smooth(method = 'lm')
summary(lm(needForCognition ~ meanFid, Var))

per1 + per2 + per3 + per4 + per5
