# source the analysis code
source('2-need-for-cognition.R')
source('fig_setup.R') # Change this file for general figure settings/ggplot theme/etc

p1 <- ggplot(Var, aes(x = needForCognition, y = meanFid)) + geom_point(size = 3, shape = 21, color = "grey") + geom_smooth(method = 'lm', color = "black", size = 1.3, linetype = 2) +ggtitle("Need for Cognition & Performance") +ylab("Mean Fidelity") +theme(aspect.ratio=1) +xlab("Need for Cognition")

p2 <- ggplot(Var, aes(x = flow, y = meanFid)) + geom_point(size = 3, shape = 21, color = "grey") + geom_smooth(method = 'lm', color = "black", size = 1.3, linetype = 2) +ggtitle("Flow & Performance") +ylab("Mean Fidelity") +theme(aspect.ratio = 1) +xlab("Flow")

p <- p1 + p2
ggsave('../figures/fig4.png', w = 7, h = 4)