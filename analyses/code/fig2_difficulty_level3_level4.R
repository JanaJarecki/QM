source("fig_setup.R")
gdata <- fread("../../data/processed/gameplay.csv")

# People that reached the learning criterion in level 3 and level 4
success_by_individual <- gdata[levelName %in% c("L3","L4"),
  .(three_suc = as.numeric(any(currentCompletions == 3))),
  by = "ID,levelName"]

ggplot(success_by_individual[, .N, by = list(levelName,three_suc)], aes(x=levelName, y=N, fill = as.factor(three_suc))) +geom_bar(stat = "identity", position = "dodge", width = .6, color = "black") +scale_fill_manual(values = c("black", "white"), "Performance criterion reached", labels = c("NO", "YES")) +theme(legend.position ="top") +xlab("Level") +ggtitle("Performance in Levels 3 and 4") +scale_y_continuous("N of 97", expand = c(0,0), limits = c(0,70))

ggsave("../figures/fig2_difficulty_level3_level4.png", w = 4, h = 4)