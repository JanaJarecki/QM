source("fig_setup.R")

qdata <- fread("../../data/processed/gameplay.csv")

# Plot density of duration of hits for successful and unsuccessful attempts
time_per_fidelity  <- gdata[levelName == "L4", .(totalTime = totalTime), by = list(Fidelity = cut(highestFidelity, seq(0,1,.1), include.lowest = TRUE, right = FALSE), ID)]

ggplot(time_per_fidelity, aes(x=Fidelity, y=totalTime)) +geom_boxplot(aes(fill = as.numeric(Fidelity) > 5.5),  width = .5) +scale_y_continuous("Time [sec]", expand = c(0,0)) +geom_vline(xintercept = 5.5, linetype = 2) +scale_fill_manual("Fidelity greater/equal .50", values = c("white", "grey"), labels = c("NO", "YES")) +theme(legend.position =  "top") +ggtitle("Time needed to achieve a good solution in Level 4")

ggsave("../figures/fig3_speed_human_participants.png", h = 4, w = 7)

