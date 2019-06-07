library(data.table)


cat("\nCreates a dataset d which merges the questionnaire and the game performance data.\n")

flow_preprocessing <- function()
{
    ddir <- "../4-Data"
    
    # Data Cleaning is done in the script by PH
    dFlow <- fread(file.path(ddir,"clean_data_questionnaire_uva.csv"))
    dGame <- fread(file.path(ddir,"clean_data_uva.csv"))
    dGame[, levelName := factor(levelName, levels=c(
        "QuantumThoughtsQMCogScileveL1",
        "QuantumThoughtsQMCogSciLeve1.2.New",
        "QuantumThoughtsQMCogSciLevel2.2",
        "QuantumThoughtsQMCogSciWiggle"),
        labels = paste("Level", 1:4), ordered = TRUE)]
    dFlow[, InputDevice := factor(InputDevice, labels = c("Mouse","Touchpad","Touchscreen","Other"), levels = 1:4)]
    setkey(dFlow, email)
    setkey(dGame, email)
    d <- dGame[dFlow]
    rm(dFlow,dGame,ddir)  
    
    return(d[])  
}

