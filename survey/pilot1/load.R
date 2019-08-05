# Load the full design of experiment
library(data.table)
doe <- fread('https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot1/doe.csv')

# Recode repsondent ID if it's greater than the max(doe$respID)
respondentID <- 1
if (respondentID > max(doe$respID)) {
    respondentID <- respondentID %% max(doe$respID)
}

# Load the functions to create the images