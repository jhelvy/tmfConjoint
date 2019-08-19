setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot3', 'functions.R'))

# -----------------------------------------------------------------------------

# Load initial formr variables
rootPath <- "https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot3/"
doePath <- paste(rootPath, "doe/doe.csv", sep="")
maxResp <- 6000
respondentID <- 1
functionsPath <- paste(rootPath, "survey/functions.R", sep='')
respTripsPath <- paste(rootPath, "doe/trips/", respondentID, ".csv", sep="")

# Example question
source(functionsPath)
doe <- fread(respTripsPath)

# Filter out the alternatives
trip1 <- doe[(altID == 1) & (qID == 2)]
trip2 <- doe[(altID == 2) & (qID == 2)]
trip3 <- doe[(altID == 3) & (qID == 2)]

makePlot(trip1)
makePlot(trip2)
makePlot(trip3)



makePlot(trip  <- as.data.table(getTripDf(doe[4,])))
