setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot7', 'functions.R'))

rootPath <- "https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot7/survey/"
tripDfPath <- paste(rootPath, 'tripDfListNoCar.csv', sep = '')
respondentID <- 7
plotFuncsPath <- paste(rootPath, "plotFuncs.R", sep='')

source(plotFuncsPath)
doe <- fread(tripDfPath)[respID == respondentID]
# Filter out the alternatives
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

makePlot(trip2)

