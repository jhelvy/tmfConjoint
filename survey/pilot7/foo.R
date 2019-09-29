setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot7', 'functions.R'))

# -----------------------------------------------------------------------------

rootPath <- "https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot7/survey/"
if (hasCar == 1) {
    tripDfPath <- paste(rootPath, 'tripDfsAll.csv', sep = '')
} else {
    tripDfPath <- paste(rootPath, 'tripDfListNoCar.csv', sep = '')
}
respID <- sample(seq(max(fread(tripDfPath)$respID)), 1)
plotFuncsPath <- paste(rootPath, "plotFuncs.R", sep='')

# Filter out the trips
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

source(here::here('survey', 'pilot7', 'survey', 'plotFuncs.R'))

makePlot(trip2)

