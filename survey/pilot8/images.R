setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
library(cowplot)
source(here::here('survey', 'pilot7', 'functions.R'))

rootPath <- "https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot7/survey/"
tripDfPath <- paste(rootPath, 'tripDfListNoCar.csv', sep = '')
respondentID <- 7
plotFuncsPath <- paste(rootPath, "plotFuncs.R", sep='')

source(plotFuncsPath)

images <- list()
fullDoe <- fread(tripDfPath)
index <- 1
for (i in 1:100) {
    respondentID <- i
    trip <- fullDoe[respID == respondentID][(altID == 1) & (qID == 1)]
    if (any(str_detect(trip$label, 'Walk')) &
        any(str_detect(trip$label, 'Bus'))) {
        images[[index]] <- makePlot(trip)
        index <- index + 1
    }
}

i <- seq(20, 24)
plot_grid(
    images[[i[1]]], images[[i[2]]], images[[i[3]]], images[[i[4]]],
    nrow = 1)

ggsave(
    here::here('survey', 'pilot7', 'survey', 'images', 'tripExample.pdf'),
    images[[24]], width = 3, height = 6)

