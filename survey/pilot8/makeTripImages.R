# setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot8', 'functions.R'))
source(here::here('survey', 'pilot8', 'filterCases.R'))

# Read in trip Dfs
tripDfsNo <- fread(
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarNo.csv'))
tripDfsYes <- fread(
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarYes.csv'))

imageRoot <- '/Users/jhelvy/sync/00_projects/TMF/survey'
for (i in seq(max(tripDfsYes$respID))) {
    dfYes <- tripDfsYes[respID == i]
    dfNo  <- tripDfsNo[respID == i]
    for (j in seq(max(tripDfsYes$qID))) {
        trip1Yes <- makePlot(dfYes[(altID == 1)][(qID == j)])
        trip2Yes <- makePlot(dfYes[(altID == 2)][(qID == j)])
        trip3Yes <- makePlot(dfYes[(altID == 3)][(qID == j)])
        trip1No <- makePlot(dfNo[(altID == 1)][(qID == j)])
        trip2No <- makePlot(dfNo[(altID == 2)][(qID == j)])
        trip3No <- makePlot(dfNo[(altID == 3)][(qID == j)])
        plotNames <- str_c(i, '-', j, '-', c(1,2,3), '.png')
        pathsYes <- paste(imageRoot, 'imagesCarYes', plotNames, sep='/')
        pathsNo <- paste(imageRoot, 'imagesCarNo', plotNames, sep='/')
        ggsave(pathsYes[1], trip1Yes, width=2.5, height=5)
        ggsave(pathsYes[2], trip2Yes, width=2.5, height=5)
        ggsave(pathsYes[3], trip3Yes, width=2.5, height=5)
        ggsave(pathsNo[1], trip1No, width=2.5, height=5)
        ggsave(pathsNo[2], trip2No, width=2.5, height=5)
        ggsave(pathsNo[3], trip3No, width=2.5, height=5)
    }
}
