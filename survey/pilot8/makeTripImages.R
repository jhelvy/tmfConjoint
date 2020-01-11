# setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot8', 'functions.R'))

# Read in DOEs
doeYes <- read_csv(here::here('survey', 'pilot8', 'survey', 'doeCarYes.csv'))
doeNo <- read_csv(here::here('survey', 'pilot8', 'survey', 'doeCarNo.csv'))

# Create all trip images
saveRoot <- '/Users/jhelvy/sync/00_projects/TMF/survey'
# for (i in seq(max(tripDfsYes$respID))) {
for (i in seq(3)) {
    respDoeYes <- filter(doeYes, respID == i)
    respDoeNo <- filter(doeNo, respID == i)
    for (j in seq(max(respDoeYes$qID))) {
        plotNames <- str_c(j, '-', c(1,2,3), '.png')
        respRootYes <- paste(saveRoot, 'imagesCarYes', i, sep = '/')
        respRootNo <- paste(saveRoot, 'imagesCarNo', i, sep = '/')
        dir.create(respRootYes)
        dir.create(respRootNo)
        pathsYes <- paste(respRootYes, plotNames, sep='/')
        pathsNo <- paste(respRootNo, plotNames, sep='/')
        tripDf1Yes <- getTripDf(filter(respDoeYes, altID == 1, qID == j))
        tripDf2Yes <- getTripDf(filter(respDoeYes, altID == 2, qID == j))
        tripDf3Yes <- getTripDf(filter(respDoeYes, altID == 3, qID == j))
        tripDf1No <- getTripDf(filter(respDoeNo, altID == 1, qID == j))
        tripDf2No <- getTripDf(filter(respDoeNo, altID == 2, qID == j))
        tripDf3No <- getTripDf(filter(respDoeNo, altID == 3, qID == j))
        trip1Yes <- makePlot(tripDf1Yes)
        trip2Yes <- makePlot(tripDf2Yes)
        trip3Yes <- makePlot(tripDf3Yes)
        trip1No <- makePlot(tripDf1No)
        trip2No <- makePlot(tripDf2No)
        trip3No <- makePlot(tripDf3No)
        ggsave(pathsYes[1], trip1Yes, width=2.5, height=5.5, dpi = 100)
        ggsave(pathsYes[2], trip2Yes, width=2.5, height=5.5, dpi = 100)
        ggsave(pathsYes[3], trip3Yes, width=2.5, height=5.5, dpi = 100)
        ggsave(pathsNo[1], trip1No, width=2.5, height=5.5, dpi = 100)
        ggsave(pathsNo[2], trip2No, width=2.5, height=5.5, dpi = 100)
        ggsave(pathsNo[3], trip3No, width=2.5, height=5.5, dpi = 100)
    }
}
