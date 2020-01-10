setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot8', 'functions.R'))
source(here::here('survey', 'pilot8', 'filterCases.R'))

# Read in trip Dfs
tripDfsNo <- fread(
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarNo.csv'))
tripDfsYes <- fread(
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarYes.csv'))

imageRoot <- '/Users/jhelvy/sync/00_projects/TMF/survey'

i <- 1
j <- 1
dfYes <- tripDfsYes[respID == i]
dfNo  <- tripDfsNo[respID == i]

trip <- dfYes[(altID == 3)][(qID == j)]
p <- makePlot(trip)


trip3Yes <- makePlot(dfYes[(altID == 3)][(qID == j)])
plotNames <- str_c(i, '-', j, '-', c(1,2,3), '.png')
pathsYes <- paste(imageRoot, 'imagesCarYes', plotNames, sep='/')
trip3Yes <- makePlot(dfYes[(altID == 3)][(qID == j)])
ggsave(pathsYes[3], trip3Yes, width=3, height=5)
