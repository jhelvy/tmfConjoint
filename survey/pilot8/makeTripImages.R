# setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot8', 'functions.R'))
source(here::here('survey', 'pilot8', 'filterCases.R'))

# Read in trip Dfs
tripDfsNo <- fread(
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarNo.csv'))
tripDfsYes <- fread(
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarYes.csv'))

for (i in seq(max(tripDfsYes$respID))) {
    dfYes <- tripDfsYes[respID == i]
    dfNo <- tripDfsNo[respID == i]
}
