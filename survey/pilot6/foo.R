setwd('/Users/jhelvy/gh/tmfConjoint/')


library(here)
source(here::here('survey', 'pilot6', 'functions.R'))

# -----------------------------------------------------------------------------
respondentID <- 1

# Load respondent doe
# path <- paste('https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot6/trips/', respondentID, '.csv', sep='')
path<- paste(here::here('survey', 'pilot6', 'survey', 'tripsAll'), '/', respondentID, '.csv', sep='')
doe <- fread(path)

# Filter out the trips
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

source(here::here('survey', 'pilot6', 'survey', 'functions.R'))

makePlot(trip2)

