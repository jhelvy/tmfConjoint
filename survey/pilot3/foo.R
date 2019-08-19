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
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]
```
(1 of 6) If these were your only options for this trip, which would you choose?"



# Load respondent doe
# path <- paste('https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot2/trips/', respondentID, '.csv', sep='')
path<- paste(here::here('survey', 'pilot2', 'doe', 'trips'), '/', respondentID, '.csv', sep='')
doe <- fread(path)

# Filter out the trips
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

ggplot(data = trip[node == 1], aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_point(size=4, alpha=.5) +
    geom_point(size=6, alpha=.25) +
    geom_line(data = trip, size=1, linetype='dotted') +
    geom_line(data = trip[line == 1], size=1) +
    theme_void() +
    geom_label_repel(data = trip[labelType == 'Transit'], aes(label=label),
        size = 4,
        force = 3,
        nudge_x = 0.1,
        fontface ="bold",
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.75, "lines"),
        color= "black",
        segment.colour = "black") +
    geom_label_repel(data = trip[labelType == 'Node'], aes(label=label),
        size = 4,
        force = 3,
        nudge_x = -0.1,
        fontface ="bold",
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.75, "lines"),
        color= "black",
        segment.colour = "black") +
    geom_label(data = trip[labelType == 'Terminal'], aes(label=label))

