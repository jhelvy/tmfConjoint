setwd('/Users/jhelvy/gh/tmfConjoint/')


library(here)
source(here::here('survey', 'pilot2', 'functions.R'))

# -----------------------------------------------------------------------------
respondentID <- 1

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

