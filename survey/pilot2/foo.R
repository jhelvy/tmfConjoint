library(data.table)
library(ggplot2)
library(here)

# -----------------------------------------------------------------------------
# Functions for making trip images

makePlot <- function(trip) {
    p <-

    ggplot(data = trip, aes(x = x, y = y)) +
        geom_line() +
        geom_label(aes(x = x, y = y, label = label, fill = type)) +
        scale_fill_manual(values=c("#FFFFFF", "#E0E0E0")) +
        scale_x_continuous(limits = c(-1, 1)) +
        theme_void() +
        theme(
            legend.position = "none",
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    return(p)
}

# -----------------------------------------------------------------------------
# Load the full design of experiment

respondentID <- 1

# Load respondent doe
path <- paste('https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot2/trips/', respondentID, '.csv', sep='')
doe <- fread(path)

# Filter out the respondent
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

makePlot(trip1)
