# -----------------------------------------------------------------------------
# Functions for making trip images

getLabels <- function(trip) {
    modeLabels <- strsplit(trip$trip, '\\|')[[1]]
    names(modeLabels) <- seq(1, 2*length(modeLabels), by=2)
    if (trip$numLegs > 1) {
        transferLabels <- rep('Transfer', trip$numLegs-1)
        names(transferLabels) <- seq(2, 2*length(transferLabels), by=2)
        labels <- c(modeLabels, transferLabels)
        labels <- as.vector(labels[order(names(labels))])
    } else {
        labels <- modeLabels
    }
    return(rev(c('Start', labels, 'End')))
}

getPlotDf <- function(trip) {
    labels <- getLabels(trip)
    plotDf <- data.table(
        x     = 0,
        y     = seq(0, 1, length.out = length(labels)),
        label = labels)
    plotDf[, type := ifelse(label %in% c('Start', 'Transfer', 'End'), 
                            'Node', 'Edge')]
    return(plotDf)
}

makePlot <- function(trip) {
    p <- ggplot(data = getPlotDf(trip), aes(x = x, y = y)) +
        geom_point(size=5, aes(color=type)) +
        scale_color_manual(values=c("#FFFFFF", "#000000")) +
        geom_line() +
        geom_label(aes(x = x + 1, y = y, label = label, fill = type)) +
        scale_fill_manual(values=c("#FFFFFF", "#E0E0E0")) +
        # annotate("text", x = 8, y = 0, label = "") +
        scale_x_continuous(limits = c(-0.5, 2)) +
        theme_void() +
        theme(legend.position = "none")
    return(p)
}

# -----------------------------------------------------------------------------
# Load the full design of experiment
library(data.table)
library(ggplot2)
doe <- fread('https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot1/doe.csv')
# doe <- fread('./survey/pilot1/doe.csv')

# Recode repsondent ID if it's greater than the max(doe$respID)
respondentID <- 1
if (respondentID > max(doe$respID)) {
    respondentID <- respondentID %% max(doe$respID)
}

# doe <- doe[respID == respondentID]
# trip <- doe[3,] # Example with 1 leg
# trip <- doe[1,] # Example with 2 legs
# trip <- doe[6,] # Example with 3 legs
# makePlot(trip)