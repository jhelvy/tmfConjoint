library(data.table)
library(ggplot2)

# -----------------------------------------------------------------------------
# Functions for making trip images

addWaitTimes <- function(labels, trip) {
    waitTime <- labels
    if (trip$taxiWaitTime > 0) {
        if ('Uber/Lyft' %in% labels) {
            labels[which(labels == 'Uber/Lyft')]
            gsub('Uber/Lyft', paste('Uber/Lyft'), labels)
            
        }

    }
}

getLabels <- function(trip) {
    modeLabels <- trimws(strsplit(trip$trip, '\\|')[[1]])
    names(modeLabels) <- seq(1, 2*length(modeLabels), by=2)
    if (trip$numLegs > 1) {
        transferLabels <- rep('Transfer', trip$numLegs-1)
        names(transferLabels) <- seq(2, 2*length(transferLabels), by=2)
        labels <- c(modeLabels, transferLabels)
        labels <- as.vector(labels[order(names(labels))])
    } else {
        labels <- modeLabels
    }
    labels <- c('Start', labels, 'End')
    labels <- addWaitTimes(labels, trip)
    return(labels)
}

getPlotDf <- function(trip) {
    labels <- getLabels(trip)
    plotDf <- data.table(
        x     = 0,
        y     = seq(0, -1, length.out = length(labels)),
        label = labels)
    plotDf[, type := ifelse(label %in% c('Start', 'Transfer', 'End'),
                            'Node', 'Edge')]
    return(plotDf)
}

makePlot <- function(trip) {
    p <- ggplot(data = getPlotDf(trip), aes(x = x, y = y)) +
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

doeFilePath <- 'https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot1/doe.csv'
doe <- fread(doeFilePath)


respondentID <- 1
if (respondentID > max(doe$respID)) {
    respondentID <- respondentID %% max(doe$respID)
}

# Filter out the respondent
doe <- doe[respID == respondentID]
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

makePlot(trip2)