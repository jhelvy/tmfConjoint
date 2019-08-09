library(tidyverse)
library(ggrepel)
library(data.table)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

# -----------------------------------------------------------------------------
# Functions making the DOE

removeIllogicalTrips <- function(ff) {
    design <- ff %>%
        mutate(
            numLegs      = str_count(trip, '\\|') + 1,
            firstLegMode = word(trip, sep='\\|', 1),
            lastLegMode  = word(trip, sep='\\|', numLegs),
            carInTrip    = str_detect(trip, 'Car'),
            busInTrip    = str_detect(trip, 'Bus'),
            taxiInTrip   = str_detect(trip, 'Taxi') | str_detect(trip, 'Uber'),
            # You only walk at the start or end of a bus trip
            walkTimeStart = ifelse(
                (busInTrip == F) | (firstLegMode != 'Bus'), 0, walkTimeStart),
            walkTimeEnd = ifelse(
                (busInTrip == F) | (lastLegMode != 'Bus'), 0, walkTimeEnd),
            # You only wait on the bus if it's in the mode set and
            busWaitTime = ifelse(
                (busInTrip == F), 0, busWaitTime),
            # You only wait on a taxi if it's in the mode set
            taxiWaitTime = ifelse(
                (taxiInTrip == F), 0, taxiWaitTime)
        ) %>% distinct()
    return(design)
}

makeFullFactorial <- function(trip, atts) {
    atts$trip <- trip
    ff <- expand.grid(atts)
    return(removeIllogicalTrips(ff))
}

getSampleIDs <- function(ff, numSamples) {
    car  <- sample(x=which(ff$carInTrip),  size=numSamples, replace=T)
    taxi <- sample(x=which(ff$taxiInTrip), size=numSamples, replace=T)
    bus  <- sample(x=which(ff$busInTrip),  size=numSamples, replace=T)
    return(list(car=car, taxi=taxi, bus=bus))
}

addMetaData <- function(doe, nAltsPerQ, nQPerResp) {
    nRowsPerResp   <- nAltsPerQ * nQPerResp
    nResp          <- nrow(doe) / nRowsPerResp # Number of respondents
    doe$respID     <- rep(seq(nResp), each=nRowsPerResp)
    doe$qID        <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
    doe$altID      <- rep(seq(nAltsPerQ), nResp*nQPerResp)
    doe$obsID      <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)
    row.names(doe) <- seq(nrow(doe))
    return(doe)
}

getUniqueAltCounts <- function(doe) {
    doe <- doe %>%
        group_by(obsID) %>%
        mutate(numUnique = n_distinct(rowID)) %>%
        ungroup()
    return(doe)
}

removeDoubleAlts <- function(doe, nAltsPerQ, nQPerResp) {
    doe <- addMetaData(doe, nAltsPerQ, nQPerResp)
    doe <- getUniqueAltCounts(doe)
    doubleRows <- which(doe$numUnique != 3)
    while (length(doubleRows) != 0) {
        newRows <- sample(x=seq(nrow(doe)), size=length(doubleRows), replace=F)
        doe[doubleRows,] <- doe[newRows,]
        doe <- addMetaData(doe, nAltsPerQ, nQPerResp)
        doe <- getUniqueAltCounts(doe)
        doubleRows <- which(doe$numUnique != 3)
    }
    doe <- doe %>% select(-rowID, -numUnique)
    return(doe)
}

# -----------------------------------------------------------------------------
# Functions for making the trips

addWalking <- function(trip, row) {
    if (row$walkTimeStart > 0) {
        label <- paste('Walk (', row$walkTimeStart, ' min)', sep='')
        trip  <- c(label, 'Transfer', trip)
    }
    if (row$walkTimeEnd > 0) {
        label <- paste('Walk (', row$walkTimeEnd, ' min)', sep='')
        trip  <- c(trip, 'Transfer', label)
    }
    return(trip)
}

addWaitTimes <- function(trip, row) {
    if (row$taxiWaitTime > 0) {
        trip <- addModeWaitTime(trip, 'Taxi', row$taxiWaitTime)
        trip <- addModeWaitTime(trip, 'Uber/Lyft', row$taxiWaitTime)
    }
    if (row$busWaitTime > 0) {
        trip <- addModeWaitTime(trip, 'Bus', row$busWaitTime)
    }
    return(trip)
}

addModeWaitTime <- function(trip, mode, waitTime) {
    if (mode %in% trip) {
        modeID <- which(trip == mode) - 1
        trip[modeID] <- paste(trip[modeID], '\n(', waitTime,
                              ' min wait)', sep='')
    }
    return(trip)
}

makeTripVector <- function(row) {
    trip <- str_replace_all(row$trip, '\\|', '|Transfer|')
    trip <- str_split(trip, '\\|')[[1]]
    trip <- addWalking(trip, row)
    trip <- c('Start', trip, 'End')
    trip <- addWaitTimes(trip, row)
    return(trip)
}

getTripNodes <- function(trip, row) {
    numNodes <- length(trip)
    if (row$walkTimeStart > 0) {numNodes <- numNodes - 2}
    if (row$walkTimeEnd > 0) {numNodes <- numNodes - 2}
    y   <- seq(0, -1, length.out = numNodes)
    gap <- y[1] - y[2]
    if (row$walkTimeStart > 0) {
        node1 <- y[1] + gap/2
        node2 <- y[1] + gap
        y <- c(node2, node1, y) - gap
    }
    if (row$walkTimeEnd > 0) {
        node1 <- y[length(y)] - gap/2
        node2 <- y[length(y)] - gap
        y <- c(y, node1, node2)
    }
    return(y)
}

addPlotStats <- function(tripDf, row) {
    # Compute where to put nodes
    node = rep(0, nrow(tripDf))
    node[which(str_detect(tripDf$label, 'Transfer'))] <- 1
    # Compute where to draw the solid line
    line = rep(0, nrow(tripDf))
    if (row$walkTimeStart > 0) {
        line[3] <- 1
    } else {
        line[1] <- 1
    }
    if (row$walkTimeEnd > 0) {
        line[nrow(tripDf)-2] <- 1
    } else {
        line[nrow(tripDf)] <- 1
    }
    # Compute which type of label to print
    labelType = rep('Transit', nrow(tripDf))
    labelType[which(str_detect(tripDf$label, 'Transfer'))] <- 'Node'
    labelType[c(1, nrow(tripDf))] <- 'Terminal'
    # Add variables to data frame
    tripDf$node <- node
    tripDf$line <- line
    tripDf$labelType <- labelType
    return(tripDf)
}

getTripDf <- function(row) {
    trip   <- makeTripVector(row)
    nodes  <- getTripNodes(trip, row)
    tripDf <- tibble(
        x     = 0,
        y     = nodes,
        label = trip) %>%
        addPlotStats(row) %>%
        mutate(
            respID = row$respID,
            qID    = row$qID,
            altID  = row$altID,
            obsID  = row$obsID)
    return(tripDf)
}

# -----------------------------------------------------------------------------
# Functions making the trip plots

makePlot <- function(trip) {
    p <-
        ggplot(data = trip[node == 1], aes(x = x, y = y)) +
        geom_point(size=2) +
        geom_point(size=4, alpha=.5) +
        geom_point(size=6, alpha=.25) +
        geom_line(data = trip, size=1, linetype='dotted') +
        geom_line(data = trip[line == 1], size=1) +
        theme_trip() +
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
    return(p)
}
