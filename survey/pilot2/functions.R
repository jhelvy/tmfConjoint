library(tidyverse)
library(ggrepel)
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

getTripDf <- function(row) {
    trip   <- makeTripVector(row)
    nodes  <- getTripNodes(trip, row)
    tripDf <- tibble(
        x     = 0,
        y     = nodes,
        label = trip) %>% 
        mutate(
            respID = row$respID,
            qID    = row$qID,
            altID  = row$altID,
            obsID  = row$obsID,
            type   = ifelse(str_detect(label, 'Start'), 'Node',
                     ifelse(str_detect(label, 'End'), 'Node',
                     ifelse(str_detect(label, 'Transfer'), 'Node',
                     ifelse(label == '', 'Node', 'Transit'))))) %>%
        select(x, y, label, type, respID, qID, altID, obsID)
    return(tripDf)
}

# -----------------------------------------------------------------------------
# Functions making the trip plots

theme_trip <- function(){
    theme(text = element_text("sans-serif"),

          panel.grid.minor = element_blank(),
          panel.background =  element_blank(),
          panel.border = element_blank(),

          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),

          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),

          legend.position = "none"
    )
}

formatPlotDf <- function(trip) {
    node  <- trip[type == 'Node']
    label <- trip[type != 'Node']
    return(list(node=node, label=label))
}

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


























# addWalkingStart <- function(trip, row) {
#
#     # y1 <- mean(tripDf$y[1:2])
#     # if (row$numLegs == 1) {y1 <- -0.2}
#     # y2 <- y1 / 2
#     # walkDf <- tibble(y=c(y1, y2), label=c("", label))
#     # tripDf <- bind_rows(tripDf, walkDf) %>% arrange(y)
#     return(tripDf)
# }

# addWalkingEnd <- function(trip, row) {
#     numRows <- nrow(tripDf)
#     label <- paste('Walk (', row$walkTimeEnd, ' min)', sep='')
#     y1 <- mean(c(1, tripDf$y[numRows-1]))
#     if (row$numLegs == 1) {y1 <- -0.8}
#     y2 <- mean(c(1, y1))
#     walkDf <- tibble(y=c(y1, y2), label=c("", label))
#     tripDf <- bind_rows(tripDf, walkDf) %>% arrange(y)
#     return(tripDf)
# }
