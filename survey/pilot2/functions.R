library(tidyverse)
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
            # You only wait on the bus if it's in the mode set
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
        trip[modeID] <- paste(trip[modeID], ' (', waitTime, 
                              ' min wait)', sep='')
    }
    return(trip)
}

addWalkingStart <- function(tripDf, row) {
    label <- paste('Walk (', row$walkTimeStart, ' min)', sep='')
    y1 <- mean(tripDf$y[1:2])
    if (row$numLegs == 1) {y1 <- 0.2}
    y2 <- y1 / 2
    walkDf <- tibble(y=c(y1, y2), label=c("", label))
    tripDf <- bind_rows(tripDf, walkDf) %>% arrange(y)
    return(tripDf)
}

addWalkingEnd <- function(tripDf, row) {    
    numRows <- nrow(tripDf)
    label <- paste('Walk (', row$walkTimeEnd, ' min)', sep='')
    y1 <- mean(c(1, tripDf$y[numRows-1]))
    if (row$numLegs == 1) {y1 <- 0.8}
    y2 <- mean(c(1, y1))
    walkDf <- tibble(y=c(y1, y2), label=c("", label))
    tripDf <- bind_rows(tripDf, walkDf) %>% arrange(y)
    return(tripDf)
}

addWalking <- function(tripDf, row) {
    if (row$walkTimeStart > 0) {tripDf <- addWalkingStart(tripDf, row)}
    if (row$walkTimeEnd > 0) {tripDf <- addWalkingEnd(tripDf, row)}    
    return(tripDf)
}

getTripDf <- function(row) {
    trip <- str_split(row$trip, '\\|')[[1]]
    trip <- addWaitTimes(trip, row)
    tripDf <- tibble(
        y     = seq(0, 1, length.out = length(trip)),
        label = trip) %>%  
        addWalking(row) %>% 
        mutate(
            x      = 0,
            respID = row$respID,
            qID    = row$qID,
            altID  = row$altID,
            obsID  = row$obsID,
            type   = ifelse(str_detect(label, 'Start'), 'Node', 
                     ifelse(str_detect(label, 'End'), 'Node', 
                     ifelse(str_detect(label, 'Transfer'), 'Node', 
                     ifelse(label == '', 'Node', 'Vehicle'))))) %>% 
        select(x, y, label, type, respID, qID, altID, obsID)
    return(tripDf)
}