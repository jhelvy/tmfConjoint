library(tidyverse)
library(here)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

# -----------------------------------------------------------------------------
# Functions

addWaitTimes <- function(trip, row) {
    if (row$taxiWaitTime > 0) {trip <- addTaxiWaitTime(trip, row)}
    if (row$busWaitTime > 0) {trip <- addBusWaitTime(trip, row)}
    return(trip)
}

addTaxiWaitTime <- function(trip, row) {
    if ('Uber/Lyft' %in% trip) {
        uberId <- which(trip == 'Uber/Lyft') - 1
        trip[uberId] <- paste(trip[uberId], ' (', row$taxiWaitTime, 
                              ' min wait)', sep='')
    }
    if ('Taxi' %in% trip) {
        taxiId <- which(trip == 'Taxi') - 1
        trip[taxiId] <- paste(trip[taxiId], ' (', row$taxiWaitTime, 
                              ' min wait)', sep='')
    }
    return(trip)
}

addBusWaitTime <- function(trip, row) {
    if ('Bus' %in% trip) {
        busId <- which(trip == 'Bus') - 1
        trip[busId] <- paste(trip[busId], ' (', row$busWaitTime, 
                             ' min wait)', sep='')
    }
    return(trip)
}

addWalkingStart <- function(tripDf, row) {
    label <- paste('Walk (', row$walkTimeStart, ' min)', sep='')
    y <- mean(tripDf$y[1:2])
    if (row$numLegs == 1) {y <- y - 0.1}
    walkDf <- tibble(x=0, y=y, label=label)
    return(bind_rows(tripDf[1,], walkDf, tripDf[2:nrow(tripDf),]))
}

addWalkingEnd <- function(tripDf, row) {    
    numRows <- nrow(tripDf)
    label <- paste('Walk (', row$walkTimeEnd, ' min)', sep='')
    y <- mean(c(1, tripDf$y[numRows-1]))
    if (row$numLegs == 1) {y <- y + 0.1}
    walkDf <- tibble(x=0, y=y, label=label)
    return(bind_rows(tripDf[1:(numRows-1),], walkDf, tripDf[numRows,]))
}

getTripDf <- function(row) {
    trip <- str_split(row$trip, '\\|')[[1]]
    trip <- addWaitTimes(trip, row)
    tripDf <- tibble(
        x     = 0,
        y     = seq(0, 1, length.out = length(trip)),
        label = trip) 
    if (row$walkTimeStart > 0) {tripDf <- addWalkingStart(tripDf, row)}
    if (row$walkTimeEnd > 0) {tripDf <- addWalkingEnd(tripDf, row)}
    tripDf <- tripDf %>% 
        mutate(
        type = ifelse(str_detect(label, 'Start'), 'Start', 
               ifelse(str_detect(label, 'End'), 'End', 
               ifelse(str_detect(label, 'Walk'), 'Walk',
               ifelse(str_detect(label, 'Transfer'), 'Node', 'Vehicle')))))
        x = ifelse(type == 'Node', 0, 1))
    tripDf$respID <- row$respID
    tripDf$qID    <- row$qID
    tripDf$altID  <- row$altID
    tripDf$obsID  <- row$obsID
    return(tripDf)
}

# -----------------------------------------------------------------------------
doe <- read_csv(here::here('survey', 'pilot2', 'doe.csv')) %>%
    # Add transfers, start, and end labels
    mutate(
        trip = str_replace_all(trip, '\\|', '|Transfer|'),
        trip = paste('Start|', trip, '|End', sep=''))
    
# Create trips
tripDfList <- list()
for (i in 1:nrow(doe)) {
    tripDfList[[i]] <- getTripDf(doe[i,])
}

saveRDS(tripDfList, here::here('survey', 'pilot2', 'tripDfList.Rds'))
