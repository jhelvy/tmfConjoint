library(tidyverse)
library(ggrepel)
library(rlang)
library(data.table)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

# -----------------------------------------------------------------------------
# Functions making the DOE

# Plot a comparison of the full factorial and doe counts
barCompare <- function(df, var) {
    df$var <- unlist(df[var])
    df %>%
        group_by(design) %>%
        count(var) %>%
        ggplot(aes(x=var, y=n)) +
        geom_bar(stat='identity', position='dodge') +
        facet_wrap(~design)
}

addMetaData <- function(doe, nAltsPerQ, nQPerResp) {
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
# Functions for making the tripDfs

addWalking <- function(trip, row) {
    if (row$walkTimeStart > 0) {
        label <- paste('Walk\n(', row$walkTimeStart, ' min)', sep='')
        trip  <- c(label, 'Transfer', trip)
    }
    if (row$walkTimeEnd > 0) {
        label <- paste('Walk\n(', row$walkTimeEnd, ' min)', sep='')
        trip  <- c(trip, 'Transfer', label)
    }
    return(trip)
}

addTransferTimes <- function(trip, row) {
    transferTimes <- row[c('transfer1Time', 'transfer2Time', 'transfer3Time')]
    trip <- c('Start', trip, 'End')
    time_i <- seq(length(transferTimes))
    trip_i <- 2*time_i - 1
    if (str_detect(trip[2], 'Walk')) {trip_i <- trip_i + 2}
    for (i in time_i) {
        if (transferTimes[i] > 0) {
            trip[trip_i[i]] = paste(
                trip[trip_i[i]], '\n(', transferTimes[i], ' min wait)', sep='')
        }
    }
    return(trip)
}

makeTripVector <- function(row) {
    trip <- str_replace_all(row$trip, '\\|', '|Transfer|')
    trip <- str_split(trip, '\\|')[[1]]
    trip <- addWalking(trip, row)
    trip <- addTransferTimes(trip, row)
    return(trip)
}

getPlotLineNodes <- function(tripDf) {
    lineNodes <- rep(-1, nrow(tripDf))
    lineNodes[which(str_detect(tripDf$label, 'Walk'))] <- 0
    index = 1
    for (i in 1:length(lineNodes)) {
        if (lineNodes[i] == 0) {
            index = index + 1
            next
        }
        lineNodes[i] = index
    }
    # Any legs < 3 nodes don't need a solid lineNodes
    walkNodes <- as.integer(names(which(table(lineNodes) < 3)))
    lineNodes[which(lineNodes %in% walkNodes)] <- 0
    return(lineNodes)
}

addPlotLabels <- function(tripDf) {
    # Compute where to put nodes
    node = rep(0, nrow(tripDf))
    node[which(str_detect(tripDf$label, 'Transfer'))] <- 1
    # Compute which type of label to print
    labelType = rep('Transit', nrow(tripDf))
    labelType[which(str_detect(tripDf$label, 'Transfer'))] <- 'Node'
    labelType[c(1, nrow(tripDf))] <- 'Terminal'
    # Set line points
    lineNodes <- getPlotLineNodes(tripDf)
    # Add variables to data frame
    tripDf$node      <- node
    tripDf$labelType <- labelType
    tripDf$lineNodes <- lineNodes
    return(tripDf)
}

getYSpacing <- function(trip, row) {
    transitTime = row$totalTripTime - row$totalWalkTime - row$totalWaitTime
    legTime     = transitTime / row$numLegs
    times       = rep(legTime, row$numLegs)
    if (str_detect(row$leg2Mode, 'Walk')) { times[2] = row$walkTimeLeg }
    if (row$walkTimeStart > 0) { times = c(row$walkTimeStart, times) }
    if (row$walkTimeEnd > 0) { times = c(times, row$walkTimeEnd) }
    breaks  = cumsum(c(0, -1 * times / sum(times)))
    if (abs(breaks[2]) < 0.12) {
        breaks[2] = -0.12
    }
    if (abs(breaks[length(breaks)-1]) > 0.88) {
        breaks[length(breaks)-1] = -0.88
    }
    spacing = (breaks[2:length(breaks)] - breaks[1:(length(breaks) - 1)]) / 2
    y       = c(0)
    for (i in 1:(length(breaks) - 1)) {
        y = c(y, breaks[i] + spacing[i], breaks[i+1])
    }
    return(y)
}

getTripDf <- function(row) {
    trip   <- makeTripVector(row)
    tripDf <- tibble(
        x     = 0,
        y     =  getYSpacing(trip, row),
        label = trip) %>%
        addPlotLabels() %>%
        mutate(
            respID    = row$respID,
            qID       = row$qID,
            altID     = row$altID,
            obsID     = row$obsID,
            price     = row$price,
            timeRange = row$tripTimeRange)
    return(tripDf)
}
