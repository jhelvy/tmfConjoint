library(tidyverse)
library(ggrepel)
library(data.table)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

# -----------------------------------------------------------------------------
# Functions making the DOE

# Plot a comparison of the full factorial and doe counts
barCompare <- function(df, var) {
    df %>%
        group_by(design) %>%
        count({{var}}) %>%
        ggplot(aes(x={{var}}, y=n)) +
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
# Functions for making the trips

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
            obsID  = row$obsID,
            price  = row$price,
            timeRange = row$tripTimeRange)
    return(tripDf)
}
