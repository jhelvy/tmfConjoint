library(tidyverse)
library(ggrepel)
library(rlang)
library(data.table)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

# -----------------------------------------------------------------------------
# Functions making the DOE

# If leg2Mode is "None", then leg3Mode also must be "None",
# and the leg times and wait times should be 0
fixNoneCases <- function(df) {
    temp <- df %>%
    mutate(
        leg3Mode = ifelse(leg2Mode == 'None', 'None', as.character(leg3Mode)),
        # If leg 2 or 3 are None, then the leg times and transfer times are 0
        leg2Time = ifelse(leg2Mode == 'None', 0, leg2Time),
        leg3Time = ifelse(leg3Mode == 'None', 0, leg3Time),
        transfer2Time = ifelse(leg2Mode == 'None', 0, transfer2Time),
        transfer3Time = ifelse(leg3Mode == 'None', 0, transfer3Time),
        # If walking, then no transfer time
        transfer1Time = ifelse(leg1Mode == 'Walk', 0, transfer1Time),
        transfer2Time = ifelse(leg2Mode == 'Walk', 0, transfer2Time)) %>%
    # Remove duplicates that may now be remaining
    distinct()
    return(temp)
}

addSummaryVars <- function(df) {
    # Generate some useful summary variables
    temp <- df %>% mutate(
        numLegs = ifelse(
            leg2Mode == 'None', 1, ifelse(
            leg3Mode == 'None', 2, 3)),
        lastLegMode = ifelse(
            numLegs == 1, as.character(leg1Mode), ifelse(
            numLegs == 2, as.character(leg2Mode), as.character(leg3Mode))),
        trip = ifelse(
            numLegs == 1, paste(leg1Mode), ifelse(
            numLegs == 2, paste(leg1Mode, leg2Mode, sep='|'),
                          paste(leg1Mode, leg2Mode, leg3Mode, sep='|'))),
        carInTrip     = str_detect(trip, 'Car'),
        expressInTrip = str_detect(trip, 'Express'),
        walkInTrip    = str_detect(trip, 'Walk'),
        busInTrip     = str_detect(trip, 'Bus'),
        taxiInTrip    = str_detect(trip, 'Taxi'),
        uberInTrip    = str_detect(trip, 'Uber'),
        totalLegTime  = leg1Time + leg2Time + leg3Time,
        totalWaitTime = transfer1Time + transfer2Time + transfer3Time,
        totalTripTime = totalLegTime + totalWaitTime,
        tripTimeMin   = round(totalTripTime*(1 - tripTimeUnc)),
        tripTimeMax   = round(totalTripTime*(1 + tripTimeUnc)),
        tripTimeRange = paste(tripTimeMin, '-', tripTimeMax, 'minutes',
                        sep=' ')) %>%
        # Remove duplicates that may now be remaining
        distinct()
    return(temp)
}

filterCases <- function(df) {
    # Filter out unrealistic or illogical cases
    temp <- df %>%
    filter(
        # Filter out unrealistic cases
            trip %in% goodTrips,
        # Filter out unrealistic prices
            # If trip is bus & walking only, maximum price is $10
            ! ((trip %in%  busTrips) & (price > 10)),
            # If trip contains car, uber, or taxi, minimum price is $5
            ! ((carInTrip | uberInTrip | taxiInTrip) & (price < 5)),
        # Filter out times
            # If walking, maximum time is 15 minutes
            ! ((leg1Mode == 'Walk') & (leg1Time > 15)),
            ! ((leg2Mode == 'Walk') & (leg2Time > 15)),
            ! ((leg3Mode == 'Walk') & (leg3Time > 15))) %>%
        # Remove duplicates that may now be remaining
        distinct()
    return(temp)
}

balanceTrips <- function(df, modes, thresholds) {
    orig <- df
    solution <- FALSE
    while (solution == FALSE) {
        result <- runTripLoop(trips, modes, thresholds)
        solution <- result$solution
    }
    return(result$trips)
}

runTripLoop <- function(trips, modes, thresholds) {
    count <- 0
    diffs <- getDiffs(trips, modes)
    while ((diffs['mode'] > thresholds['mode']) |
           (diffs['leg'] > thresholds['leg'])) {
        trips <- getNewTrips(trips, diffs, modes)
        diffs <- getDiffs(trips, modes)
        count <- count + 1
        if (count > 200) {
            return(list(trips=trips, solution=FALSE))
        }
    }
    return(list(trips=trips, solution=TRUE))
}

# Adds a random new row to the unique set of trips.
# If the new row helps balance the mode and legs, keep it, otherwise
# return the original df
getNewTrips <- function(df, diffs, modes) {
    row <- df[sample(seq(nrow(df)), 1),]
    temp <- rbind(df, row)
    newDiffs <- getDiffs(temp, modes)
    if ((newDiffs['mode'] <  diffs['mode']) |
        (newDiffs['leg'] < diffs['leg'])) {
        return(temp)
    }
    return(df)
}

getDiffs <- function(df, modes) {
    modeCounts <- apply(df[modes], 2, sum)
    legCounts <- table(df$numLegs)
    modeDiff <- max(max(modeCounts) - modeCounts)
    legDiff <- max(max(legCounts) - legCounts)
    return(c(mode = modeDiff, leg = legDiff))
}

getBalancedFF <- function (ff, trips) {
    proportions <- trips %>% count(trip)
    ids <- list()
    for (i in 1:nrow(proportions)) {
        ids[[i]] <- which(ff$trip == proportions$trip[i])
    }
    nAlts <- unlist(map(ids, length))
    numSamples <- rep(max(nAlts), length(ids)) * proportions$n
    samples <- list()
    for (i in 1:length(ids)) {
        samples[[i]] <- sample(x=ids[[i]], size=numSamples[i], replace=T)
    }
    ff_bal <- ff[unlist(samples),] # "bal" is for "balanced"
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
    temp <- doe %>%
        mutate(distinctTrip = paste(
            leg1Mode, leg2Mode, leg3Mode, leg1Time, leg2Time, leg3Time,
            transfer1Time, transfer2Time, transfer3Time, sep='|'))
    uniqueTrips <- unique(temp$distinctTrip)
    tripsDf <- data.frame(
        distinctTrip = uniqueTrips,
        tripID = seq(length(uniqueTrips)))
    doe <- temp %>%
        left_join(tripsDf) %>%
        group_by(obsID) %>%
        mutate(numUnique = n_distinct(tripID)) %>%
        ungroup() %>%
        select(-distinctTrip)
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
    doe <- doe %>% select(-tripID, -numUnique)
    return(doe)
}

# -----------------------------------------------------------------------------
# Functions for making the tripDfs
addLegTimes <- function(trip, row) {
    legTimes <- row[c('leg1Time', 'leg2Time', 'leg3Time')]
    legTimes <- legTimes[1:row$numLegs]
    time_i <- seq(length(legTimes))
    trip_i <- 2*time_i
    for (i in time_i) {
        trip[trip_i[i]] = paste(
            trip[trip_i[i]], '\n(', legTimes[i], ' mins)', sep='')
    }
    return(trip)
}

addTransferTimes <- function(trip, row) {
    transferTimes <- row[c('transfer1Time', 'transfer2Time', 'transfer3Time')]
    transferTimes <- transferTimes[1:row$numLegs]
    time_i <- seq(length(transferTimes))
    trip_i <- 2*time_i - 1
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
    trip <- c('Start', str_split(trip, '\\|')[[1]], 'End')
    trip <- addLegTimes(trip, row)
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

getYSpacing <- function(trip, row) {
    legTimes <- row[c('leg1Time', 'leg2Time', 'leg3Time')]
    legTimes <- legTimes[1:row$numLegs]
    breaks  = cumsum(c(0, -1 * legTimes / sum(legTimes)))
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
