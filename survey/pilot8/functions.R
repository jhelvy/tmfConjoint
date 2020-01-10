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
            # If not driving, max time for leg 1 is 30 minutes
            ! ((leg1Mode == 'Bus') & (leg1Time > 30)),
            ! ((leg1Mode == 'Taxi') & (leg1Time > 30)),
            ! ((leg1Mode == 'Uber/Lyft') & (leg1Time > 30)),
            # Maximum walking time is 15 minutes
            ! ((leg1Mode == 'Walk') & (leg1Time > 15)),
            ! ((leg2Mode == 'Walk') & (leg2Time > 15)),
            ! ((leg3Mode == 'Walk') & (leg3Time > 15))) %>%
        # Remove duplicates that may now be remaining
        distinct()
    return(temp)
}

carSpecificCleaning <- function(df) {
    # Filter out unrealistic or illogical cases
    temp <- df %>%
    mutate(
        # You can only have an express lane fee for car modes
        expressFee = ifelse(carInTrip, expressFee, 0),
        price = price + expressFee,
        # If first leg is car, then no wait time
        transfer1Time = ifelse(
            leg1Mode %in% c('Car', 'Car:\nExpress'), 0, transfer1Time)) %>%
    filter(
        # Minimum driving time is 10 minutes
        ! (str_detect(leg1Mode, 'Car') & (leg1Time < 10))) %>%
        # Remove duplicates that may now be remaining
        distinct()
    return(temp)
}

getBalancedTrips <- function(df, modes, thresholds) {
    trips <- getTrips(df, modes)
    orig <- df
    solution <- FALSE
    while (solution == FALSE) {
        result <- runTripLoop(trips, modes, thresholds)
        solution <- result$solution
    }
    return(result$trips)
}

getTrips <- function(df, modes) {
    trips <- df %>%
    distinct(trip, carInTrip, expressInTrip,
             walkInTrip, busInTrip, taxiInTrip, uberInTrip, numLegs) %>%
    mutate(
        car  = ifelse(carInTrip | expressInTrip, T, F),
        taxi = ifelse(taxiInTrip | uberInTrip, T, F),
        bus  = busInTrip,
        walk = walkInTrip) %>%
    select(trip, numLegs, car, taxi, walk, bus)
    if ('car' %in% modes) { return(trips) }
    return(select(trip, -car))
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
            return(list(trips = trips, solution = FALSE))
        }
    }
    return(list(trips=trips, solution=TRUE))
}

getDiffs <- function(df, modes) {
    modeCounts <- apply(df[modes], 2, sum)
    legCounts <- table(df$numLegs)
    modeDiff <- max(max(modeCounts) - modeCounts)
    legDiff <- max(max(legCounts) - legCounts)
    return(c(mode = modeDiff, leg = legDiff))
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

getBalancedFF <- function (ff, trips) {
    proportions <- count(trips, trip)
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
    return(ff_bal)
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

# -----------------------------------------------------------------------------
# Functions for making the trip plot

makePlot <- function(trip) {
    p <-
        ggplot(data = trip, aes(x = x, y = y)) +
        # Draw lines
        geom_line(data = trip, size = 1, linetype = 'dotted') +
        geom_line(data = trip[lineNodes == 1], size = 1) +
        geom_line(data = trip[lineNodes == 2], size = 1) +
        geom_line(data = trip[lineNodes == 3], size = 1) +
        # Draw nodes
        geom_point(data = trip[node == 1], size = 4, pch = 21,
                   fill = 'white', colour = 'black') +
        theme_void() +
        geom_text_repel(data = trip[labelType == 'Transit'], aes(label=label),
                         size           = 4,
                         force          = 3,
                         nudge_x        = 1,
                         fontface       = "bold",
                         box.padding    = unit(0.35, "lines"),
                         point.padding  = unit(0.75, "lines"),
                         color          = "black",
                         # fill           = "white",
                         segment.colour = "black") +
        geom_label_repel(data = trip[labelType == 'Node'], aes(label=label),
                         size           = 4,
                         force          = 3,
                         nudge_x        = -1,
                         fontface       = "bold",
                         box.padding    = unit(0.35, "lines"),
                         point.padding  = unit(0.75, "lines"),
                         color          = "black",
                         fill           = "white",
                         segment.colour = "black") +
        geom_label(data = trip[labelType == 'Terminal'], aes(label=label),
                   label.size = 1,
                   fontface   = "bold",
                   fill       = "white",
                   color      = "black") +
        scale_x_continuous(limits=c(-1, 0.6)) +
        # Add price and time totals at the top
        annotate("text", x = -0.7, y = 0.15, fontface = "bold",
                 label = "Total Price:\nTotal Time:") +
        annotate("text", x = -0.35, y = 0.15, hjust = 0,
                 label = paste0("$", unique(trip$price),
                                "\n$", unique(trip$timeRange)))
    return(p)
}
