library(here)
source(here::here('survey', 'pilot5', 'functions.R'))

# -----------------------------------------------------------------------------
# Main DOE construction - randomized, stratified by number of trips and modes

# Define modes
carExpress <- 'Car:\nExpress'
car  <- 'Car'
uber <- 'Uber/Lyft'
taxi <- 'Taxi'
bus  <- 'Bus'
walk <- 'Walk'
none <- 'None'

# Generate full factorial
ff <- as_tibble(expand.grid(
    leg1Mode      = c(car, carExpress, uber, taxi, bus),
    leg2Mode      = c(none, bus, walk),
    leg3Mode      = c(none, uber, taxi, bus),
    leg1Time      = c(10, 15, 20, 30, 40), # Minutes
    leg2Time      = c(3, 5, 10, 15, 20, 25), # Minutes
    leg3Time      = c(10, 15, 20, 25), # Minutes
    transfer1Time = c(2, 5, 10), # Minutes
    transfer2Time = c(2, 5, 10), # Minutes
    transfer3Time = c(2, 5, 10), # Minutes
    price         = c(10, 15, 20, 25, 30), # USD $
    expressFee    = c(5, 10), # USD $
    tripTimeUnc   = c(0.05, 0.10, 0.20)) # Plus/minus % of total trip time
    ) %>%
    # Filter out unrealistic or illogical cases
    filter(
        # No 3-leg trips for car modes
        ! ((leg1Mode %in% c(car, carExpress)) & (leg3Mode != none)),
        # If driving, minimum time is 10 minutes
        ! ((leg1Mode %in% c(car, carExpress)) & (leg1Time < 10)),
        # If walking, maximum time is 15 minutes
        ! ((leg2Mode == walk) & (leg2Time > 15)),
        # If bus, minimum time is 10 minutes
        ! ((leg2Mode == bus) & (leg2Time < 10)),
        # If not driving, max time for leg1 is 30 minutes
        ! ((leg1Mode == bus) & (leg1Time > 30)),
        ! ((leg1Mode == taxi) & (leg1Time > 30)),
        ! ((leg1Mode == uber) & (leg1Time > 30)),
        # If leg2Mode is "None", there can't be a leg 3
        ! ((leg2Mode == none) & (leg3Mode !=  none)),
        # Only walk in the 2nd leg if last leg is "None" or "Bus"
        #     i.e. you wouldn't uber -> walk -> uber...you would just uber the
        #     whole way.
        ! ((leg2Mode == walk) & (! leg3Mode %in% c(bus, none))),
        # If you start with Uber, you wouldn't use a Taxi later, and vice versa
        ! ((leg1Mode == uber) & (leg3Mode == taxi)),
        ! ((leg1Mode == taxi) & (leg3Mode == taxi))
    ) %>%
    mutate(
        # If driving, there's no transfer time at the start
        transfer1Time = ifelse(
            leg1Mode %in% c(car, carExpress), 0, transfer1Time),
        # If leg 2 or 3 are None, then the leg times and transfer times are 0
        leg2Time = ifelse(leg2Mode == none, 0, leg2Time),
        leg3Time = ifelse(leg3Mode == none, 0, leg3Time),
        transfer2Time = ifelse(leg2Mode == none, 0, transfer2Time),
        transfer3Time = ifelse(leg3Mode == none, 0, transfer3Time),
        # If walking, no transfer time
        transfer2Time = ifelse(leg2Mode == walk, 0, transfer2Time)
    ) %>%
    # Generate some useful variables
    mutate(
        numLegs = ifelse(
            leg2Mode == none, 1, ifelse(
            leg3Mode == none, 2, 3)),
        lastLegMode = ifelse(
            numLegs == 1, as.character(leg1Mode), ifelse(
            numLegs == 2, as.character(leg2Mode), as.character(leg3Mode))),
        carInTrip     = str_detect(leg1Mode, 'Car'),
        expressInTrip = str_detect(leg1Mode, 'Express'),
        walkInTrip    = leg2Mode == walk,
        busInTrip     = (leg1Mode == bus) | (leg2Mode == bus) |
                        (leg3Mode == bus),
        taxiInTrip = (leg1Mode == taxi) | (leg3Mode == taxi),
        uberInTrip = (leg1Mode == uber) | (leg3Mode == uber),
        # You can only have an express lane fee for car modes
        expressFee = ifelse(carInTrip, expressFee, 0)
    ) %>%
    distinct()
ff$rowID <- seq(nrow(ff))

# Sample from ff to balance the mode alternatives:
ids <- list(
    car1     = which(ff$carInTrip & ff$numLegs == 1),
    car2     = which(ff$carInTrip & ff$numLegs == 2),
    car3     = which(ff$carInTrip & ff$numLegs == 3),
    express1 = which(ff$expressInTrip & ff$numLegs == 1),
    express2 = which(ff$expressInTrip & ff$numLegs == 2),
    express3 = which(ff$expressInTrip & ff$numLegs == 3),
    walk1    = which(ff$walkInTrip & ff$numLegs == 1),
    walk2    = which(ff$walkInTrip & ff$numLegs == 2),
    walk3    = which(ff$walkInTrip & ff$numLegs == 3),
    bus1     = which(ff$busInTrip & ff$numLegs == 1),
    bus2     = which(ff$busInTrip & ff$numLegs == 2),
    bus3     = which(ff$busInTrip & ff$numLegs == 3),
    taxi1    = which(ff$taxiInTrip & ff$numLegs == 1),
    taxi2    = which(ff$taxiInTrip & ff$numLegs == 2),
    taxi3    = which(ff$taxiInTrip & ff$numLegs == 3),
    uber1    = which(ff$uberInTrip & ff$numLegs == 1),
    uber2    = which(ff$uberInTrip & ff$numLegs == 2),
    uber3    = which(ff$uberInTrip & ff$numLegs == 3)
)
nAlts <- unlist(map(ids, length))
numSamples <- rep(max(nAlts), length(ids))
names(numSamples) <- names(nAlts)
ids[which(nAlts == 0)] <- NULL
# Adjust sampling for unbalanced modes
unbalanced <- c('car1', 'car2', 'express1', 'express2', 'walk2', 'walk3')
numSamples[which(names(ids) %in% unbalanced)] <- max(nAlts)*1.5
for (i in 1:length(ids)) {
    samples[[i]] <- sample(x=ids[[i]], size=numSamples[i], replace=T)
}
ff_bal <- ff[unlist(samples),] # "bal" is for "balanced"

# Randomly sample from the ff_bal to evenly fit the desired sample size
nResp        <- 6000 # Number of respondents
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent
nRowsPerResp <- nAltsPerQ * nQPerResp
nRows        <- nResp*nRowsPerResp
doe <- ff_bal[sample(x=seq(nrow(ff_bal)), size=nRows, replace=T),]

# Make sure no two identical alts appear in one question
doe <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp)

# Add additional variables for plotting
doe <- doe %>%
    mutate(
        trip = ifelse(
            numLegs == 1, paste(leg1Mode), ifelse(
                numLegs == 2, paste(leg1Mode, leg2Mode, sep='|'),
                paste(leg1Mode, leg2Mode, leg3Mode, sep='|'))),
        totalLegTime  = leg1Time + leg2Time + leg3Time,
        totalWaitTime = transfer1Time + transfer2Time + transfer3Time,
        totalTripTime = totalLegTime + totalWaitTime,
        tripTimeMin = round(totalTripTime*(1 - tripTimeUnc)),
        tripTimeMax = round(totalTripTime*(1 + tripTimeUnc)),
        tripTimeRange  = paste(
            tripTimeMin, '-', tripTimeMax, 'minutes', sep=' '))

# Save design
write_csv(doe, here::here('survey', 'pilot5', 'survey', 'doeAll.csv'))

# Compare balance of modes:
doe %>%
    gather(mode, count, carInTrip:uberInTrip) %>%
    select(mode, count) %>%
    count(mode, count) %>%
    mutate(percent = n / nrow(doe)) %>%
    filter(count == TRUE) %>%
    ggplot() +
    geom_bar(aes(x = mode, y = percent), stat='identity')

# Compare balance of trip legs:
doe %>%
    ggplot() +
    geom_bar(aes(x = numLegs))

# -----------------------------------------------------------------------------
# Read in the doe and convert it to individual trips

doe <- read_csv(here::here('survey', 'pilot5', 'survey', 'doeAll.csv'))

# Create trips
tripDfList <- list()
for (i in 1:nrow(doe)) {
    tripDfList[[i]] <- getTripDf(doe[i,])
}

# Save trip list
saveRDS(tripDfList, here::here('survey', 'pilot5', 'survey',
                               'tripDfListAll.Rds'))

# -----------------------------------------------------------------------------
# Read in trip list and save all trips for each respondent as a csv file

tripDfList <- readRDS(here::here('survey', 'pilot5', 'survey',
                                 'tripDfListAll.Rds'))

# Save each trip
respID <- 1
index <- 2
temp <- list(tripDfList[[1]])
for (i in 2:length(tripDfList)) {
    trip <- tripDfList[[i]]
    if (trip$respID[1] == respID) {
        temp[[index]] <- trip
        index <- index + 1
    } else {
        # Save the tripDf
        tripDf <- do.call(rbind, temp)
        write_csv(tripDf, here::here('survey', 'pilot5', 'survey', 'tripsAll',
                                     paste(respID, '.csv', sep='')))
        # Start a new temp list
        respID <- trip$respID[1]
        index <- 2
        temp <- list(trip)
    }
}
