library(here)
source(here::here('survey', 'pilot5', 'functions.R'))

# -----------------------------------------------------------------------------
# Main DOE construction - randomized, stratified by number of trips and modes

# List full set of possible modes in each trip leg
car  <- c('Car', 'Car\n(Express)')
taxi <- c('Uber/Lyft', 'Taxi')
bus  <- 'Bus'
walk <- 'Walk'

# Generate the full factorial design and filter illogical trips
ff <- as_tibble(expand.grid(
    leg1Mode       = c(car, taxi, bus),
    leg2Mode       = c('None', bus, walk),
    leg3Mode       = c('None', taxi, bus),
    leg1Time       = c(5, 10, 15), # Minutes
    leg2Time       = c(5, 10, 15), # Minutes
    leg3Time       = c(5, 10, 15), # Minutes
    transfer1Time  = c(2, 5, 10), # Minutes
    transfer2Time  = c(2, 5, 10), # Minutes
    transfer3Time  = c(2, 5, 10), # Minutes
    price          = c(10, 15, 20, 25, 30), # USD $
    expressFee     = c(5, 10), # USD $
    tripTimeUnc    = c(0.05, 0.1, 0.2))) %>% # Plus/minus % of total trip time
    # If leg2Mode is "None", there can't be a leg 3
    filter(! ((leg2Mode == 'None') &
              (leg3Mode != 'None'))) %>%
    # Only walk in the 2nd leg if last leg is "None" or "Bus"
    filter(! ((leg2Mode %in% walk) &
              (! leg3Mode %in% c(bus, 'None')))) %>%
    # If start with Uber, can't go to Taxi, and vice versa
    filter(! ((leg1Mode == 'Uber/Lyft') &
              (leg3Mode == 'Taxi'))) %>%
    filter(! ((leg1Mode == 'Taxi') &
              (leg3Mode == 'Uber/Lyft'))) %>%
    mutate(
        # Generate some useful variables
        numLegs = ifelse(
            leg2Mode == 'None', 1, ifelse(
            leg3Mode == 'None', 2, 3)),
        lastLegMode = ifelse(
            numLegs == 1, as.character(leg1Mode), ifelse(
            numLegs == 2, as.character(leg2Mode), as.character(leg3Mode))),
        carInTrip     = str_detect(leg1Mode, 'Car'),
        expressInTrip = str_detect(leg1Mode, 'Express'),
        walkInTrip    = leg2Mode == 'Walk',
        busInTrip     = (leg1Mode == bus) | (leg2Mode == bus) |
                        (leg3Mode == bus),
        taxiInTrip = (leg1Mode == 'Taxi') | (leg3Mode == 'Taxi'),
        uberInTrip = (leg1Mode == 'Uber/Lyft') | (leg3Mode == 'Uber/Lyft'),
        # You can only have an express lane fee for car modes
        expressFee = ifelse(str_detect(leg1Mode, 'Car'), expressFee, 0)) %>%
    distinct()
ff$rowID <- seq(nrow(ff))

# Sample from ff to balance the numbers of trip legs and modes
ids <- list(
    car_legs1     = which(ff$carInTrip & ff$numLegs == 1),
    car_legs2     = which(ff$carInTrip & ff$numLegs == 2),
    car_legs3     = which(ff$carInTrip & ff$numLegs == 3),
    express_legs1 = which(ff$expressInTrip & ff$numLegs == 1),
    express_legs2 = which(ff$expressInTrip & ff$numLegs == 2),
    express_legs3 = which(ff$expressInTrip & ff$numLegs == 3),
    bus_legs1     = which(ff$busInTrip & ff$numLegs == 1),
    bus_legs2     = which(ff$busInTrip & ff$numLegs == 2),
    bus_legs3     = which(ff$busInTrip & ff$numLegs == 3),
    taxi_legs1    = which(ff$taxiInTrip & ff$numLegs == 1),
    taxi_legs2    = which(ff$taxiInTrip & ff$numLegs == 2),
    taxi_legs3    = which(ff$taxiInTrip & ff$numLegs == 3),
    uber_legs1    = which(ff$uberInTrip & ff$numLegs == 1),
    uber_legs2    = which(ff$uberInTrip & ff$numLegs == 2),
    uber_legs3    = which(ff$uberInTrip & ff$numLegs == 3),
    # walk_legs1    = which(ff$walkInTrip & ff$numLegs == 1), Doesn't exist
    walk_legs2    = which(ff$walkInTrip & ff$numLegs == 2),
    walk_legs3    = which(ff$walkInTrip & ff$numLegs == 3))
numSamples <- 10000
samples <- list()
for (i in 1:length(ids)) {
    samples[[i]] <- sample(x=ids[[i]], size=numSamples, replace=T)
}
ff_bal <- ff[unlist(samples),]

# Randomly sample from the ff_bal to evenly fit the desired sample size
nResp        <- 6000 # Number of respondents
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent
nRowsPerResp <- nAltsPerQ * nQPerResp
nRows        <- nResp*nRowsPerResp
doe <- ff_bal[sample(x=seq(nrow(ff_bal)), size=nRows, replace=T),]

# Make sure no two identical alts appear in one question, and set meta data
doe <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp) %>%
    # Add additional variables for plotting
    mutate(
        trip = ifelse(
            numLegs == 1, paste(leg1Mode), ifelse(
            numLegs == 2, paste(leg1Mode, leg2Mode, sep='|'),
            paste(leg1Mode, leg2Mode, leg3Mode, sep='|'))),
        leg2Mode = ifelse(walkTimeLeg > 0,
            paste(leg2Mode, '\n(', walkTimeLeg, ' min)', sep=''), leg2Mode),
        totalWalkTime = walkTimeStart + walkTimeLeg + walkTimeEnd,
        totalWaitTime = transfer1Time + transfer2Time + transfer3Time,
        totalTripTime = totalWalkTime + totalWaitTime + transitTime,
        tripTimeMin = round(totalTripTime*(1 - tripTimeUnc)),
        tripTimeMax = round(totalTripTime*(1 + tripTimeUnc)),
        tripTimeRange  = paste(
            tripTimeMin, '-', tripTimeMax, 'minutes', sep=' '))

# Save design
write_csv(doe, here::here('survey', 'pilot5', 'survey', 'doeAll.csv'))

# -----------------------------------------------------------------------------
# View summary plots of doe to check for mode and trip leg balance

ff$design <- 'ff'
plotDf <- doe %>%
    mutate(design = 'doe') %>%
    bind_rows(ff)

# Relatively even balance in number of legs in each trip:
barCompare(plotDf, numLegs)

# Balance in leg modes
barCompare(plotDf, leg1Mode)
barCompare(plotDf, leg2Mode)
barCompare(plotDf, leg3Mode)
barCompare(plotDf %>% filter(numLegs == 1), leg1Mode)
barCompare(plotDf %>% filter(numLegs == 2), leg2Mode)
barCompare(plotDf %>% filter(numLegs == 3), leg3Mode)

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
