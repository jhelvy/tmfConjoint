# setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot8', 'functions.R'))
source(here::here('survey', 'pilot8', 'filterCases.R'))

# -----------------------------------------------------------------------------
# Main DOE construction - randomized, stratified by number of trips and modes

# List of leg modes in this DOE:
# 'Uber/Lyft'
# 'Taxi'
# 'Bus'
# 'Walk'
# 'None'

# Generate full factorial
ff <- as_tibble(expand.grid(
    leg1Mode      = c('Uber/Lyft', 'Taxi', 'Bus', 'Walk'),
    leg2Mode      = c('None', 'Uber/Lyft', 'Taxi', 'Bus', 'Walk'),
    leg3Mode      = c('None', 'Uber/Lyft', 'Taxi', 'Bus', 'Walk'),
    price         = c(2, 5, 10, 15, 20, 25, 30), # Full trip, USD $
    leg1Time      = c(10, 15, 20, 25, 30), # Minutes
    leg2Time      = c(10, 15, 20), # Minutes
    leg3Time      = c(10, 15, 20), # Minutes
    transfer1Time = c(2, 5, 10), # Minutes
    transfer2Time = c(2, 5, 10), # Minutes
    transfer3Time = c(2, 5, 10), # Minutes
    tripTimeUnc   = c(0.05, 0.10, 0.20)) # Plus/minus % of total trip time
)

# Filter out nonsensical alternatives and add some helpful variables
FF <- ff %>%
    fixNoneCases() %>%
    addSummaryVars() %>%
    filterCases()




ff$rowID <- seq(nrow(ff))

# Sample from ff to balance the mode alternatives and legs:
# First get the unique trip combinations
trips <- ff %>%
    distinct(trip, walkInTrip, busInTrip, taxiInTrip, uberInTrip, numLegs) %>%
    mutate(
        taxi = ifelse(taxiInTrip | uberInTrip, T, F),
        bus  = busInTrip,
        walk = walkInTrip
    ) %>%
    select(trip, numLegs, taxi, walk, bus)
# Now add random samples from "trips" to balance it by mode and numLegs
trips <- balanceTrips(
    trips,
    modes = c('taxi', 'walk', 'bus'),
    thresholds = c('mode'=8, 'leg'=2))

# Use the resulting proportions of unique trips to select rows for DOE
ff_bal <- getBalancedFF(ff, trips)

# Randomly sample from the ff_bal to evenly fit the desired sample size
nResp        <- 6000 # Number of respondents
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent
nRowsPerResp <- nAltsPerQ * nQPerResp
nRows        <- nResp*nRowsPerResp
doe <- ff_bal[sample(x=seq(nrow(ff_bal)), size=nRows, replace=T),]

# Make sure no two identical alts appear in one question
doe <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp)

# Save design
write_csv(doe, here::here('survey', 'pilot7', 'survey', 'doeNoCar.csv'))

# Compare balance of modes:
doe %>%
    mutate(
        taxi = ifelse(taxiInTrip | uberInTrip, T, F),
        bus  = busInTrip,
        walk = walkInTrip
    ) %>%
    gather(mode, count, taxi:walk) %>%
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

doe <- read_csv(here::here('survey', 'pilot7', 'survey', 'doeNoCar.csv'))

# Create trips
tripDfList <- list()
for (i in 1:nrow(doe)) {
    tripDfList[[i]] <- getTripDf(doe[i,])
}

# Save trip list
saveRDS(tripDfList, here::here('survey', 'pilot7', 'survey',
                               'tripDfListNoCar.Rds'))

# Create and save data frame of tripDfs
tripDfList <- readRDS(here::here('survey', 'pilot7', 'survey',
                                 'tripDfListNoCar.Rds'))

tripDfs <- data.table::rbindlist(tripDfList)
write_csv(tripDfs, here::here('survey', 'pilot7', 'survey',
    'tripDfListNoCar.csv'))
