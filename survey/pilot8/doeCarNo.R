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
FF$rowID <- seq(nrow(FF))

# Get a balanced set of trips by mode and numLegs
trips <- getBalancedTrips(FF,
    modes = c('taxi', 'walk', 'bus'),
    # thresholds are differences in count for mode and numLegs
    thresholds = c('mode' = 2, 'leg' = 2))

# Use the resulting proportions of unique trips to select rows for DOE
FF_bal <- getBalancedFF(FF, trips)

# Randomly sample from the FF_bal to evenly fit the desired sample size
nResp        <- 6000 # Number of respondents
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent
nRowsPerResp <- nAltsPerQ * nQPerResp
nRows        <- nResp*nRowsPerResp
doe <- FF_bal[sample(x=seq(nrow(FF_bal)), size=nRows, replace=T),]

# Make sure no two identical alts appear in one question
doe <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp)

# Save design
write_csv(doe, here::here('survey', 'pilot8', 'survey', 'doeCarNo.csv'))

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
    count(numLegs) %>%
    mutate(p = 100*(n / sum(n))) %>%
    ggplot() +
    geom_col(aes(x = numLegs, y = p))

# -----------------------------------------------------------------------------
# Read in the doe and convert it to individual trips

doe <- read_csv(here::here('survey', 'pilot8', 'survey', 'doeCarNo.csv'))

# Create trips
tripDfList <- list()
for (i in 1:nrow(doe)) {
    tripDfList[[i]] <- getTripDf(doe[i,])
}

# Save trip list
saveRDS(tripDfList,
    here::here('survey', 'pilot8', 'survey', 'tripDfListCarNo.Rds'))

# Create and save data frame of tripDfs
tripDfList <- readRDS(
    here::here('survey', 'pilot8', 'survey', 'tripDfListCarNo.Rds'))

tripDfs <- data.table::rbindlist(tripDfList)
write_csv(tripDfs,
    here::here('survey', 'pilot8', 'survey', 'tripDfsCarNo.csv'))
