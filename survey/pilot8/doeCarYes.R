# setwd('/Users/jhelvy/gh/tmfConjoint/')

library(here)
source(here::here('survey', 'pilot8', 'doeSettings.R'))

# -----------------------------------------------------------------------------
# Main DOE construction - randomized, stratified by number of trips and modes

# Generate full factorial
ff <- as_tibble(expand.grid(
    leg1Mode      = c(uber, taxi, bus, walk, car, express),
    leg2Mode      = c(none, uber, taxi, bus, walk),
    leg3Mode      = c(none, uber, taxi, bus, walk),
    price         = c(2, 5, 10, 15, 20, 25, 30), # Full trip, USD $
    expressFee    = c(5, 10, 15), # USD $
    leg1Time      = c(10, 15, 20, 25, 30, 40), # Minutes
    leg2Time      = c(10, 15, 20), # Minutes
    leg3Time      = c(10, 15, 20), # Minutes
    transfer1Time = c(2, 5, 10), # Minutes
    transfer2Time = c(2, 5, 10), # Minutes
    transfer3Time = c(2, 5, 10), # Minutes
    tripTimeUnc   = c(0.05, 0.10, 0.20)) # Plus/minus % of total trip time
)

# Filter out nonsensical alternatives and add some helpful variables
FF <- ff %>%
    walkSpecificCleaning() %>% 
    fixNoneCases() %>%
    addSummaryVars() %>%
    carSpecificCleaning() %>%
    addTimeSummary() %>% 
    filterCases() 
FF$rowID <- seq(nrow(FF))

# Get a balanced set of trips by mode and numLegs
trips <- getBalancedTrips(FF,
    modes = c('taxi', 'walk', 'bus', 'car'),
    # thresholds are differences in count for mode and numLegs
    thresholds = c('mode' = 5, 'leg' = 2))

# Use the resulting proportions of unique trips to select rows for DOE
FF_bal <- getBalancedFF(FF, trips)

# Randomly sample from the FF_bal to evenly fit the desired sample size
nRowsPerResp <- nAltsPerQ*nQPerResp
nRows        <- nResp*nRowsPerResp
doeIDs       <- sample(x=seq(nrow(FF_bal)), size=nRows, replace=T)
doe          <- FF_bal[doeIDs,]

# Make sure no two identical alts appear in one question
doe <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp)

# Save design
write_csv(doe, here::here('survey', 'pilot8', 'survey', 'doeCarYes.csv'))

# Compare balance of modes:
doe %>%
    mutate(
        car  = ifelse(carInTrip | expressInTrip, T, F),
        taxi = ifelse(taxiInTrip | uberInTrip, T, F),
        bus  = busInTrip,
        walk = walkInTrip
    ) %>%
    gather(mode, count, car:walk) %>%
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
