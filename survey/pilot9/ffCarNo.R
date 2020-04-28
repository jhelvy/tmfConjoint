# Main full factorial DOE construction
# randomized, stratified by number of trips and modes

source(here::here('survey', 'pilot9', 'functions.R'))
source(here::here('survey', 'pilot9', 'defineModes.R'))

# Generate full factorial
ff <- as_tibble(expand.grid(
    leg1Mode      = c(uber, taxi, bus, walk),
    leg2Mode      = c(none, uber, taxi, bus, walk),
    leg3Mode      = c(none, uber, taxi, bus, walk),
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
    walkSpecificCleaning() %>%
    fixNoneCases() %>%
    addSummaryVars() %>%
    addTimeSummary() %>%
    filterCases()
FF$rowID <- seq(nrow(FF))

# Get a balanced set of trips by mode and numLegs
trips <- getBalancedTrips(FF,
    modes = c('taxi', 'walk', 'bus'),
    # thresholds are differences in count for mode and numLegs
    thresholds = c('mode' = 2, 'leg' = 2))

# Use the resulting proportions of unique trips to select rows for DOE
FF_bal <- getBalancedFF(FF, trips)

# Save result
saveRDS(FF_bal, here::here(
    'survey', 'pilot9', 'survey', 'doe', 'ff_balanced_no.Rds'))
