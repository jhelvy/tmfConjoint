# Main full factorial DOE construction
# randomized, stratified by number of trips and modes

source(here::here('survey', 'pilot9', 'functions.R'))
source(here::here('survey', 'pilot9', 'defineTrips.R'))

getBalancedTrips <- function(trips, thresholds, hasCar = TRUE) {
    if (! hasCar) {
        trips <- trips %>% 
            filter(! carInTrip, ! expressInTrip)
    }
    trips_orig <- trips
    count <- 0
    diffs <- getDiffs(trips)
    while ((diffs$type    > thresholds$type) |
           (diffs$numLegs > thresholds$numLegs)) {
        trips_new <- addNewTrip(trips, diffs, trips_orig)
        if (nrow(trips_new) > nrow(trips)) {
            count <- 0
            trips <- trips_new
        } else {
            count <- count + 1
        }
        if (count > 100) {
            return(trips)
        }
    }
}
    
getDiffs <- function(trips) {
    type <- trips %>% 
        count(type) %>% 
        mutate(diff = max(n) - n)
    numLegs <- trips %>% 
        count(numLegs) %>% 
        mutate(diff = max(n) - n)
    diff_type <- max(type$diff)
    diff_numLegs <- max(numLegs$diff)
    return(list(type = diff_type, numLegs = diff_numLegs))
}
    
# Adds a random new trip from the unique trip set
# If the new trip helps balance the type and numLegs, keep it, otherwise
# return the original trips
addNewTrip <- function(trips, diffs, trips_orig) {
    newTrip <- trips_orig[sample(seq(nrow(trips_orig)), 1),]
    temp <- bind_rows(trips, newTrip)
    diffs_new <- getDiffs(temp)
    if ((diffs_new$type    < diffs$type) |
        (diffs_new$numLegs < diffs$numLegs)) {
        return(temp)
    }
    return(trips)
}

# Get balanced trips
trips <- getBalancedTrips(
    trips, hasCar = FALSE, 
    thresholds = list(type = 2, numLegs = 2))
getDiffs(trips)

# Generate full factorial
ff <- as_tibble(expand.grid(
    leg1Mode      = c(taxi, bus, walk),
    leg2Mode      = c(none, taxi, bus, walk),
    leg3Mode      = c(none, taxi, bus, walk),
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
    thresholds = c('mode' = 2, 'leg' = 1))

# Use the resulting proportions of unique trips to select rows for DOE
FF_bal <- getBalancedFF(FF, trips)

# Save result
saveRDS(FF_bal, here::here(
    'survey', 'pilot9', 'survey', 'doe', 'ff_balanced_no.Rds'))
