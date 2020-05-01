# Main full factorial DOE construction
# randomized, stratified by number of trips and modes

source(here::here('survey', 'pilot10', 'functions.R'))
source(here::here('survey', 'pilot10', 'defineTrips.R'))

# Get a balanced set of trips by number of legs and trip types
trips <- getBalancedTrips(trips, trips, thresholds = list(
    tripType = 1, numLegs = 1)) %>%
    mutate(tripId = row_number())

dim(trips)
getDiffs(trips)
trips %>% count(tripType)
trips %>% count(numLegs)
trips %>%
    count(trip) %>%
    arrange(desc(n)) %>%
    as.data.frame()

# Generate full factorial for all attributes except for trips
ff <- as_tibble(expand.grid(
    price         = c(2, 5, 10, 15, 20, 25, 30), # Full trip, USD $
    expressFee    = c(5, 10, 15), # USD $
    leg1Time      = c(10, 15, 20, 25, 30, 40), # Minutes
    leg2Time      = c(10, 15, 20), # Minutes
    transfer1Time = c(2, 5, 10), # Minutes
    transfer2Time = c(2, 5, 10), # Minutes
    tripTimeUnc   = c(0.05, 0.10, 0.20)) # Plus/minus % of total trip time
) %>%
    mutate(ffId = row_number())

# Add trips to the full factorial
ff <- expand.grid(
    ffId   = ff$ffId,
    tripId = trips$tripId) %>%
    left_join(trips) %>%
    left_join(ff) %>%
    select(-ffId)

# Filter out nonsensical alternatives and add some helpful variables
ff_bal <- ff %>%
    walkSpecificCleaning() %>%
    fixNoneCases() %>%
    addTimeSummary() %>%
    carSpecificCleaning() %>%
    filterCases() %>%
    getBalancedFF() # Add repeated rows to balance the FF based on the tripId

# Save result
saveRDS(ff_bal, here::here(
    'survey', 'pilot10', 'survey', 'doe', 'ff_balanced_yes.Rds'))
