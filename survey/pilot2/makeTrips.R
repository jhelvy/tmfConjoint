library(here)
source(here::here('survey', 'pilot2', 'functions.R'))

# -----------------------------------------------------------------------------
# Read in the doe and convert it to individual trips 

doe <- read_csv(here::here('survey', 'pilot2', 'doe', 'doe.csv')) %>%
    # Add transfers, start, and end labels
    mutate(
        trip = str_replace_all(trip, '\\|', '|Transfer|'),
        trip = paste('Start|', trip, '|End', sep=''))
    
# Create trips
tripDfList <- list()
for (i in 1:nrow(doe)) {
    tripDfList[[i]] <- getTripDf(doe[i,])
}

saveRDS(tripDfList, here::here('survey', 'pilot2', 'tripDfList.Rds'))
