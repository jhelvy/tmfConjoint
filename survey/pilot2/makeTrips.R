library(here)
source(here::here('survey', 'pilot2', 'functions.R'))

# -----------------------------------------------------------------------------
# Read in the doe and convert it to individual trips

doe <- read_csv(here::here('survey', 'pilot2', 'doe', 'doe.csv'))

# Create trips
tripDfList <- list()
for (i in 1:nrow(doe)) {
    tripDfList[[i]] <- getTripDf(doe[i,])
}

# Save trip list
saveRDS(tripDfList, here::here('survey', 'pilot2', 'doe', 'tripDfList.Rds'))
# tripDfList <- readRDS(here::here('survey', 'pilot2', 'doe', 'tripDfList.Rds'))

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
        write_csv(tripDf, here::here('survey', 'pilot2', 'doe', 'trips',
                                     paste(respID, '.csv', sep='')))
        # Start a new temp list
        respID <- trip$respID[1]
        index <- 2
        temp <- list(trip)
    }
}
