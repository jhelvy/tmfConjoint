library(here)
source(here::here('survey', 'pilot2', 'functions.R'))

# -----------------------------------------------------------------------------
# Main DOE construction - randomized, stratified by number of trips and modes

# Get unique trip combinations
trips1 <- data.frame(trip = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'))
trips2 <- trips1 %>% mutate(trip = paste(trip, '|Bus', sep=''))
trips3 <- expand.grid(
    leg1 = c('Car', 'Uber/Lyft', 'Bus'),
    leg2 = 'Bus',
    leg3 = c('Uber/Lyft', 'Bus')) %>%
    mutate(trip = paste(leg1, leg2, leg3, sep='|') )%>%
    select(trip)
# Add taxi options for trips3
taxi3 <- trips3 %>%
    filter(str_detect(trip, 'Uber')) %>%
    mutate(trip = str_replace_all(trip, 'Uber/Lyft', 'Taxi'))
trips3 <- rbind(trips3, taxi3)

# Set general attributes
atts <- list(
    price         = c(10, 15, 20, 25, 30), # USD $
    travelTime    = c(20, 30, 40, 50, 60), # Minutes for time in motion
    tripTimeUnc   = c(0.05, 0.1, 0.2), # Plus/minus percentage of totalTripTime
    walkTimeStart = c(2, 5, 10), # Minutes
    walkTimeEnd   = c(2, 5, 10), # Minutes
    taxiWaitTime  = c(2, 5, 10), # Minutes
    busWaitTime   = c(2, 5, 10)  # Minutes
)

# Make ff for trips of length 1
ff1 <- makeFullFactorial(trips1$trip, atts)
ff1$rowID <- paste('1', seq(nrow(ff1)), sep='-')

# Make ff for trips of length 2
ff2 <- makeFullFactorial(trips2$trip, atts)
ff2$rowID <- paste('2', seq(nrow(ff2)), sep='-')

# Make ff for trips of length 3
ff3 <- makeFullFactorial(trips3$trip, atts)
ff3$rowID <- paste('3', seq(nrow(ff3)), sep='-')

# Sample from ff1, ff2, and ff3 for even numbers of trip legs and modes
N <- 6000
sampleIDs1 <- getSampleIDs(ff1, N)
sampleIDs2 <- getSampleIDs(ff2, N)
sampleIDs3 <- getSampleIDs(ff3, N)
doe1 <- ff1[unlist(sampleIDs1),]
doe2 <- ff2[unlist(sampleIDs2),]
doe3 <- ff3[unlist(sampleIDs3),]
doe <- rbind(doe1, doe2, doe3)

# Randomize alternatives
rows <- sample(x=seq(nrow(doe)), size=nrow(doe), replace=F)
doe  <- doe[rows,]

# -----------------------------------------------------------------------------
# Translate full factorial into design for plotting

# Compute trip time range
doe <- doe %>%
    mutate(
        nBus  = str_count(trip, 'Bus'),
        nTaxi = str_count(trip, 'Taxi') + str_count(trip, 'Uber'),
        totalWalkTime = walkTimeStart + walkTimeEnd,
        totalWaitTime = taxiWaitTime*nTaxi + busWaitTime*nBus,
        totalTripTime = totalWalkTime + totalWaitTime + travelTime,
        tripTimeMin = round(totalTripTime*(1 - tripTimeUnc)),
        tripTimeMax = round(totalTripTime*(1 + tripTimeUnc)),
        tripTimeRange  = paste(
            tripTimeMin, '-', tripTimeMax, 'minutes', sep=' '))

# Make sure no 2 same alts appear in one question, and set meta data
nAltsPerQ <- 3 # Number of alternatives per question
nQPerResp <- 6 # Number of questions per respondent
doe       <- removeDoubleAlts(doe, nAltsPerQ, nQPerResp)

# Save design
write_csv(doe, here::here('survey', 'pilot2', 'doe', 'doe.csv'))

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

# -----------------------------------------------------------------------------
# Read in trip list and save all trips for each respondent as a csv file

tripDfList <- readRDS(here::here('survey', 'pilot2', 'doe', 'tripDfList.Rds'))

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
