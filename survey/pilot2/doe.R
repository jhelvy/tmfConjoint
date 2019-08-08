library(tidyverse)
library(here)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

# -----------------------------------------------------------------------------
# Functions for balancing the DOE 

removeIllogicalTrips <- function(design) {
    design <- design %>% 
        # You only walk when taking the bus
        filter(! ((str_detect(trip, 'Bus') == F) & (walkTimeStart != 0))) %>% 
        filter(! ((str_detect(trip, 'Bus') == F) & (walkTimeEnd != 0))) %>% 
        # You only wait on a taxi / uber when they're in the mode set
        filter(! ((str_detect(trip, 'Uber') == F) & 
                  (str_detect(trip, 'Taxi') == F) & 
                  (taxiWaitTime != 0)))
    return(design)
}

getSampleIDs <- function(doe, numSamples) {
    hasCar  <- which(str_detect(doe$trip, 'Car'))
    hasTaxi <- which(str_detect(doe$trip, 'Taxi') | 
                         str_detect(doe$trip, 'Uber'))
    hasBus  <- which(str_detect(doe$trip, 'Bus'))
    car     <- sample(x=hasCar, size=numSamples, replace=T)
    taxi    <- sample(x=hasTaxi, size=numSamples, replace=T)
    bus     <- sample(x=hasBus, size=numSamples, replace=T)
    return(list(car=car, taxi=taxi, bus=bus))
}

# -----------------------------------------------------------------------------
# Main DOE construction - randomized, stratefied by number of trips and modes

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

# Make ff for trips of length 1 
ff1 <- expand.grid(
    trip          = trips1$trip,
    price         = c(10, 15, 20, 25, 30), # USD $
    tripTime      = c(30, 40, 50, 60, 70), # Minutes for whole trip
    tripTimeUnc   = c(0.05, 0.1, 0.2), # Plus/minus percentage of tripTime
    walkTimeStart = c(0, 5, 10), # Minutes
    walkTimeEnd   = c(0, 5, 10), # Minutes
    taxiWaitTime  = c(0, 5, 10), # Minutes
    busWaitTime   = c(0, 5, 10) # Minutes
) %>% removeIllogicalTrips()
ff1$id <- paste('1', seq(nrow(ff1)), sep='-')

# Make ff for trips of length 2
ff2 <- expand.grid(
    trip          = trips2$trip,
    price         = c(10, 15, 20, 25, 30), # USD $
    tripTime      = c(30, 40, 50, 60, 70), # Minutes for whole trip
    tripTimeUnc   = c(0.05, 0.1, 0.2), # Plus/minus percentage of tripTime
    walkTimeStart = c(0, 5, 10), # Minutes
    walkTimeEnd   = c(0, 5, 10), # Minutes
    taxiWaitTime  = c(0, 5, 10), # Minutes
    busWaitTime   = c(0, 5, 10) # Minutes
) %>% removeIllogicalTrips()
ff2$id <- paste('2', seq(nrow(ff2)), sep='-')

# Make ff for trips of length 3
ff3 <- expand.grid(
    trip          = trips3$trip,
    price         = c(10, 15, 20, 25, 30), # USD $
    tripTime      = c(30, 40, 50, 60, 70), # Minutes for whole trip
    tripTimeUnc   = c(0.05, 0.1, 0.2), # Plus/minus percentage of tripTime
    walkTimeStart = c(0, 5, 10), # Minutes
    walkTimeEnd   = c(0, 5, 10), # Minutes
    taxiWaitTime  = c(0, 5, 10), # Minutes
    busWaitTime   = c(0, 5, 10) # Minutes
) %>% removeIllogicalTrips()
ff3$id <- paste('3', seq(nrow(ff3)), sep='-')

# Sample from ff1, ff2, and ff3 for even numbers of trip legs and modes
N <- 6000
sampleIDs1 <- getSampleIDs(ff1, N)
sampleIDs2 <- getSampleIDs(ff2, N)
sampleIDs3 <- getSampleIDs(ff3, N)
doe1 <- ff1[unlist(sampleIDs1),]
doe2 <- ff2[unlist(sampleIDs2),]
doe3 <- ff3[unlist(sampleIDs3),]
doe <- rbind(doe1, doe2, doe3)
row.names(doe) <- seq(nrow(doe))

# Randomize alternatives
rows <- sample(x=seq(nrow(doe)), size=nrow(doe), replace=F)
doe  <- doe[rows,]

# -----------------------------------------------------------------------------
# Translate full factorial into design for plotting

# Compute trip time range and number of legs
doe <- doe %>%
    mutate(
        tripTimeRange  = paste(
            round(tripTime*(1 - tripTimeUnc)), '-',
            round(tripTime*(1 + tripTimeUnc)), 'minutes', sep=' '),
        numLegs = str_count(trip, '\\|') + 1)

# Set meta data
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent
nRowsPerResp <- nAltsPerQ * nQPerResp
nResp        <- nrow(doe) / nRowsPerResp # Number of respondents
doe$respID   <- rep(seq(nResp), each=nRowsPerResp)
doe$qID      <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
doe$altID    <- rep(seq(nAltsPerQ), nResp*nQPerResp)
doe$obsID    <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)

# Make sure no 2 same alts appear in one question
doe <- doe %>% 
    group_by(obsID) %>% 
    mutate(numUnique = n_distinct(id))
doubleRows <- which(doe$numUnique != 3)
while (length(doubleRows) != 0) {
    newRows <- sample(x=seq(nrow(doe)), size=length(doubleRows), replace=F)
    doe[doubleRows,] <- doe[newRows,]
    doe <- doe %>% 
        group_by(obsID) %>% 
        mutate(numUnique = n_distinct(id))
    doubleRows <- which(doe$numUnique != 3)
}
doe <- select(doe, -id, -numUnique)

# Save design
write_csv(doe, here::here('survey', 'pilot2', 'doe.csv'))