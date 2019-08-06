library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# Main DOE

trips1 <- expand.grid(
    trip = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'))
trips2 <- expand.grid(
    leg1 = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'),
    leg2 = 'Bus') %>%
    mutate(trip = paste(leg1, leg2, sep='|Transfer|')) %>%
    select(trip)
trips3 <- expand.grid(
    leg1 = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'),
    leg2 = 'Bus',
    leg3 = c('Uber/Lyft', 'Taxi', 'Bus')) %>%
    mutate(trip = paste(leg1, leg2, leg3, sep='|Transfer|') )%>%
    select(trip)
trip <- rbind(trips1, trips2, trips3)
trip <- as.character(trip$trip)

ff <- expand.grid(
    trip          = trip,
    price         = c(10, 15, 20, 25, 30), # USD $
    tripTime      = c(30, 40, 50, 60, 70), # Minutes for whole trip
    tripTimeUnc   = c(0.05, 0.1, 0.2), # Plus/minus percentage of tripTime
    walkTimeStart = c(0, 5, 10), # Minutes
    walkTimeEnd   = c(0, 5, 10), # Minutes
    taxiWaitTime  = c(5, 10), # Minutes
    busWaitTime   = c(5, 10) # Minutes
)

# -----------------------------------------------------------------------------
# Translate full factorial into design for plotting

# Compute trip time range and number of legs
doe <- ff %>%
    mutate(
        tripTimeRange  = paste(
            round(tripTime*(1 - tripTimeUnc)), '-',
            round(tripTime*(1 + tripTimeUnc)), 'minutes', sep=' '),
        numLegs = str_count(trip, '\\|') + 1)
        


    if (trip$taxiWaitTime > 0) {
        if ('Uber/Lyft' %in% labels) {
            uberId <- which(labels == 'Uber/Lyft') - 1
            labels[uberId] <- paste(labels[uberId], ' (', trip$taxiWaitTime, ' min wait)', sep='')
        } 
        if ('Taxi' %in% labels) {
            taxiId <- which(labels == 'Taxi') - 1
            labels[taxiId] <- paste(labels[taxiId], ' (', trip$taxiWaitTime, ' min wait)', sep='')
        }
    }
    if (trip$busWaitTime > 0) {
        if ('Bus' %in% labels) {
            busId <- which(labels == 'Bus') - 1
            labels[busId] <- paste(labels[busId], ' (', trip$busWaitTime, ' min wait)', sep='')
        } 
    }



# Randomize design 
doe <- doe[sample(x=seq(nrow(doe)), size=nrow(doe), replace=F),]

# Set meta data
nAltsPerQ    <- 3 # Number of alternatives per question
nQPerResp    <- 6 # Number of questions per respondent
nRowsPerResp <- nAltsPerQ * nQPerResp
nResp        <- nrow(doe) / nRowsPerResp # Number of respondents
doe$respID   <- rep(seq(nResp), each=nRowsPerResp)
doe$qID      <- rep(rep(seq(nQPerResp), each=nAltsPerQ), nResp)
doe$altID    <- rep(seq(nAltsPerQ), nResp*nQPerResp)
doe$obsID    <- rep(seq(nResp * nQPerResp), each=nAltsPerQ)

# Save design
write_csv(doe, here::here('survey', 'pilot2', 'doe.csv'))
