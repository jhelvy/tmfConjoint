library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# Main DOE

trips1 <- expand.grid(
    trip = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'))
trips2 <- expand.grid(
    leg1 = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'),
    leg2 = 'Bus') %>%
    mutate(trip = paste(leg1, leg2, sep=' | ')) %>%
    select(trip)
trips3 <- expand.grid(
    leg1 = c('Car', 'Uber/Lyft', 'Taxi', 'Bus'),
    leg2 = 'Bus',
    leg3 = c('Uber/Lyft', 'Taxi', 'Bus')) %>%
    mutate(trip = paste(leg1, leg2, leg3, sep=' | ') )%>%
    select(trip)
trip <- rbind(trips1, trips2, trips3)
trip <- as.character(trip$trip)

ff <- expand.grid(
    trip         = trip,
    price        = c(5, 10, 15, 20, 25, 30), # USD $
    tripTime     = c(20, 30, 40, 50, 60, 70, 80), # Minutes for whole trip
    tripTimeUnc  = c(0.05, 0.1, 0.2), # Plus/minus percentage of tripTime
    walkTime     = c(5, 10, 15), # Minutes
    taxiWaitTime = c(5, 10), # Minutes
    busWaitTime  = c(5, 10) # Minutes
)

# -----------------------------------------------------------------------------
# Translate full factorial into design for plotting

design <- ff %>%
    mutate(
        uber_time = car_time + uber_waitTime,
        car_time  = paste(
            round(car_time*(1 - car_rideTimeUnc)), 'to',
            round(car_time*(1 + car_rideTimeUnc)), 'minutes', sep=' '),
        uber_time = paste(
            round(uber_time*(1 - uber_rideTimeUnc)), 'to',
            round(uber_time*(1 + uber_rideTimeUnc)), 'minutes', sep=' '),
        bus_time = paste(
            round(bus_time*(1 - bus_rideTimeUnc)), 'to',
            round(bus_time*(1 + bus_rideTimeUnc)), 'minutes', sep=' '),
        id = seq(n())) %>%
    select(
        id, lastMileTime, numTransfers, car_price, car_time, uber_price,
        uber_time, bus_price, bus_time) %>%
    gather(type, val, car_price:bus_time) %>%
    separate(type, c('type', 'var'), sep='_') %>%
    arrange(id) %>%
    spread(var, val) %>%
    select(id, type, price, time, lastMileTime, numTransfers)

design %>%
    distinct(type, time) %>%
    arrange(type, time)

write_csv(design, here::here('survey', 'survey_doe.csv'))







