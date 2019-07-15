library(tidyverse)

# -----------------------------------------------------------------------------
# Mode-specific designs

car <- expand.grid(
    price           = c(5, 10, 15, 20, 30), # USD $ Parking + Gas
    travel_time     = c(10, 15, 20, 30, 45, 60), # Minutes
    travel_time_unc = c(0, 10, 20, 30, 40) # Percentage of time
)

bus <- expand.grid(
    price              = c(1, 3, 5, 9), # USD $ ticket fare
    travel_time        = c(20, 30, 45, 60, 75, 90), # Minutes
    travel_time_unc    = c(0, 10, 20, 30, 40), # Percentage of time
    num_transfers      = c(0, 1, 2, 3),
    transfer_wait_time = c(5, 10, 15, 20, 30) # Minutes
)

rideHail <- expand.grid(
    price           = c(10, 15, 20, 30), # USD $ Travel fare
    travel_time     = c(10, 15, 20, 30, 45, 60), # Minutes
    travel_time_unc = c(0, 10, 20, 30, 40) # Percentage of time
)

roll <- expand.grid(
    price           = c(1.50, 3.00, 5.00), # USD $ Rental fare
    travel_time     = c(5, 10, 15, 20), # Minutes
    travel_time_unc = c(0, 10, 20, 30, 40) # Percentage of time
)

walk <- expand.grid(
    travel_time     = c(5, 10, 15, 20), # Minutes
    travel_time_unc = c(0, 10, 20, 30, 40) # Percentage of time
)

# -----------------------------------------------------------------------------
# Public vs. Private designs

private <- expand.grid(
    mode        = c('Drive', 'Uber / Lyft'),
    price       = c(5, 10, 15, 20, 30), # USD $ Parking + Gas OR Ride Fare
    rideTime    = c(10, 15, 20, 30, 45, 60), # Minutes
    rideTimeUnc = c(0, 10, 20, 30) # Percentage of time
)

public <- expand.grid(
    price        = c(1, 3, 5, 9), # USD $ ticket fare
    rideTime     = c(20, 30, 45, 60, 75, 90), # Minutes
    rideTimeUnc  = c(0, 10, 20, 30, 40), # Percentage of time
    numTransfers = c(0, 1, 2),
    lastMileMode = c('Walk', 'Scooter / Bike ($1.50)',
                     'Scooter / Bike ($3.00)', 'Scooter / Bike ($5.00)'),
    lastMileTime = c(5, 10, 15) # Minutes
)

# -----------------------------------------------------------------------------
# Full factorial

ff1 <- expand.grid(
    car_price        = c(10, 15, 20, 30), # USD $ Parking + Gas
    uber_price       = c(10, 15, 20, 30), # Ride Fare
    car_rideTime     = c(1, 2, 3, 4), # Level (defined by map)
    car_rideTimeUnc  = c(0, 10, 20), # Percentage of time
    bus_price        = c(1, 3, 5, 9), # USD $ ticket fare
    bus_rideTime     = c(1, 2, 3, 4), # Level (defined by map)
    bus_rideTimeUnc  = c(0, 10, 20, 30), # Percentage of time
    bus_numTransfers = c(0, 1, 2),
    bus_lastMileMode = c('Walk', 'Scooter / Bike ($1.50)',
                         'Scooter / Bike ($3.00)', 'Scooter / Bike ($5.00)'),
    bus_lastMileTime = c(5, 10) # Minutes
)

ff2 <- expand.grid(
    car_price_drive  = c(10, 20, 30), # USD $ Parking + Gas
    car_price_uber   = c(10, 20, 30), # Ride Fare
    car_rideTime     = c(1, 2, 3), # Level determined by map
    car_rideTimeUnc  = c(0, 10, 20), # Percentage of time
    bus_price        = c(2, 5, 8), # USD $ ticket fare
    bus_rideTime     = c(1, 2, 3), # Level determined by map
    bus_rideTimeUnc  = c(0, 10, 20), # Percentage of time
    bus_numTransfers = c(0, 1, 2),
    bus_lastMileTime = c(5, 10) # Minutes
)

ff3 <- expand.grid(
    car_price        = c(10, 20, 30), # USD $ Parking + Gas
    car_rideTime     = c(1, 2, 3), # Level determined by map
    car_rideTimeUnc  = c(0.05, 0.1, 0.2), # Percentage of time
    uber_price       = c(10, 20, 30), # Ride Fare
    uber_waitTime    = c(5, 10), # Additional wait time for total trip
    uber_rideTimeUnc = c(0.05, 0.1, 0.2), # Percentage of time
    bus_price        = c(2, 5, 8), # USD $ ticket fare
    bus_rideTime     = c(1, 2, 3), # Level determined by map
    bus_rideTimeUnc  = c(0.05, 0.1, 0.2), # Percentage of time
    numTransfers     = c(0, 1, 2), # Number of bus transfers
    lastMileTime     = c(5, 10) # Minutes
)

# -----------------------------------------------------------------------------
# Translate full factorial into design for plotting

carTimes <- data.frame(
    car_rideTime = seq(3),
    car_time     = c(30, 45, 60)
)
busTimes <- data.frame(
    bus_rideTime = seq(3),
    bus_time     = c(45, 60, 75)
)
design <- ff3 %>%
    left_join(carTimes) %>%
    left_join(busTimes) %>%
    mutate(
        uber_time       = car_time + uber_waitTime,
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
    spread(var, val)

# -----------------------------------------------------------------------------
# Make trip maps

getPlotDf <- function(x) {return(data.frame(x = x, y = rep(1, length(x))))}

getPlotX <- function(row) {
    carDf = getPlotDf(x=c(0, 1))
    uberDf = getPlotDf(x=c(0, 1))
    busDf = getPlotDf(x=seq(0, 1, length.out=(unique(row$numTransfers) + 2)))
    plotDf = getPlotDf0()
    plotDf = data.frame(
        type = c('car', 'car', 'uber', 'uber'),
        x    = c(0, 1, 0, 1),
        y    = c(1, 1, 1, 1)
    )
    if (row$bus_numTransfers == 1) {
        x = c(0, 0.5, 1)
    } else if (row$bus_numTransfers == 2) {
        x = c(0, 0.33, 0.66, 1)
    }
    plotDf = data.frame(x = x, y = rep(1, length(x)))
    return(plotDf)
}



i <- 13123
row  <- filter(design, id == i)
plotDf <- getPlotDf(row)

carPlot <- ggplot(plotDf,
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void()
uberPlot <- ggplot(plotDf,
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void()
busPlot <- ggplot(plotDf,
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void()



ggplot(getPlotDf(x=c(0, 1)),
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.8, label = row[which(row$type == 'car'),]$time) +
    annotate("text", x = 0.5, y = 0.6, label = paste('$', row[which(row$type == 'car'),]$price, ' parking + gas', sep='')) +
    annotate("text", x = 0.5, y = 10, label = '')










