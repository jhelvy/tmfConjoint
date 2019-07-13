library(ggplot2)

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
    car_price_drive  = c(10, 15, 20, 30), # USD $ Parking + Gas
    car_price_uber   = c(10, 15, 20, 30), # Ride Fare
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

# -----------------------------------------------------------------------------
# Make trip maps 

row <- ff2[1,]

getDf <- function(row) {
    x = c(0, 1)
    if (row$bus_numTransfers == 1) {
        x = c(0, 0.5, 1)
    } else if (row$bus_numTransfers == 2) {
        x = c(0, 0.33, 0.66, 1)
    } 
    return(data.frame(x = x, y = rep(1, length(x))))
}

ggplot(getDf(row), 
    aes(x = x, y = y)) +
    geom_point(size=2) + 
    geom_line() + 
    theme_bw()
    
