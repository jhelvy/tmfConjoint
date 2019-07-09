# -----------------------------------------------------------------------------
# Setup

library(tidyverse)
library(rvest)
library(mapsapi)
options(dplyr.width = Inf) # Option to preview all columns in a data frame
source(file.path('code', 'functions.R'))

# -----------------------------------------------------------------------------

# Attributes & Levels
mode_price <- c('Bus ($1)', 'Bus ($3)', 'Bus ($5)', 'Bus ($9)',
              'Car (Free Parking)', 'Car ($5 Parking)', 'Car ($10 Parking)',
              'Car ($15 Parking)', 'Car ($20 Parking)',
              'Scooter / Bicycle ($1.50)', 'Scooter / Bicycle ($3.00)',
              'Scooter / Bicycle ($5.00)', 'Uber / Lyft ($10)',
              'Uber / Lyft ($15)', 'Uber / Lyft ($20)',
              'Uber / Lyft ($30)', 'Walking ')
leg_travel_time     <- c(10, 15, 20, 30, 45, 60) # Minutes
leg_travel_time_unc <- c(0, 10, 20, 30, 40) # Percentage of time
num_transfers       <- c(0, 1, 2, 3)
transfer_wait_time  <- c(5, 10, 15, 20, 30) # Minutes

fullFactorial <- expand.grid(
    mode_price, leg_travel_time, leg_travel_time_unc, num_transfers, 
    transfer_wait_time)

