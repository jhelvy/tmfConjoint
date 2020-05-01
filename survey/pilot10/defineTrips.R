# Define global leg mode vars:
car     = 'Car'
express = 'Car:\nExpress'
taxi    = 'Uber/Taxi'
bus     = 'Bus'
walk    = 'Walk'
none    = 'None'

# List of possible trips - those marked "NO" will be filtered out.
# Below I list every single possible trip out to 3 legs,
# and if a case is filtered out I explain why.

# 1 LEG TRIPS
# Car
# Car express
# Bus
# Walk        # NO, we're not interested in walking only
# Uber/Taxi

# 2 LEG TRIPS
# Car         -> Bus
# Car express -> Bus
# Car         -> Walk       # NO, you'd just drive the whole way
# Car express -> Walk       # NO, you'd just drive the whole way
# Car         -> Uber/Taxi  # NO, you're already driving
# Car express -> Uber/Taxi  # NO, you're already driving
# Bus         -> Bus
# Walk        -> Bus
# Uber/Taxi   -> Bus
# Bus         -> Walk
# Walk        -> Walk       # NO, you're already walking
# Uber/Taxi   -> Walk       # NO, you'd just taxi the whole way
# Bus         -> Uber/Taxi
# Walk        -> Uber/Taxi  # NO, you would just taxi from the start
# Uber/Taxi   -> Uber/Taxi  # NO, you're already in a taxi

goodTrips <- tribble(
    ~trip,                             ~tripType,
    # 1 LEG TRIPS
    car,                                'car',
    express,                            'car',
    bus,                                'bus',
    taxi,                               'taxi',
    # 2 LEG TRIPS
    paste(car, bus, sep = '|'),         'car-bus',
    paste(express, bus, sep = '|'),     'car-bus',
    paste(bus, bus, sep = '|'),         'bus', 
    paste(walk, bus, sep = '|'),        'bus', 
    paste(taxi, bus, sep = '|'),        'taxi-bus', 
    paste(bus, walk, sep = '|'),        'bus',
    paste(bus, taxi, sep = '|'),        'taxi-bus')

trips <- expand.grid(
    leg1Mode = c(taxi, bus, walk, car, express),
    leg2Mode = c(none, taxi, bus, walk)) %>% 
    mutate(
        numLegs = ifelse(leg2Mode == none, 1, 2),
        leg2Mode = ifelse(numLegs == 1, none, as.character(leg2Mode)),
        lastLegMode = ifelse(
            numLegs == 1, as.character(leg1Mode), as.character(leg2Mode)),
        trip = ifelse(
            numLegs == 1, paste(leg1Mode), paste(leg1Mode, leg2Mode, sep='|')),
        carInTrip     = str_detect(trip, car),
        expressInTrip = str_detect(trip, express),
        walkInTrip    = str_detect(trip, walk),
        busInTrip     = str_detect(trip, bus),
        taxiInTrip    = str_detect(trip, taxi)) %>%
    filter(trip %in% goodTrips$trip) %>% 
    left_join(goodTrips) %>% 
    distinct() # Remove duplicates that may now be remaining
