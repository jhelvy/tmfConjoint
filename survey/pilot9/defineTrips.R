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
# Car         -> Walk
# Car express -> Walk
# Car         -> Uber/Taxi  # NO, you're already driving
# Car express -> Uber/Taxi  # NO, you're already driving
# Bus         -> Bus
# Walk        -> Bus
# Uber/Taxi   -> Bus
# Bus         -> Walk
# Walk        -> Walk       # NO, you're already walking
# Uber/Taxi   -> Walk
# Bus         -> Uber/Taxi
# Walk        -> Uber/Taxi  # NO, you would just taxi from the start
# Uber/Taxi   -> Uber/Taxi  # NO, you're already in a taxi

# 3 LEG TRIPS
# Bus       -> Bus       -> Bus
# Walk      -> Bus       -> Bus
# Uber/Taxi -> Bus       -> Bus        # NO, you would taxi to the 2nd bus stop
# Bus       -> Walk      -> Bus
# Uber/Taxi -> Walk      -> Bus        # NO, you would uber to the bus stop
# Bus       -> Uber/Taxi -> Bus        # NO, you would taxi to the end
# Bus       -> Bus       -> Walk
# Walk      -> Bus       -> Walk
# Uber/Taxi -> Bus       -> Walk
# Bus       -> Walk      -> Walk       # NO, you're already walking
# Uber/Taxi -> Walk      -> Walk       # NO, you're already walking
# Bus       -> Uber/Taxi -> Walk       # NO, you would taxi to the end
# Bus       -> Bus       -> Uber/Taxi  # NO, you would taxi from the next stop
# Walk      -> Bus       -> Uber/Taxi
# Uber/Taxi -> Bus       -> Uber/Taxi  # NO, you would taxi to the end
# Bus       -> Walk      -> Uber/Taxi
# Uber/Taxi -> Walk      -> Uber/Taxi  # NO, you would taxi to the end
# Bus       -> Uber/Taxi -> Uber/Taxi  # NO, you're already in a taxi

# NO to all 3-leg car trips - you would just drive to the end

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
    paste(car, walk, sep = '|'),        'car',
    paste(express, walk, sep = '|'),    'car',
    paste(bus, bus, sep = '|'),         'bus', 
    paste(walk, bus, sep = '|'),        'bus', 
    paste(taxi, bus, sep = '|'),        'taxi-bus', 
    paste(bus, walk, sep = '|'),        'bus',
    paste(taxi, walk, sep = '|'),       'taxi',
    paste(bus, taxi, sep = '|'),        'taxi-bus', 
    # 3 LEG TRIPS
    paste(bus, bus, bus, sep = '|'),    'bus',
    paste(walk, bus, bus, sep = '|'),   'bus',
    paste(bus, walk, bus, sep = '|'),   'bus',
    paste(bus, bus, walk, sep = '|'),   'bus',
    paste(walk, bus, walk, sep = '|'),  'bus',
    paste(taxi, bus, walk, sep = '|'),  'taxi-bus',
    paste(walk, bus, taxi, sep = '|'),  'taxi-bus',
    paste(bus, walk, taxi, sep = '|'),  'taxi-bus')

trips <- expand.grid(
    leg1Mode = c(taxi, bus, walk, car, express),
    leg2Mode = c(none, taxi, bus, walk),
    leg3Mode = c(none, taxi, bus, walk)) %>% 
    mutate(
        numLegs = ifelse(
            leg2Mode == none, 1, ifelse(
            leg3Mode == none, 2, 3)),
        leg3Mode = ifelse(
            numLegs %in% c(1, 2), none, as.character(leg3Mode)),
        lastLegMode = ifelse(
            numLegs == 1, as.character(leg1Mode), ifelse(
            numLegs == 2, as.character(leg2Mode), as.character(leg3Mode))),
        trip = ifelse(
            numLegs == 1, paste(leg1Mode), ifelse(
            numLegs == 2, paste(leg1Mode, leg2Mode, sep='|'),
            paste(leg1Mode, leg2Mode, leg3Mode, sep='|'))),
        carInTrip     = str_detect(trip, car),
        expressInTrip = str_detect(trip, express),
        walkInTrip    = str_detect(trip, walk),
        busInTrip     = str_detect(trip, bus),
        taxiInTrip    = str_detect(trip, taxi)) %>%
    filter(trip %in% goodTrips$trip) %>% 
    left_join(goodTrips) %>% 
    distinct() # Remove duplicates that may now be remaining
