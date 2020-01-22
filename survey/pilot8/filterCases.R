# List of possible trips - those marked "NO" will be filtered out.
# Below I list every single possible trip out to 3 legs,
# and if a case is filtered out I explain why.

# 1 LEG TRIPS
# Bus
# Walk                            # NO, we're not interested in walking only
# Uber/Lyft
# Taxi

# 2 LEG TRIPS
# Bus       -> Bus
# Walk      -> Bus
# Uber/Lyft -> Bus
# Taxi      -> Bus
# Bus       -> Walk
# Walk      -> Walk               # NO, you're already walking
# Uber/Lyft -> Walk
# Taxi      -> Walk
# Bus       -> Uber/Lyft
# Walk      -> Uber/Lyft          # NO, you would just uber from the start
# Uber/Lyft -> Uber/Lyft          # NO, you're already in an uber
# Taxi      -> Uber/Lyft          # NO, you wouldn't switch taxi to uber
# Bus       -> Taxi
# Walk      -> Taxi               # NO, you would just taxi from the start
# Uber/Lyft -> Taxi               # NO, you wouldn't switch uber to taxi
# Taxi      -> Taxi               # NO, you're already in a taxi

# 3 LEG TRIPS
# Bus       -> Bus       -> Bus
# Walk      -> Bus       -> Bus
# Uber/Lyft -> Bus       -> Bus        # NO, you would uber to the 2nd bus stop
# Taxi      -> Bus       -> Bus        # NO, you would taxi to the 2nd bus stop
# Bus       -> Walk      -> Bus
# Uber/Lyft -> Walk      -> Bus        # NO, you would uber to the bus stop
# Taxi      -> Walk      -> Bus        # NO, you would uber to the bus stop
# Bus       -> Uber/Lyft -> Bus        # NO, you would uber to the end
# Bus       -> Taxi      -> Bus        # NO, you would taxi to the end
# Bus       -> Bus       -> Walk
# Walk      -> Bus       -> Walk
# Uber/Lyft -> Bus       -> Walk
# Taxi      -> Bus       -> Walk
# Bus       -> Walk      -> Walk       # NO, you're already walking
# Uber/Lyft -> Walk      -> Walk       # NO, you're already walking
# Taxi      -> Walk      -> Walk       # NO, you're already walking
# Bus       -> Uber/Lyft -> Walk       # NO, you would uber to the end
# Bus       -> Taxi      -> Walk       # NO, you would taxi to the end
# Bus       -> Bus       -> Uber/Lyft  # NO, you would uber from the next stop
# Walk      -> Bus       -> Uber/Lyft
# Uber/Lyft -> Bus       -> Uber/Lyft  # NO, you would uber to the end
# Taxi      -> Bus       -> Uber/Lyft  # NO, you would taxi to the end
# Bus       -> Walk      -> Uber/Lyft
# Uber/Lyft -> Walk      -> Uber/Lyft  # NO, you would uber to the end
# Taxi      -> Walk      -> Uber/Lyft  # NO, you would taxi to the end
# Bus       -> Uber/Lyft -> Uber/Lyft  # NO, you're already in an uber
# Bus       -> Taxi      -> Uber/Lyft  # NO, you're already in a taxi
# Bus       -> Bus       -> Taxi       # NO, you would taxi from the next stop
# Walk      -> Bus       -> Taxi
# Uber/Lyft -> Bus       -> Taxi       # NO, you would uber to the end
# Taxi      -> Bus       -> Taxi       # NO, you would taxi to the end
# Bus       -> Walk      -> Taxi
# Uber/Lyft -> Walk      -> Taxi       # NO, you would uber to the end
# Taxi      -> Walk      -> Taxi       # NO, you would taxi to the end
# Bus       -> Uber/Lyft -> Taxi       # NO, you're already in an uber
# Bus       -> Taxi      -> Taxi       # NO, you're already in a taxi

# Additional car cases
# Car
# Car express
# Car          -> Bus
# Car express  -> Bus
# Car          -> Walk
# Car express  -> Walk
# Car          -> Uber/Lyft     # NO, you're already driving
# Car express  -> Uber/Lyft     # NO, you're already driving
# Car          -> Taxi          # NO, you're already driving
# Car express  -> Taxi          # NO, you're already driving

# NO to all 3-leg car trips - you would just drive to the end

goodTrips <- c(
    car,
    express,
    bus,
    uber,
    taxi,
    paste(car, bus, sep = '|'),
    paste(express, bus, sep = '|'),
    paste(bus, bus, sep = '|'),
    paste(walk, bus, sep = '|'),
    paste(uber, bus, sep = '|'),
    paste(taxi, bus, sep = '|'),
    paste(car, walk, sep = '|'),
    paste(express, walk, sep = '|'),
    paste(bus, walk, sep = '|'),
    paste(uber, walk, sep = '|'),
    paste(taxi, walk, sep = '|'),
    paste(bus, uber, sep = '|'),
    paste(bus, taxi, sep = '|'),
    paste(bus, bus, bus, sep = '|'),
    paste(walk, bus, bus, sep = '|'),
    paste(bus, walk, bus, sep = '|'),
    paste(bus, bus, walk, sep = '|'),
    paste(walk, bus, walk, sep = '|'),
    paste(uber, bus, walk, sep = '|'),
    paste(taxi, bus, walk, sep = '|'),
    paste(walk, bus, uber, sep = '|'),
    paste(bus, walk, uber, sep = '|'),
    paste(walk, bus, taxi, sep = '|'),
    paste(bus, walk, taxi, sep = '|')
)

busTrips <- c(
    bus,
    paste(bus, bus, sep = '|'),
    paste(walk, bus, sep = '|'),
    paste(bus, walk, sep = '|'),
    paste(bus, bus, bus, sep = '|'),
    paste(walk, bus, bus, sep = '|'),
    paste(bus, walk, bus, sep = '|'),
    paste(bus, bus, walk, sep = '|'),
    paste(walk, bus, walk, sep = '|')
)
