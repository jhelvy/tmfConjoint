# List of possible trips - those marked "NO" will be filtered out:

# Bus
# Uber/Lyft
# Taxi
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
# Bus       -> Bus       -> Bus
# Walk      -> Bus       -> Bus
# Uber/Lyft -> Bus       -> Bus        # NO, you would uber to the 2nd bus
# Taxi      -> Bus       -> Bus        # NO, you would taxi to the 2nd bus
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

# NO 3-leg car trips - you would just drive to the end

goodTrips <- c(
    'Bus',
    'Uber/Lyft',
    'Taxi',
    'Bus|Bus',
    'Walk|Bus',
    'Uber/Lyft|Bus',
    'Taxi|Bus',
    'Bus|Walk',
    'Uber/Lyft|Walk',
    'Taxi|Walk',
    'Bus|Uber/Lyft',
    'Bus|Taxi',
    'Bus|Bus|Bus',
    'Walk|Bus|Bus',
    'Bus|Walk|Bus',
    'Bus|Bus|Walk',
    'Walk|Bus|Walk',
    'Uber/Lyft|Bus|Walk',
    'Taxi|Bus|Walk',
    'Walk|Bus|Uber/Lyft',
    'Bus|Walk|Uber/Lyft',
    'Walk|Bus|Taxi',
    'Bus|Walk|Taxi')

busTrips <- c(
    'Bus',
    'Bus|Bus',
    'Walk|Bus',
    'Bus|Walk',
    'Bus|Bus|Bus',
    'Walk|Bus|Bus',
    'Bus|Walk|Bus',
    'Bus|Bus|Walk',
    'Walk|Bus|Walk')
