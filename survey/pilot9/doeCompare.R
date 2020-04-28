does <- readRDS(here::here(
    'survey', 'pilot9', 'survey', 'doe', 'doe_6000.Rds'))

doe_no  <- does$no %>% mutate(hasCar = 'no')
doe_yes <- does$yes %>% mutate(hasCar = 'yes')
doe     <- bind_rows(doe_no, doe_yes)

# Compare balance of modes:
doe %>% 
    group_by(hasCar) %>% 
    mutate(
        car  = ifelse(carInTrip | expressInTrip, T, F),
        taxi = ifelse(taxiInTrip | uberInTrip, T, F),
        bus  = busInTrip,
        walk = walkInTrip
    ) %>%
    gather(mode, count, car:walk) %>%
    count(hasCar, mode, count) %>%
    mutate(percent = n / nrow(doe)) %>%
    filter(count == TRUE) %>%
    ggplot() +
    geom_bar(aes(x = mode, y = percent), stat='identity') +
    facet_wrap(~hasCar)

# Compare balance of trip legs:
doe %>% 
    group_by(hasCar) %>% 
    count(numLegs) %>%
    mutate(p = 100*(n / sum(n))) %>%
    ggplot() +
    geom_col(aes(x = numLegs, y = p)) + 
    facet_wrap(~hasCar)    
