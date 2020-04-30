doe_all <- readRDS(here::here(
    'survey', 'pilot9', 'survey', 'doe', 'doe.Rds'))

doe     <- doe_all$`6000`
doe_no  <- doe$no %>% mutate(hasCar = 'no')
doe_yes <- doe$yes %>% mutate(hasCar = 'yes')
doe     <- bind_rows(doe_no, doe_yes)

# Compare balance of modes:
doe %>%
    group_by(hasCar) %>%
    count(hasCar, tripType) %>%
    mutate(percent = n / nrow(doe)) %>%
    ggplot() +
    geom_bar(aes(x = tripType, y = percent), stat='identity') +
    facet_wrap(~hasCar)

# Compare balance of trip legs:
doe %>%
    group_by(hasCar) %>%
    count(numLegs) %>%
    mutate(p = 100*(n / sum(n))) %>%
    ggplot() +
    geom_col(aes(x = numLegs, y = p)) +
    facet_wrap(~hasCar)
