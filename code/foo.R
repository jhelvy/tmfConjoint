# -----------------------------------------------------------------------------
# Setup

library(tidyverse)
library(here)
library(rvest)
library(mapsapi)
options(dplyr.width = Inf) # Option to preview all columns in a data frame
source(here('code', 'functions.R'))

# -----------------------------------------------------------------------------
# mapsapi:
# https://cran.rstudio.com/web/packages/mapsapi/vignettes/intro.html

# Animation of commuters:
# https://la.curbed.com/2016/5/25/11775412/map-los-angeles-commute-animation

# Metro map:
# https://upload.wikimedia.org/wikipedia/commons/f/fd/Los_Angeles_County_Metro_Rail_and_Metro_Liner_map.svg

# Regions defined from economic development zones:
# https://laedc.org/wtc/chooselacounty/regions-of-la-county/
locations <- data.frame(
    origin = c('Lancaster, CA',                   # Antelope Valley
               'Central LA, Los Angeles, CA',     # Central Los Angeles
               'Paramount, California',           # Gateway Cities
               'San Fernando Valley, California', # San Fernando Valley
               'El Monte, California',            # San Gabriel Valley
               'Santa Clarita, CA',               # Santa Clarita Valley
               'South Bay, California',           # South Bay
               'Culver City, California'),        # Westside
    destination = c('Los Angeles, CA')
)
modes <- c('driving', 'transit', 'walking', 'bicycling')

directions = mp_directions(
    origin       = as.character(locations$origin[1]),
    destination  = as.character(locations$destination[1]),
    alternatives = TRUE,
    mode         = as.character(modes[2]),
    key          = api_key
)

routes = mp_get_routes(directions)
routes

segments <- mp_get_segments(directions)
segments