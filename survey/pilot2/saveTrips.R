library(tidyverse)
library(here)
options(dplyr.width = Inf) # Option to preview all columns in a data frame

tripDfList <- readRDS(here::here('survey', 'pilot2', 'tripDfList.Rds'))

respID <- 1
index <- 2
temp <- list(tripDfList[[1]])
for (i in 2:length(tripDfList)) {
    trip <- tripDfList[[i]]
    if (trip$respID[1] == respID) {
        temp[[index]] <- trip
        index <- index + 1
    } else {
        # Save the tripDf
        tripDf <- do.call(rbind, temp)
        write_csv(tripDf, here::here('survey', 'pilot2', 'trips', 
                                     paste(respID, '.csv', sep='')))
        # Start a new temp list
        respID <- trip$respID[1]
        index <- 2
        temp <- list(trip)
    }
}