library(here)
source(here::here('survey', 'pilot7', 'functions.R'))

# Load DOE from github (same process as formr will do)
rootPath <- "https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot7/survey/"
doeNoCarPath <- paste(rootPath, 'doeNoCar.csv', sep = '')
doeAllPath <- paste(rootPath, 'doeAll.csv', sep = '')
doeAll <- fread(doeAllPath)
doeNoCar <- fread(doeNoCarPath)

# Set the sample sizes
numResp <- seq(100, 3000, 300)
for (size in numResp) {
    respIDs <- sample(seq(max(doeAll$respID)), size)
    tempDoeAll <- doeAll[respID %in% respIDs]
    tempDoeNoCar <- doeNoCar[respID %in% respIDs]
    

    tempDoe <- tempDoeAll %>% 
        bind_rows(tempDoeNoCar) %>% 
        arrange(respID, qID, altID)
    
}


source(plotFuncsPath)
doe <- fread(tripDfPath)[respID == respondentID]
# Filter out the alternatives
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

makePlot(trip2)

