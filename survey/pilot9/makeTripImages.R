source(here::here('survey', 'pilot9', 'functions.R'))

# Read in DOEs
doe_all <- readRDS(here::here(
    'survey', 'pilot9', 'survey', 'doe', 'doe.Rds'))
doe     <- doe_all$`6000`
doeNo  <- doe$no %>% mutate(hasCar = 'no')
doeYes <- doe$yes %>% mutate(hasCar = 'yes')

# Create all trip images
saveRoot <- '/Users/jhelvy/sync/00_projects/TMF/survey'
s <- data.frame(w = 2.5, h = 5.5, d = 100) # image save settings
for (i in seq(max(doeYes$respID))) {
    respDoeYes <- filter(doeYes, respID == i)
    respDoeNo <- filter(doeNo, respID == i)
    for (j in seq(max(respDoeYes$qID))) {
        plotNames <- str_c(j, '-', c(1,2,3), '.png')
        respRootYes <- paste(saveRoot, 'imagesCarYes', i, sep = '/')
        respRootNo <- paste(saveRoot, 'imagesCarNo', i, sep = '/')
        dir.create(respRootYes)
        dir.create(respRootNo)
        pathsYes <- paste(respRootYes, plotNames, sep='/')
        pathsNo <- paste(respRootNo, plotNames, sep='/')
        tripDf1Yes <- getTripDf(filter(respDoeYes, altID == 1, qID == j))
        tripDf2Yes <- getTripDf(filter(respDoeYes, altID == 2, qID == j))
        tripDf3Yes <- getTripDf(filter(respDoeYes, altID == 3, qID == j))
        tripDf1No <- getTripDf(filter(respDoeNo, altID == 1, qID == j))
        tripDf2No <- getTripDf(filter(respDoeNo, altID == 2, qID == j))
        tripDf3No <- getTripDf(filter(respDoeNo, altID == 3, qID == j))
        trip1Yes <- makePlot(tripDf1Yes)
        trip2Yes <- makePlot(tripDf2Yes)
        trip3Yes <- makePlot(tripDf3Yes)
        trip1No <- makePlot(tripDf1No)
        trip2No <- makePlot(tripDf2No)
        trip3No <- makePlot(tripDf3No)
        ggsave(pathsYes[1], trip1Yes, width=s$w, height=s$h, dpi = s$d)
        ggsave(pathsYes[2], trip2Yes, width=s$w, height=s$h, dpi = s$d)
        ggsave(pathsYes[3], trip3Yes, width=s$w, height=s$h, dpi = s$d)
        ggsave(pathsNo[1], trip1No, width=s$w, height=s$h, dpi = s$d)
        ggsave(pathsNo[2], trip2No, width=s$w, height=s$h, dpi = s$d)
        ggsave(pathsNo[3], trip3No, width=s$w, height=s$h, dpi = s$d)
    }
}
