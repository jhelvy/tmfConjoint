library(tidyverse)

# -----------------------------------------------------------------------------
# Make trip maps

getPlotDf <- function(x) {return(data.frame(x = x, y = rep(1, length(x))))}

getPlotX <- function(row) {
    carDf = getPlotDf(x=c(0, 1))
    uberDf = getPlotDf(x=c(0, 1))
    busDf = getPlotDf(x=seq(0, 1, length.out=(unique(row$numTransfers) + 2)))
    plotDf = getPlotDf0()
    plotDf = data.frame(
        type = c('car', 'car', 'uber', 'uber'),
        x    = c(0, 1, 0, 1),
        y    = c(1, 1, 1, 1)
    )
    if (row$bus_numTransfers == 1) {
        x = c(0, 0.5, 1)
    } else if (row$bus_numTransfers == 2) {
        x = c(0, 0.33, 0.66, 1)
    }
    plotDf = data.frame(x = x, y = rep(1, length(x)))
    return(plotDf)
}



i <- 13123
row  <- filter(design, id == i)
plotDf <- getPlotDf(row)

carPlot <- ggplot(plotDf,
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void()
uberPlot <- ggplot(plotDf,
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void()
busPlot <- ggplot(plotDf,
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void()



ggplot(getPlotDf(x=c(0, 1)),
    aes(x = x, y = y)) +
    geom_point(size=2) +
    geom_line() +
    theme_void() +
    annotate("text", x = 0.5, y = 0.8, label = row[which(row$type == 'car'),]$time) +
    annotate("text", x = 0.5, y = 0.6, label = paste('$', row[which(row$type == 'car'),]$price, ' parking + gas', sep='')) +
    annotate("text", x = 0.5, y = 10, label = '')










