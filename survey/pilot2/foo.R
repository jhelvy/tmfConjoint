library(data.table)
library(ggplot2)
library(here)

# -----------------------------------------------------------------------------
# Functions for making trip images

theme_trip <- function(){
    theme(text = element_text("sans-serif"),
          
          panel.grid.minor = element_blank(),
          panel.background =  element_blank(),
          panel.border = element_blank(), 
          
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          
          legend.position = "none"
    )
}

formatPlotDf <- function(trip) {
    node  <- trip[type == 'Node']
    label <- trip[type != 'Node']
    return(list(node=node, label=label))
}

makePlot <- function(trip) {
    p <-
    ggplot(data = trip, aes(x = x, y = y)) +
        geom_line() +
        geom_label(aes(x = x, y = y, label = label, fill = type)) +
        scale_fill_manual(values=c("#FFFFFF", "#E0E0E0")) +
        scale_x_continuous(limits = c(-1, 1)) +
        theme_void() +
        theme(
            legend.position = "none",
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    return(p)
}

# -----------------------------------------------------------------------------
# Load the full design of experiment

respondentID <- 1

# Load respondent doe
# path <- paste('https://raw.githubusercontent.com/jhelvy/tmfConjoint/master/survey/pilot2/trips/', respondentID, '.csv', sep='')
path<- paste(here::here('survey', 'pilot2', 'trips'), '/', respondentID, '.csv', sep='')
doe <- fread(path)

# Filter out the trips
trip1 <- doe[(altID == 1) & (qID == 1)]
trip2 <- doe[(altID == 2) & (qID == 1)]
trip3 <- doe[(altID == 3) & (qID == 1)]

trip <- formatPlotDf(trip1)

ggplot(data = trip$node, aes(x = x, y = y)) +
    ggplot(data = schedule, aes(x=Date, y=Sport, color=Sport)) +
    geom_vline(xintercept = today(), linetype=4, colour="dark grey") +
    geom_vline(xintercept = as.Date("2019-07-01"), colour="black") +
    geom_point(size=2) +
    geom_point(size=4, alpha=.5) +
    geom_line(size=1) +
    theme_trip()