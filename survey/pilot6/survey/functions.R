library(data.table)
library(ggplot2)
library(ggrepel)

makePlot <- function(trip) {
    p <-
        ggplot(data = trip, aes(x = x, y = y)) +
        # Draw lines
        geom_line(data = trip, size = 1, linetype = 'dotted') +
        geom_line(data = trip[lineNodes == 1], size = 1) +
        geom_line(data = trip[lineNodes == 2], size = 1) +
        geom_line(data = trip[lineNodes == 3], size = 1) +
        # Draw nodes
        geom_point(data = trip[node == 1], size = 4, pch = 21,
                   fill = 'white', colour = 'black') +
        theme_void() +
        geom_text_repel(data = trip[labelType == 'Transit'], aes(label=label),
                         size           = 4,
                         force          = 3,
                         nudge_x        = 1,
                         fontface       = "bold",
                         box.padding    = unit(0.35, "lines"),
                         point.padding  = unit(0.75, "lines"),
                         color          = "black",
                         # fill           = "white",
                         segment.colour = "black") +
        # geom_label_repel(data = trip[labelType == 'Node'], aes(label=label),
        #                  size           = 4,
        #                  force          = 3,
        #                  nudge_x        = -1,
        #                  fontface       = "bold",
        #                  box.padding    = unit(0.35, "lines"),
        #                  point.padding  = unit(0.75, "lines"),
        #                  color          = "black",
        #                  fill           = "white",
        #                  segment.colour = "black") +
        geom_text_repel(data = trip, aes(label=times),
                        size           = 4,
                        force          = 3,
                        nudge_x        = -1,
                        fontface       = "bold",
                        box.padding    = unit(0.35, "lines"),
                        point.padding  = unit(0.75, "lines"),
                        color          = "black",
                        # fill           = "white",
                        segment.colour = "black") +
        geom_label(data = trip[labelType == 'Terminal'], aes(label=label),
                   label.size = 1,
                   fontface   = "bold",
                   fill       = "white",
                   color      = "black") +
        scale_x_continuous(limits=c(-1, 0.6))
    return(p)
}
