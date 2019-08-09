library(data.table)
library(ggplot2)
library(ggrepel)

makePlot <- function(trip) {
    p <-
        ggplot(data = trip[node == 1], aes(x = x, y = y)) +
        geom_point(size = 2) +
        geom_point(size = 4, alpha = 0.5) +
        geom_point(size = 6, alpha = 0.25) +
        geom_line(data = trip, size = 1, linetype = 'dotted') +
        geom_line(data = trip[line == 1], size = 1) +
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
        geom_label_repel(data = trip[labelType == 'Node'], aes(label=label),
                         size           = 4,
                         force          = 3,
                         nudge_x        = -1,
                         fontface       = "bold",
                         box.padding    = unit(0.35, "lines"),
                         point.padding  = unit(0.75, "lines"),
                         color          = "black",
                         fill           = "white",
                         segment.colour = "black") +
        geom_label(data = trip[labelType == 'Terminal'], aes(label=label),
                   label.size = 1,
                   fontface   = "bold",
                   fill       = "white",
                   color      = "black") +
        scale_x_continuous(limits=c(-1, 0.6))
    return(p)
}
