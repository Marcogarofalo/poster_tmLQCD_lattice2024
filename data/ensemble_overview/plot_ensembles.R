require("hadron")
require("ggplot2")
require("ggforce")
require("knitr")
require("kableExtra")
require("stringr")
require("tidyverse")
require("RColorBrewer")
require("ggstance")
ensembles <- read.table("ensembles.dat", header=TRUE)
ensembles <- cbind(ensembles, 
                   Lmpi = ensembles$mpi/197.3*ensembles$a*ensembles$L,
                   Lfm = ensembles$L*ensembles$a)

ensembles <- ensembles[,c("ensemble", "beta", "mpi", "L", "T", "a", "Lmpi", "Lfm", "state","this_proposal")]

# add newlines to long state descriptions
ensembles$state <- factor(str_wrap(ensembles$state,10))

## do not overwrite hand-adjusted table
##kable(ensembles, "latex", 
##      booktabs = TRUE, 
##      escape = FALSE, 
##      digits = 3,
##      linesep = "",
##      col.names = c("Ensemble", "$\\beta$", "$M_\\pi$", "L/a", "T/a", "$a$ [fm]", "$M_\\pi \\cdot L$", "$L$ [fm]", "status")) %>%
##  cat(., file="ensembles_table.tex")

## let's build a custom colour scheme for this
require("RColorBrewer")
colScale <- scale_colour_manual(name = "a",
                                values = RColorBrewer::brewer.pal(n = 6, name = "Set1"),
                                breaks = sort(unique(ensembles$a), decreasing = TRUE))

tikzfiles <- tikz.init(basename = "ensembles_asquared_mpi",
                       standAlone = TRUE,
                       width = 3.2, height = 2.625)

p <- ggplot(ensembles, aes(x=mpi, y=a^2)) +
     geom_vline(xintercept = 135, linetype='dashed', alpha = 0.5) +
     geom_point(aes(shape=factor(state), colour=factor(a)), 
                stroke=1.5,
                position = ggstance::position_dodgev(height = min(ensembles$a^2)/6,
                                                     preserve = 'total')) +
     scale_shape(solid=FALSE) +
     scale_y_continuous(breaks = unique(ensembles$a^2),
                        labels = sprintf("$(%.3f)^2$",unique(ensembles$a))) +
     scale_x_continuous(breaks = unique(ensembles$mpi),
                        labels = unique(ensembles$mpi)) +
     colScale +
	# padding title with spaces allows to influence ordering of guides...
	# BIZARRE! https://stackoverflow.com/questions/11393123/controlling-ggplot2-legend-display-order
     guides(colour = guide_legend(title = "      $a$ [fm]"),
            shape = guide_legend(title = "sim. status")) +
     labs(x = "$M_\\pi$ [MeV]",
	        y = "$a^2$ [fm$^2$]") +
        #lims( x = c(50, 400), y = c(0.068, 0.1) ) +
     theme_bw() +
	#geom_ellipse(aes(x0 = 135, y0 = 0.00824464, a = 7, b = 0.0004, angle = 0), colour = "deepskyblue") +
     theme(legend.text = element_text(size=8),
           legend.key.size = unit(9.0, 'points'),
           legend.spacing.y = unit(0.0, 'points'),
           legend.box.spacing = unit(4.0, 'points'),
           legend.box.margin = margin(25,0,0,0,"pt"),
	         #legend.title.align = 0.0,
           legend.title = element_text("hjust"),
	         legend.box.just = "left",
	         legend.margin = margin(2,2,2,2,"pt"),
           axis.text.x = element_text(angle = 90, vjust = 0.5))
plot(p)

tikz.finalize(tikzfiles, crop=FALSE)

tikzfiles <- tikz.init(basename = "ensembles_L_vs_mpi",
                       standAlone = TRUE,
                       width = 2.5, height = 2.625)

p <- ggplot(ensembles, aes(x=mpi, y=Lmpi)) +
      geom_vline(xintercept = 135, linetype='dashed') +
      geom_point(aes(shape=factor(state), colour=factor(a)), 
                 stroke=1.5,
                 position = ggplot2::position_dodge(width = min(ensembles$mpi)/20)
                ) +
      scale_y_continuous(limits = c(2.3, 5.7),
                         breaks = 0.5*seq(0,20)) +
      scale_x_continuous(breaks = unique(ensembles$mpi),
                         labels = unique(ensembles$mpi)) +
      scale_shape(solid=FALSE) +
      colScale +
      labs( y = "$M_\\pi \\cdot L$", x = "$M_\\pi$ [MeV]" ) +
 #geom_ellipse(aes(x0 = 135, y0 = 5.8112, a = 4, b = 0.15, angle = 0), colour = "deepskyblue") +
 #geom_ellipse(aes(x0 = 135, y0 = 5.5, a = 6, b = 0.7, angle = 0), colour = "blue") +
      #lims( y = c(350, 120) ) +
      theme_bw() + 
      theme(legend.position="none",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
            #legend.box.background = element_rect(colour = "black"),
            #legend.background = element_blank(),
            #legend.text = element_text(size=8),
            #legend.key.size = unit(10.0, 'points'),
            #legend.spacing.y = unit(0.0, 'points'),
            #legend.box.spacing = unit(0.0, 'points'))
plot(p)
tikz.finalize(tikzfiles, crop = FALSE)

tikzfiles <- tikz.init(basename = "ensembles_phys_point",
                       standAlone = TRUE,
                       width = 2.5, height = 2.625)

Lmpi_max <- 6

rects <- data.frame(xstart = rep(0.02^2,Lmpi_max), xend = rep(0.12^2, Lmpi_max),
                    ystart = seq(1,Lmpi_max)*197.3/135, yend = seq(2,Lmpi_max+1)*197.3/135,
                    labels = sprintf("$M_\\pi\\cdot L \\geq %.1f$", seq(1,Lmpi_max)))

p <- ggplot2::ggplot(data = dplyr::filter(ensembles, mpi < 150), 
                     aes(x = a^2, y = Lfm, 
                         #size = as.factor(L), 
                         shape = as.factor(state),
                         #fill = as.factor(this_proposal),
                         colour = as.factor(a))) +
     colScale +
     scale_x_continuous(breaks = unique(ensembles$a^2),
                        labels = sprintf("$(%.3f)^2$",unique(ensembles$a)),
                        ) +
     scale_y_continuous(breaks = 0.5*seq(0,20)) +
     coord_cartesian(ylim = c(3.0, 9.0), xlim = c(0.045^2, 0.088^2)) +
     scale_shape_manual(values = c(1,2,0),
                        breaks = c("done","ongoing","planned")) +
     ggplot2::geom_rect(data = rects,
                        inherit.aes = FALSE,
                        aes(xmin = xstart, xmax = xend, ymin = ystart, ymax = yend, 
                            fill = 'blue', alpha = 1-yend/max(yend))) +
     ggplot2::geom_point(stroke = 1.5) +
     ggplot2::geom_point(size = 0.5, shape = 16) +
     ggplot2::geom_text(data = rects,
                        inherit.aes = FALSE,
                        size = 1.8,
                        hjust = 'left',
                        #vjust = 'top',
                        aes(x = 0.045^2, y = ystart+0.2, label = labels)) +
     ggplot2::theme_bw() +
     ggplot2::labs(x = "$a^2$ [fm$^2$]",
                   size = "$L/a$",
                   shape = "simulation state:",
                   fill = "this proposal",
                   y = "$L$ [fm]") +
                   # title = "$M_\\pi \\approx 135$ MeV") +
     ggplot2::theme(legend.position = "top",
                    legend.direction = 'horizontal',
                    legend.justification = "right",
                    legend.text = element_text(size = 5),
                    legend.title = element_text(size = 7),
                    legend.key.size = unit(2, 'pt'),
                    legend.margin = margin(0,0,0,0),
                    legend.box.margin = margin(-3,-3,-3,-3),
                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
     ggplot2::guides(colour = "none",
                     #shape = "none",
                     fill = "none",
                     alpha = "none",
                     colour = "none",
                     size = "none",
                     title = "none")
plot(p)

tikz.finalize(tikzfiles, crop = FALSE)

