require(dplyr)
require(ggplot2)
require(hadron)
require(scales)
require(RColorBrewer)
require(stringr)
require(patchwork)
require(ggrepel)

dat <- read.table("perf.dat", header=TRUE) %>%
       dplyr::group_by(ens) %>%
       dplyr::mutate(speedup = max(trajtime)/trajtime,
                     trajhour = trajtime/3600,
                     reltime = trajtime/max(trajtime)) %>%
       dplyr::ungroup() %>%
       dplyr::mutate(volume = ifelse(ens == "B64", "$64^3 \\cdot 128$", "$112^3 \\cdot 224$"))

brewer_pal <- RColorBrewer::brewer.pal(name="Set1", n=4)



cscale <- brewer_pal[c(4,3,2,1)]
clabels <- unique(dat$machine)[c(3,4,1,2)]

tikzfiles <- hadron::tikz.init(basename = "quda_speedup",
                               width = 3.6, height = 3)
# create N plots, one for each volume
# the plots consist of two panels
#  - the left-hand panel shows the time per trajectory in hours as a bar plot
#    and is ordered in decreasing order
#  - the machine type is indicated by the fill & colour of the bar
#  - the second panel shows the speedup relative to the longest trajectory time
#    as a point and line plot
#  - the two panels are arranged side-by-side with the speedup axis on the right
#  - a horizontal common legend is placed at the bottom of the plot grid
#    which gives the machine name
for( vol in unique(dat$volume) ){
  vdat <- dplyr::filter(dat, volume == vol)
  p1 <- ggplot2::ggplot(vdat, aes(x = seq(1,nrow(vdat)), 
                                 y = trajhour, 
                                 colour = reorder(machine, speedup), fill = reorder(machine, speedup))) +
       ggplot2::geom_bar(stat = 'identity', width = 0.6) +
       ggplot2::theme_bw() +
       ggplot2::labs(y = "time per trajectory [h]",
                     colour = "",
                     fill = "") + 
       ggplot2::coord_cartesian(ylim = c(0.7*min(vdat$trajhour), max(vdat$trajhour))) +
       ggplot2::scale_y_continuous(breaks = seq(0,6,0.5),
                                   ) +
       ggplot2::scale_fill_manual(values = cscale,
                                  breaks = clabels,
                                  labels = clabels) + 
       ggplot2::scale_color_manual(values = cscale, 
                                   breaks = clabels,
                                   labels = clabels) + 
       ggplot2::theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank())

  p2 <- ggplot2::ggplot(vdat, aes(x = seq(1,nrow(vdat)), y = speedup)) + 
        ggplot2::geom_line(aes(y = speedup),
                           colour = 'black',
                           lty = 'dashed') +
        ggplot2::geom_label(label = c("CPU", "GPU solve", "GPU light force", "GPU force"),
                            size = 2.5) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "",
                      y = "speedup") +
        # remove the first y axis and replace it with one on the right
        ggplot2::scale_y_continuous(name = NULL,
                                    sec.axis = sec_axis(~., name = "speedup", breaks = seq(0,3,0.5)),
                                    ) +
        ggplot2::coord_cartesian(ylim = c(0.8,3), xlim = c(0.4, 5)) +
        # we do not want to have any X axis
        ggplot2::theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank()) +
        ggplot2::guides(y = "none")
  
  # combine the two plots and build the layout and legend using 'patchwork'
  # the guide position and the layout for 'fill' scale must be applied
  # at the very end to the completed layout
  combined <- (p1 + p2)
  combined <- combined + 
              patchwork::plot_layout(guides = "collect") &
              ggplot2::theme(legend.position = "bottom") &
              ggplot2::guides(fill = guide_legend(nrow = 2, byrow = FALSE))
  plot(combined)

}
hadron::tikz.finalize(tikzfiles, crop = FALSE)
