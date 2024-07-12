require(dplyr)
require(ggplot2)
require(hadron)
require(scales)
require(RColorBrewer)
require(stringr)

dat <- read.table("perf.dat", header=TRUE) %>%
       dplyr::group_by(ens) %>%
       dplyr::mutate(speedup = max(trajtime)/trajtime,
                     reltime = trajtime/max(trajtime)) %>%
       dplyr::ungroup() %>%
       dplyr::mutate(volume = ifelse(ens == "B64", "$64^3 \\cdot 128$", "$112^3 \\cdot 224$"))

brewer_pal <- RColorBrewer::brewer.pal(name="Set1", n=4)

cscale <- brewer_pal[c(4,3,2,1)]
clabels <- unique(dat$machine)[c(3,4,1,2)]

tikzfiles <- hadron::tikz.init(basename = "quda_speedup",
                               width = 2.5, height = 3.5)
#p <- ggplot2::ggplot(dat, aes(x = reorder(volume, ifelse(ens == "B64", 1, 2)), 
#                              y = reltime, 
#                              colour = reorder(machine, speedup), fill = reorder(machine, speedup))) +
#     ggplot2::geom_bar(stat = 'identity',
#                       position = "dodge2") +
#     ggplot2::theme_bw() +
#     ggplot2::labs(x = "4D volume",
#                   y = "time per trajectory (relative)",
#                   colour = "",
#                   fill = "") + 
#     ggplot2::scale_y_continuous(labels = c(1.0, 0.75, 0.5, 0.25, 0.0),
#                                 breaks = c(1.0, 0.75, 0.5, 0.25, 0.0)) +
#     ggplot2::scale_fill_manual(values = cscale,
#                                breaks = clabels,
#                                labels = stringr::str_wrap(clabels, 24)) +
#     ggplot2::scale_color_manual(values = cscale, 
#                                 breaks = clabels,
#                                 labels = stringr::str_wrap(clabels, 24)) +
#     ggplot2::theme(legend.position = 'bottom',
#                    legend.position.inside = c(-1,1)) + 
#     ggplot2::guides(fill = guide_legend(nrow = 2, byrow = FALSE))
#
#plot(p)
for( vol in unique(dat$volume) ){
  vdat <- dplyr::filter(dat, volume == vol)
  p <- ggplot2::ggplot(vdat, aes(x = volume, 
                                y = reltime, 
                                colour = reorder(machine, speedup), fill = reorder(machine, speedup))) +
       ggplot2::geom_bar(stat = 'identity',
                         position = "dodge2") +
       ggplot2::theme_bw() +
       ggplot2::labs(x = "4D volume",
                     y = "time per trajectory (relative)",
                     colour = "",
                     fill = "") + 
       ggplot2::scale_y_continuous(labels = c(1.0, 0.75, 0.5, 0.25, 0.0),
                                   breaks = c(1.0, 0.75, 0.5, 0.25, 0.0)) +
       ggplot2::scale_fill_manual(values = cscale,
                                  breaks = clabels,
                                  labels = clabels) + #stringr::str_wrap(clabels, 24)) +
       ggplot2::scale_color_manual(values = cscale, 
                                   breaks = clabels,
                                   labels = clabels) + #stringr::str_wrap(clabels, 24)) +
       ggplot2::theme(legend.position = 'bottom',
                      legend.position.inside = c(-1,1)) + 
       ggplot2::guides(fill = guide_legend(nrow = 2, byrow = FALSE))
  plot(p)
}
hadron::tikz.finalize(tikzfiles, crop = FALSE)
