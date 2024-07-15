require(hadron)
require(dplyr)
require(ggplot2)
require(ggh4x)

dat <- read.table("scaling.dat", header=TRUE)

dat <- dplyr::mutate(dat, gpus = nodes * 4,
                          traj_nodeh = nodes * traj_tts / 3600,
                          type = "")
tikzfiles <- tikz.init("HMC_Scaling", width = 3, height = 2)

for (l in unique(dat$volume)){
  ldat <- dplyr::filter(dat, volume == l) %>%
          dplyr::mutate(speedup = max(traj_tts)/traj_tts,
                        peff    = min(nodes) * speedup / nodes)

  print(ldat)
  clr <- ifelse(unique(ldat$machine) == "Juwels Booster", "darkgreen", "red")
  lxpos <- ifelse(unique(ldat$machine) == "Juwels Booster", 0.2, 0.14)

  p <- ggplot2::ggplot(ldat, aes(x = nodes, y = speedup, 
				 colour = interaction(machine, type, sep = " "),
				 linetype = interaction(machine, type, sep = " "),
				 shape = interaction(machine, type, sep = " "))) +
       ggplot2::geom_abline(data = mutate(subset(ldat, nodes == min(nodes)),
					  machine = "ideal",
					  type = ""),
			    aes(intercept = 0, 
				slope = 1.0/nodes,
				colour = interaction(machine, type, sep = " "),
				linetype = interaction(machine, type, sep = " "),
				shape = interaction(machine, type, sep = " ")
				),
			    lwd = 1 
			    ) +
       ggplot2::scale_linetype_manual(values = c("dashed", NA), name = "") +
       ggplot2::scale_colour_manual(values = c("black", clr), name = "") +
       ggplot2::scale_shape_manual(values = c(16, 16), name = "") +
       ggplot2::geom_point() +
       ggplot2::theme_bw() +
       ggplot2::labs(x = "Nodes",
                     y = "Speed-up",
		     colour = "",
		     linetype = "",
		     shape = ""
                     #title = sprintf("$%d^3 \\cdot %d$ HMC @ phys.point", l, 2*l)
		     ) +
       ggplot2::guides(x = ggh4x::guide_axis_manual(breaks = ldat$nodes, labels = ldat$nodes)) +
       ggplot2::lims(y = c(0.9, max(ldat$nodes)/min(ldat$nodes))) +
       ggplot2::theme(legend.position = "inside",
                      legend.position.inside = c(lxpos, 0.86),
		      legend.box.background = element_rect(colour = "black",
							   fill = "white"),
		      legend.box.margin = margin(.2,.2,.2,.2,"pt"),
		      legend.margin = margin(-1,1,1,1,"pt"),
		      legend.key.size = unit(10, "pt"),
		      legend.text = element_text(size = 6)) +
       ggplot2::guides(colour = guide_legend(title = NULL),
		       linetype = guide_legend(title = NULL),
		       shape = guide_legend(title = NULL))

  plot(p)
}

tikz.finalize(tikzfiles, crop = FALSE)

