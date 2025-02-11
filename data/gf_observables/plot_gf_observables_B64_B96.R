require(ggplot2)
require(hadron)
require(dplyr)
require(patchwork)

ensembles <- c("cB211.072.64", 
               "cD211.054.96_BC", "cD211.054.96_DE")
as <- c(0.080, 0.080, 0.080)
Ls <- c(64, 96, 96)

dat  <- NULL
gf_summary_dat <- NULL

for( i in 1:length(ensembles) ){

  a <- as[i]
  ens <- ensembles[i]
  L <- Ls[i]
  
  fsuffix <- "combined_gf_analysis.Rdata"
  if( grepl("cD211.054.96", ens) ){
    fsuffix <- "gf_analysis.Rdata"
  }
  
  load(sprintf("%s_%s", ens, fsuffix))

  dens <- ens
  if( grepl("cD211.054.96", ens) ){
    dens <- "cD211.054.96"
  } else if ( grepl("cE211", ens) ){
    dens <- "cE211.044.112"
  }

  md_offset <- 0
  if( ens == "cD211.054.96_DE" ){
    md_offset <- max(filter(dat, ens == "cD211.054.96")$md_idx)
  } else if ( ens == "cE211cd.044.112" ){
    md_offset <- max(filter(dat, ens == "cE211.044.112")$md_idx)
  }

  stream_id <- ""
  if( ens == "cD211.054.96_DE" || ens == "cE211cd.044.112" ){
    stream_id <- ""
  }

  dat <- rbind(dat,
               data.frame(ens = dens,
                          stream = stream_id,
                          md_idx = seq(0,length(gf_analysis$interpolations$w0$Wsym_w0)-1) * gf_analysis$md_scalefac +
                                   md_offset,
                          Wsym_w0 = gf_analysis$interpolations$w0$Wsym_w0,
                          Qsym_w0 = gf_analysis$interpolations$w0$Qsym_w0
                          )
               )

  Qsym_w0_uw <- hadron::uwerrprimary(data = gf_analysis$interpolations$w0$Qsym_w0, pl = FALSE)
  Wsym_w0_uw <- hadron::uwerrprimary(data = gf_analysis$interpolations$w0$Wsym_w0, pl = FALSE) 

  gf_summary_dat <- rbind(gf_summary_dat,
                          data.frame(ens = dens,
                                     a = a,
                                     L = L,
                                     stream = stream_id,
                                     Qsym_w0_tauint = Qsym_w0_uw$tauint * gf_analysis$md_scalefac ,
                                     Qsym_w0_dtauint = Qsym_w0_uw$dtauint * gf_analysis$md_scalefac ,
                                     Wsym_w0_tauint = Wsym_w0_uw$tauint * gf_analysis$md_scalefac ,
                                     Wsym_w0_dtauint = Wsym_w0_uw$dtauint * gf_analysis$md_scalefac  
                                     )
                          )
                          
}

dat_stat <- dplyr::group_by(dat, ens) %>%
            dplyr::summarise(Wsym_w0_max = max(Wsym_w0),
                             Wsym_w0_min = min(Wsym_w0),
                             Qsym_w0_max = max(Qsym_w0),
                             Qsym_w0_min = min(Qsym_w0)) %>%
            dplyr::ungroup()

lim_dat <- data.frame(ens = rep(dat_stat$ens, each=2), 
                      y_Wsym = as.vector(rbind(
                                         dat_stat$Wsym_w0_min-0.25*abs(dat_stat$Wsym_w0_max-dat_stat$Wsym_w0_min), 
                                         dat_stat$Wsym_w0_max+0.25*abs(dat_stat$Wsym_w0_max-dat_stat$Wsym_w0_min))),
                      y_Qsym = as.vector(rbind(
                                         dat_stat$Qsym_w0_min-0.25*abs(dat_stat$Qsym_w0_max-dat_stat$Qsym_w0_min), 
                                         dat_stat$Qsym_w0_max+0.25*abs(dat_stat$Qsym_w0_max-dat_stat$Qsym_w0_min))))

label_dat <- data.frame(ens = c(ensembles[1], "cD211.054.96"),
                        label = sprintf("$%s, a \\sim %.3f$ fm, $L/a = %d$",
                                        c(ensembles[1], "cD211.054.96"),
                                        c(0.080, 0.080),
                                        c(64, 96)),
                        x = rep(250, times=2),
                        y_Wsym = dat_stat$Wsym_w0_max - 0.10*abs(dat_stat$Wsym_w0_max-dat_stat$Wsym_w0_min),
                        y_Qsym = dat_stat$Qsym_w0_max - 0.10*abs(dat_stat$Qsym_w0_max-dat_stat$Qsym_w0_min))

tikzfiles <- hadron::tikz.init(basename = "gf_observables_md_histories_B64_B96", width = 6, height = 1.8)

p1 <- ggplot2::ggplot(data = dat, 
                     aes(x = md_idx, y = Wsym_w0, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_line() +
     ggplot2::lims(x = c(0,2200)) +
     ggplot2::geom_point(aes(y = Wsym_w0),
                         x = 1000, colour = NA, fill = NA,
                         inherit.aes = FALSE) +
     ggplot2::facet_wrap("ens ~ .", ncol = 1, scales = 'free_y') +
     ggplot2::theme_bw() +
     ggplot2::geom_label(data = label_dat,
                        aes(x = x, y = y_Wsym, label = label),
                        size = unit(1.8, 'pt'),
                        inherit.aes = FALSE) + 
     ggplot2::labs(x = "$t_{\\mathrm{MD}}$",
                   y = "$t \\frac{d}{dt} [ t^2 E_{\\mathrm{sym}}(t) ]|_{t = w_0^2}$",
                   colour = "ensemble name") +
     ggplot2::theme(strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    plot.margin = margin(t = 0, r = 2, b = 0, l = 0),
                    legend.position = "none")

p2 <- ggplot2::ggplot(data = dat, 
                      aes(y = Wsym_w0, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_histogram(aes(after_stat(ndensity),
                                 fill = interaction(ens,stream,sep=""))) +
     ggplot2::facet_wrap("ens ~ .", ncol = 1, scales = 'free_y') +
     ggplot2::theme_bw() +
     ggplot2::labs(y = "",
                   x = "",
                   colour = "ensemble name") +
     ggplot2::guides(y = "none", x = "none") +
     ggplot2::theme(strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title = element_blank(),
                    legend.position = "none",
                    axis.ticks.length = unit(0, "pt"),
                    plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

layout <- "AAAAAB"

combined <- (p1 + p2)
combined <- combined +
            patchwork::plot_layout(guides = "collect", design = layout)#widths = c(1,1), heights = c(2,2))    
plot(combined)    

p1 <- ggplot2::ggplot(dat = dat, 
                     aes(x = md_idx, y = Qsym_w0, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_line() +
     ggplot2::geom_point(aes(y = Qsym_w0),
                         x = 1000, colour = NA, fill = NA,
                         inherit.aes = FALSE) +
     ggplot2::facet_wrap("ens ~ .", ncol = 1, scales = 'free_y') +
     ggplot2::lims(x = c(0,2200)) +
     ggplot2::theme_bw() +
     ggplot2::labs(x = "$t_{\\mathrm{MD}}$",
                   #y = "$\\langle \\varepsilon_{\\mu \\nu \\rho \\gamma}^{\\mathrm{(eucl)}} F_{\\mu \\nu} F_{\\rho \\gamma}(t) \\rangle|_{t=(w_0/a)^2}$",
                   y = "$Q(t)|_{t=w_0^2}$",
                   colour = "ensemble name") +
     ggplot2::geom_label(data = label_dat,
                        aes(x = x, y = y_Qsym, label = label),
                        size = unit(1.8, 'pt'),
                        inherit.aes = FALSE) + 
     ggplot2::theme(strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    plot.margin = margin(t = 0, r = 2, b = 0, l = 0),
                    legend.position = "none")

p2 <- ggplot2::ggplot(data = dat, 
                      aes(y = Qsym_w0, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_histogram(aes(after_stat(ndensity),
                                 fill = interaction(ens,stream,sep=""))) +
     ggplot2::facet_wrap("ens ~ .", ncol = 1, scales = 'free_y') +
     ggplot2::theme_bw() +
     ggplot2::labs(y = "",
                   x = "",
                   colour = "ensemble name") +
     ggplot2::guides(y = "none", x = "none") +
     ggplot2::theme(strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title = element_blank(),
                    legend.position = "none",
                    axis.ticks.length = unit(0, "pt"),
                    plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

combined <- (p1 + p2)
combined <- combined +
            patchwork::plot_layout(guides = "collect", design = layout)#widths = c(1,1), heights = c(2,2))    
plot(combined)    

hadron::tikz.finalize(tikzfiles)

