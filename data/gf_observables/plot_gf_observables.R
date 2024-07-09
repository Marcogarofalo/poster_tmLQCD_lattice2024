require(ggplot2)
require(hadron)
require(dplyr)

ensembles <- c("cAp211.085.48", "cB211.072.64", "cB211.072.96", "cC211.06.80", 
               "cD211.054.96_BC", "cD211.054.96_DE", "cE211ab.044.112", "cE211cd.044.112")
as <- c(0.087, 0.080, 0.080, 0.068, 
        0.057, 0.057,  0.049, 0.049)
Ls <- c(48, 64, 96, 80, 
        96, 96, 112, 112)

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
    md_offset <- 1250
  } else if ( ens == "cE211cd.044.112" ){
    md_offset <- 2200
  }

  stream_id <- ""
  if( ens == "cD211.054.96_DE" || ens == "cE211cd.044.112" ){
    stream_id <- "b"
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

label_dat <- data.frame(ens = c(ensembles[1:4], "cD211.054.96", "cE211.044.112"),
                        label = sprintf("$a \\sim %.3f$ fm, $L/a = %d$",
                                        c(0.087, 0.080, 0.080, 0.068, 0.057, 0.049),
                                        c(48, 64, 96, 80, 96, 112)),
                        x = rep(2600, times=6),
                        y_Wsym = dat_stat$Wsym_w0_min + 0.10*abs(dat_stat$Wsym_w0_max-dat_stat$Wsym_w0_min),
                        y_Qsym = dat_stat$Qsym_w0_min + 0.10*abs(dat_stat$Qsym_w0_max-dat_stat$Qsym_w0_min))

tikzfiles <- hadron::tikz.init(basename = "gf_observables_md_histories", width = 3, height = 4)

p <- ggplot2::ggplot(data = dat, 
                     aes(x = md_idx, y = Wsym_w0, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_line() +
     ggplot2::lims(x = c(0,3200)) +
     ggplot2::facet_wrap("ens ~ .", ncol = 1, scales = 'free_y') +
     ggplot2::theme_bw() +
     ggplot2::geom_label(data = label_dat,
                        aes(x = x, y = y_Wsym, label = label),
                        size = unit(2, 'pt'),
                        inherit.aes = FALSE) + 
     ggplot2::labs(x = "$t_{\\mathrm{MD}}$",
                   y = "$\\langle t \\frac{d}{dt} [ t^2 E_{\\mathrm{sym}}(t) ]\\rangle|_{t = (w_0/a)^2}$",
                   colour = "ensemble name") +
     ggplot2::theme(strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    legend.position = "none")
plot(p)

p <- ggplot2::ggplot(dat = dat, 
                     aes(x = md_idx, y = Qsym_w0, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_line() +
     ggplot2::facet_wrap("ens ~ .", ncol = 1, scales = 'free_y') +
     ggplot2::lims(x = c(0,3200)) +
     ggplot2::theme_bw() +
     ggplot2::labs(x = "$t_{\\mathrm{MD}}$",
                   #y = "$\\langle \\varepsilon_{\\mu \\nu \\rho \\gamma}^{\\mathrm{(eucl)}} F_{\\mu \\nu} F_{\\rho \\gamma}(t) \\rangle|_{t=(w_0/a)^2}$",
                   y = "$Q(t)|_{t=(w_0/a)^2}$",
                   colour = "ensemble name") +
     ggplot2::geom_label(data = label_dat,
                        aes(x = x, y = y_Qsym, label = label),
                        size = unit(2, 'pt'),
                        inherit.aes = FALSE) + 
     ggplot2::theme(strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    legend.position = "none")
plot(p)

hadron::tikz.finalize(tikzfiles)

tikzfiles <- hadron::tikz.init(basename = "gf_observables_tauint", width = 2.5, height = 2.5) 

p <- ggplot2::ggplot(data = gf_summary_dat,
                     aes(x = a^2, y = Wsym_w0_tauint, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_smooth(aes(x = a^2, y = Wsym_w0_tauint, weight = 1.0/Wsym_w0_dtauint^2),
                          method = 'lm', se = FALSE, inherit.aes = FALSE) +
     ggplot2::geom_point() +
     ggplot2::geom_errorbar(aes(ymin = Wsym_w0_tauint - Wsym_w0_dtauint, ymax = Wsym_w0_tauint + Wsym_w0_dtauint),
                            width = 0.0) + 
     ggplot2::theme_bw() +
     ggplot2::labs(x = "$a^2$ [fm$^2$]",
                   y = "$\\tau_{\\mathrm{int}}[w^{\\mathrm{sym}}_0/a]$") +
     ggplot2::theme(legend.position = 'none')
plot(p)

p <- ggplot2::ggplot(data = gf_summary_dat,
                     aes(x = a^(-5), y = Qsym_w0_tauint, colour = interaction(ens,stream,sep=""))) +
     ggplot2::geom_smooth(aes(x = a^(-5), y = Qsym_w0_tauint, weight = 1.0/Qsym_w0_dtauint^2),
                          method = 'lm', se = FALSE, inherit.aes = FALSE) +
     ggplot2::geom_point() +
     ggplot2::geom_errorbar(aes(ymin = Qsym_w0_tauint - Qsym_w0_dtauint, ymax = Qsym_w0_tauint + Qsym_w0_dtauint),
                            width = 0.0) + 
     ggplot2::theme_bw() +
     ggplot2::scale_x_continuous(trans = 'log10') +
     ggplot2::labs(x = "$a^{-5}$ [fm$^{-5}$]",
                   y = "$\\tau_{\\mathrm{int}}[Q(w_0/a)]$") +
     ggplot2::theme(legend.position = 'none')
plot(p)
 
hadron::tikz.finalize(tikzfiles)
