# f_CV ####
f_CV <- function(x) {
  sd(x) / mean(x) * 100
}

# f_smrzCumulContaWorkers ####
f_smrzCumulContaWorkers <- function(
  IL,
  IS,
  nbsim = NULL,
  sampleseed = NULL
) {
  
  if (!is.null(sampleseed)) set.seed(sampleseed)
  
  all_seed <- unique(IS$seed)
  
  if (!is.null(nbsim)) {
    IS %>%
      dplyr::filter(., seed %in% sample(all_seed, nbsim)) -> ISsub
  } else {
    ISsub <- IS
  }
  
  cumul <- subset(ISsub, Day == max(IS$Day))$Infected_cumul
  
  statistics <- c("mean" = mean(cumul),
                  "sd" = sd(cumul),
                  "CV(%)" = f_CV(cumul),
                  quantile(cumul, probs = c(0.025, 0.5, 0.975)))
  
  if (is.null(nbsim)) {
    output <- c("nbsim" = length(all_seed), statistics)
  } else {
    output <- c("nbsim" = nbsim, statistics)
  }

  return(data.frame(cumul=cumul))
}




# f_plotConvCumulContaWorkers ####
f_plotConvCumulContaWorkers <- function(
  IL,
  IS,
  nsim = c(10, 50, 100, 150),
  CVmax = 0.25,
  resampling = 100,
  plotseed = NULL
) {
  if (!is.null(plotseed)) set.seed(plotseed)
  
  all_seed <- unique(IS$seed)
  
  full <- f_smrzCumulContaWorkers(IL = IL, IS = IS)$cumul
  
  indicator_sup <- mean(full) * (1 + CVmax)
  indicator_inf <- mean(full) * (1 - CVmax)
  
  g0 <- ggplot() +
    theme(axis.ticks=element_blank(),
          legend.position = "none",
          panel.background=element_rect(fill="white"),
          plot.title = element_text(face = "bold", size=10),
          axis.title = element_text(size=10),
          axis.text = element_text(size=10),
          panel.grid.major.y=element_line(colour="lightgrey"),
          panel.grid.major.x=element_line(colour="lightgrey"),
          panel.grid.minor.y=element_line(colour="white"),
          panel.grid.minor.x=element_line(colour="lightgrey")) +
    coord_cartesian(xlim = c(0,max(IS$Infected_cumul))) + 
    xlab("") + 
    ylab("")
  
  g_all <- list()
  
  for (g in 1:length(nsim)) {
    g_all <- append(g_all, list(g0))
  }
  
  
  for (i in 1:resampling) {
    for (g in 1:length(nsim)) {
      dg <- f_smrzCumulContaWorkers(IL = IL, IS = IS, nbsim = nsim[[g]])
      
      col_dg <- ifelse(test = between(mean(dg$cumul), indicator_inf, indicator_sup),
                       yes = "darkgreen",
                       no = "darkred")
      
      g_all[[g]] <- g_all[[g]] +
        geom_density(data = dg,
                     mapping = aes(x = cumul),
                     colour = col_dg) +
        geom_vline(xintercept = mean(dg$cumul),
                   size = 0.5, alpha = 0.7, colour = col_dg)
    }
  }
  
  for (g in 1:length(nsim)) {
    g_all[[g]] <- g_all[[g]] +
      labs(title = paste("n =", nsim[[g]], "simulations kept")) +
      geom_vline(xintercept = mean(full), linetype = "solid", size = 1, colour = "black") +
      # geom_vline(xintercept = indicator_inf, linetype = "solid", size = 1, colour = "black") +
      # geom_vline(xintercept = indicator_sup, linetype = "solid", size = 1, colour = "black") +
      annotate("rect", xmin = indicator_inf, xmax = indicator_sup, ymin = -Inf, ymax = Inf,
               alpha = 0.3)
  }
  
  g_output <- ggpubr::ggarrange(plotlist = g_all, ncol = 1)
  annotate_figure(g_output,
                  top = text_grob(paste("Indicator - Cumulative number of infections over 100 workers after a period of 28 days\nConvergence analysis based on a total number of ",length(all_seed), " successful independent simulations\nIndicator distributions obtained by repeatedly (", resampling, " times) keeping n independent simulations\nAcceptability range (gray) determined by fixing the maximum coefficient of variation at ", CVmax, "\n (green: indicator within range, red: indicator out of range)" , sep = ""),
                                  color = "black", size = 11),
                  bottom = "number of infected workers",
                  left = "probability density"
  ) -> g_output

  return(g_output)
}




