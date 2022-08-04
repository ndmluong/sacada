# f_smrzCumulContaWorkers 
f_smrzCumulContaWorkers <- function(
    IL,
    IS,
    nbsim = NULL,
    sampleseed = NULL
) {
  
  if (!is.null(sampleseed)) set.seed(sampleseed)
  
  all_seed <- unique(IS$seed)
  
  if (!is.null(nbsim)) {
    if (nbsim > length(all_seed)) stop(">>> required number of simulations not sufficient !")
    IS %>%
      dplyr::filter(., seed %in% sample(all_seed, nbsim)) -> ISsub
  } else {
    ISsub <- IS
  }
  
  ISsub %>%
    dplyr::filter(., Day == max(IS$Day)) %>%
    dplyr::group_by(., seed) %>%
    dplyr::summarise(., cumul = Infected_cumul) %>%
    as.data.frame() -> output
  
  return(output)
}




# f_plotConvCumulContaWorkers - Plot check - Indicator : cumulative number of infected workers
f_plotConvCumulContaWorkers <- function(
    IL,
    IS,
    keptsim = c(10, 50, 100, 150), # number of kept simulations
    CVmax = 0.20, # acceptable maximum value for the coefficient of variation
    resampling = 100, # number of keeping iterations
    plotseed = NULL # for traceability purposes
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
  
  for (g in 1:length(keptsim)) {
    g_all <- append(g_all, list(g0))
  }
  
  
  for (i in 1:resampling) {
    for (g in 1:length(keptsim)) {
      dg <- f_smrzCumulContaWorkers(IL = IL, IS = IS, nbsim = keptsim[[g]])
      
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
  
  for (g in 1:length(keptsim)) {
    g_all[[g]] <- g_all[[g]] +
      labs(title = paste("n =", keptsim[[g]], "simulations kept")) +
      geom_vline(xintercept = mean(full), linetype = "solid", size = 1, colour = "black") +
      annotate("rect", xmin = indicator_inf, xmax = indicator_sup, ymin = -Inf, ymax = Inf,
               alpha = 0.3)
  }
  
  g_output <- ggpubr::ggarrange(plotlist = g_all, ncol = 1)
  annotate_figure(g_output,
                  bottom = text_grob(paste("Cumulative number of infections over ", length(unique(IL$W_ID)) ,
                                           " workers after a period of ", max(IS$Day)," days" , sep = ""),
                                     color = "black", size = 7),
                  top = text_grob("Cumulative number of infected workers", face = "bold", size = 14),
                  left = ""
  ) -> g_output
  
  return(g_output)
}




# f_smrzAverageFoodContaRatio
f_smrzAverageFoodContaRatio <- function(
    FPS,
    detection = 5, # (integer) expressed in log10: the detection detection/quantification for the number of RNA copies
    nbsim = NULL,
    sampleseed = NULL
){
  
  if (!detection %in% 3:9) {
    stop(">>> detection threshold not available!\n  >>> possible values: 3 4 5 6 7 8 9")
  }
  
  if (!is.null(sampleseed)) set.seed(sampleseed)
  
  all_seed <- unique(FPS$seed)
  
  if (!is.null(nbsim)) {
    if (nbsim > length(all_seed)) stop(">>> required number of simulations not sufficient !")
    FPS %>%
      dplyr::filter(., seed %in% sample(all_seed, nbsim)) -> FPSsub
  } else {
    FPSsub <- FPS
  }
  
  detection_var <- paste("pos_FP_", detection, "log", sep = "")
  
  FPSsub %>%
    dplyr::filter(., nb_FP > 0) %>%
    dplyr::mutate(., dailycontaratio = .[[detection_var]] / nb_FP) %>%
    dplyr::group_by(., seed) %>%
    dplyr::summarise(., contaratio = mean(dailycontaratio)) %>%
    as.data.frame() -> output
  
  return(output)
}






# f_plotConvFoodContaRatio - Plot check - Indicator : average food contamination ratio during the whole simulation period
f_plotConvFoodContaRatio <- function(
    FPS,
    detection = 5,
    keptsim = c(10, 50, 100, 150), # number of kept simulations
    CVmax = 0.20, # acceptable maximum value for the coefficient of variation
    resampling = 100, # number of keeping iterations
    plotseed = NULL # for traceability purposes
) {
  if (!is.null(plotseed)) set.seed(plotseed)
  
  all_seed <- unique(FPS$seed)
  
  full <- f_smrzAverageFoodContaRatio(FPS = FPS, detection = detection)$contaratio
  
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
    coord_cartesian(xlim = c(0, max(full))) + 
    xlab("") + 
    ylab("")
  
  g_all <- list()
  
  for (g in 1:length(keptsim)) {
    g_all <- append(g_all, list(g0))
  }
  
  
  for (i in 1:resampling) {
    for (g in 1:length(keptsim)) {
      dg <- f_smrzAverageFoodContaRatio(FPS = FPS, detection = detection, nbsim = keptsim[[g]])
      
      col_dg <- ifelse(test = between(mean(dg$contaratio), indicator_inf, indicator_sup),
                       yes = "darkgreen",
                       no = "darkred")
      
      g_all[[g]] <- g_all[[g]] +
        geom_density(data = dg,
                     mapping = aes(x = contaratio),
                     colour = col_dg) +
        geom_vline(xintercept = mean(dg$contaratio),
                   size = 0.5, alpha = 0.7, colour = col_dg)
    }
  }
  
  for (g in 1:length(keptsim)) {
    g_all[[g]] <- g_all[[g]] +
      labs(title = paste("n =", keptsim[[g]], "simulations kept")) +
      geom_vline(xintercept = mean(full), linetype = "solid", size = 1, colour = "black") +
      annotate("rect", xmin = indicator_inf, xmax = indicator_sup, ymin = -Inf, ymax = Inf,
               alpha = 0.3)
  }
  
  g_output <- ggpubr::ggarrange(plotlist = g_all, ncol = 1)
  annotate_figure(g_output,
                  bottom = text_grob(paste("Average food contamination ratio over a period of ", max(FPS$Day)+1,
                                           " days\n(detection threshold: ", detection, " log10 ARN copies)", sep = ""),
                                     color = "black", size = 7),
                  top = text_grob("Average food contamination ratio", face = "bold", size = 14),
                  left = ""
  ) -> g_output
  
  return(g_output)
}







# f_smrzAverageSurfaceContaRatio
f_smrzAverageSurfacesContaRatio <- function(
    SS,
    plant,
    detection = 5, # (integer) expressed in log10: the detection detection/quantification for the number of RNA copies
    nbsim = NULL,
    sampleseed = NULL
){
  
  if (!detection %in% 3:9) {
    stop(">>> detection threshold not available!\n  >>> possible values: 3 4 5 6 7 8 9")
  }
  
  if (!is.null(sampleseed)) set.seed(sampleseed)
  
  all_seed <- unique(SS$seed)
  
  plant$L %>%
    dplyr::filter(., location %in% c("Conveyor1", "Conveyor2", "Equipment 1")) %>%
    nrow() -> nb_S
  
  if (!is.null(nbsim)) {
    if (nbsim > length(all_seed)) stop(">>> required number of simulations not sufficient !")
    SS %>%
      dplyr::filter(., seed %in% sample(all_seed, nbsim)) -> SSsub
  } else {
    SSsub <- SS
  }
  
  detection_var <- paste("pos_S_", detection, "log", sep = "")
  
  SSsub %>%
    dplyr::mutate(., dailycontaratio = .[[detection_var]] / nb_S) %>%
    dplyr::group_by(., seed) %>%
    dplyr::summarise(., contaratio = mean(dailycontaratio)) %>%
    as.data.frame() -> output
  
  return(output)
}




# f_plotConvSurfacesContaRatio - Plot check - Indicator : average surfaces contamination ratio during the whole simulation period
f_plotConvSurfacesContaRatio <- function(
    SS,
    plant,
    detection = 5,
    keptsim = c(10, 50, 100, 150), # number of kept simulations
    CVmax = 0.20, # acceptable maximum value for the coefficient of variation
    resampling = 100, # number of keeping iterations
    plotseed = NULL # for traceability purposes
) {
  if (!is.null(plotseed)) set.seed(plotseed)
  
  all_seed <- unique(SS$seed)
  
  full <- f_smrzAverageSurfacesContaRatio(SS = SS, plant = plant, detection = detection)$contaratio
  
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
    coord_cartesian(xlim = c(0, max(full))) + 
    xlab("") + 
    ylab("")
  
  g_all <- list()
  
  for (g in 1:length(keptsim)) {
    g_all <- append(g_all, list(g0))
  }
  
  
  for (i in 1:resampling) {
    for (g in 1:length(keptsim)) {
      dg <- f_smrzAverageSurfacesContaRatio(SS = SS, plant = plant, detection = detection,
                                            nbsim = keptsim[[g]])
      
      col_dg <- ifelse(test = between(mean(dg$contaratio), indicator_inf, indicator_sup),
                       yes = "darkgreen",
                       no = "darkred")
      
      g_all[[g]] <- g_all[[g]] +
        geom_density(data = dg,
                     mapping = aes(x = contaratio),
                     colour = col_dg) +
        geom_vline(xintercept = mean(dg$contaratio),
                   size = 0.5, alpha = 0.7, colour = col_dg)
    }
  }
  
  for (g in 1:length(keptsim)) {
    g_all[[g]] <- g_all[[g]] +
      labs(title = paste("n =", keptsim[[g]], "simulations kept")) +
      geom_vline(xintercept = mean(full), linetype = "solid", size = 1, colour = "black") +
      annotate("rect", xmin = indicator_inf, xmax = indicator_sup, ymin = -Inf, ymax = Inf,
               alpha = 0.3)
  }
  
  plant$L %>%
    dplyr::filter(., location %in% c("Conveyor1", "Conveyor2", "Equipment 1")) %>%
    nrow() -> nb_S
  
  g_output <- ggpubr::ggarrange(plotlist = g_all, ncol = 1)
  annotate_figure(g_output,
                  bottom = text_grob(paste("Average surfaces contamination ratio over a period of ", max(SS$Day)+1,
                                           " days\n(detection threshold: ", detection,
                                           " log10 ARN copies)\n(ratio between the number of contaminated one-square-meter tiles amont a total of ",
                                           nb_S, " tiles)" , sep = ""),
                                     color = "black", size = 7),
                  top = text_grob(paste("Average surfaces contamination ratio", sep =""), face = "bold", size = 14),
                  left = ""
  ) -> g_output
  
  return(g_output)
}






# f_estimateR()
f_estimateRt <- function(
    ISsub, ## data.frame (Infection summary), output of the function f_runModel associated with ONE GIVEN SIMULATION SEED
    prm_conta,
    prm_workers
){
  ISsub <- tibble::add_column(ISsub, Incidence = NA, .after = "Day")
  ISsub$Incidence[1] <- ISsub$Infected_cumul[1]
  for (i in 2:nrow(ISsub)) {
    ISsub$Incidence[i] <- ISsub$Infected_cumul[i] - ISsub$Infected_cumul[i-1]
  }
  
  MyInci <- data.frame(dates = as.numeric(ISsub$Day),
                       I = ISsub$Incidence)
  
  if (sum(MyInci$I) <= 1) {
    
    return(NA)
    
  } else {
    
    ## width of the sliding windows for calculating Rt
    MyInci$dates[MyInci$I > 0] %>%
      diff() %>%
      max() -> bwinfections
    
    tstep <- max(prm_workers$RecoveredDay - prm_workers$InfectedDay,
                 bwinfections)
    
    DayLastInci <- max(MyInci$dates[MyInci$I > 0])
    
    # beginning points for the windows
    if (DayLastInci < 2) {
      R_output <- 0
    } else {
      if (DayLastInci < tstep) {
        t_start <- seq(2, DayLastInci)
      } else {
        t_start <- seq(2, nrow(MyInci) - tstep)
      }
      # ending points for the windows
      t_end <- t_start + tstep
      
      # if the some ending points of the windows (t_end) exceed the last day of the simulation, remove them and correct t_start
      t_end <- t_end[t_end <= max(MyInci$dates)]
      t_start <- t_end - tstep
      
      options(warn = -1)
      Rt_res <- EpiEstim::estimate_R(incid = MyInci,
                                     method = "parametric_si",
                                     config = make_config(list(mean_si = prm_conta$SerialInterval[[prm_conta$VoC]][["mu"]],
                                                               std_si = prm_conta$SerialInterval[[prm_conta$VoC]][["sigma"]],
                                                               t_start = t_start,
                                                               t_end = t_end)))
      options(warn = 0)
      R_output <- mean(Rt_res$R$`Mean(R)`)
      
    }
    
    return(R_output)
  }
  
}





# f_smrzRt
f_smrzRt <- function(
    IS,
    prm_conta,
    prm_workers,
    nbsim = NULL,
    sampleseed = NULL
) {
  
  if (!is.null(sampleseed)) set.seed(sampleseed)
  
  all_seed <- unique(IS$seed)
  
  if (!is.null(nbsim)) {
    if (nbsim > length(all_seed)) stop(">>> required number of simulations not sufficient !")
    
    chosen_seed <- sample(all_seed, nbsim)
  } else {
    chosen_seed <- all_seed
  }
  
  ISsub <- dplyr::filter(IS, seed %in% chosen_seed)
  
  Rt <- sapply(chosen_seed, function(x) (f_estimateRt(ISsub = subset(ISsub, seed == x),
                                                      prm_conta = Parms_Conta,
                                                      prm_workers = Parms_Workers)))
  
  output <- data.frame(seed = chosen_seed,
                       Rt = Rt)
  
  return(output)
}







# f_plotConvRt - Plot check - Indicator : average surfaces contamination ratio during the whole simulation period
f_plotConvRt <- function(
    IS,
    prm_conta,
    prm_workers,
    keptsim = c(10, 50, 100, 150), # number of kept simulations
    CVmax = 0.20, # acceptable maximum value for the coefficient of variation
    resampling = 100, # number of keeping iterations
    plotseed = NULL # for traceability purposes
) {
  if (!is.null(plotseed)) set.seed(plotseed)
  
  all_seed <- unique(IS$seed)
  
  full <- f_smrzRt(IS = IS, prm_conta = prm_conta, prm_workers = prm_workers)$Rt
  
  indicator_sup <- mean(full, na.rm = TRUE) * (1 + CVmax)
  indicator_inf <- mean(full, na.rm = TRUE) * (1 - CVmax)
  
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
    coord_cartesian(xlim = c(0, max(full, na.rm = TRUE))) + 
    xlab("") + 
    ylab("")
  
  g_all <- list()
  
  for (g in 1:length(keptsim)) {
    g_all <- append(g_all, list(g0))
  }
  
  for (i in 1:resampling) {
    setTxtProgressBar(txtProgressBar(min = 1, max = resampling, initial = 1,
                                     char = "-", width = 50, style = 3),
                      value = i)
    
    for (g in 1:length(keptsim)) {
      dg <- f_smrzRt(IS = IS, prm_conta = prm_conta, prm_workers = prm_workers,
                     nbsim = keptsim[[g]])
      
      while (sum(is.na(dg$Rt)) == length(dg$Rt)) { # if the re-sampling step provides only NA values, re-start
        dg <- f_smrzRt(IS = IS, prm_conta = prm_conta, prm_workers = prm_workers,
                       nbsim = keptsim[[g]])
      }
      
      col_dg <- ifelse(test = between(mean(dg$Rt, na.rm = TRUE), indicator_inf, indicator_sup),
                       yes = "darkgreen",
                       no = "darkred")
      
      g_all[[g]] <- g_all[[g]] +
        geom_density(data = dg,
                     mapping = aes(x = Rt),
                     colour = col_dg) +
        geom_vline(xintercept = mean(dg$Rt),
                   size = 0.5, alpha = 0.7, colour = col_dg)
    }
  }
  
  for (g in 1:length(keptsim)) {
    g_all[[g]] <- g_all[[g]] +
      labs(title = paste("n =", keptsim[[g]], "simulations kept")) +
      geom_vline(xintercept = mean(full, na.rm=T), linetype = "solid", size = 1, colour = "black") +
      annotate("rect", xmin = indicator_inf, xmax = indicator_sup, ymin = -Inf, ymax = Inf,
               alpha = 0.3)
  }
  
  
  g_output <- ggpubr::ggarrange(plotlist = g_all, ncol = 1)
  annotate_figure(g_output,
                  bottom = text_grob(paste("Average transmisison rate over a period of 28 days\n", sep = ""),
                                     color = "black", size = 7),
                  top = text_grob(paste("Average transmisison rate Rt", sep =""), face = "bold", size = 14),
                  left = ""
  ) -> g_output
  
  return(g_output)
}







# f_plotConvAllIndicators
f_plotConvAllIndicators <- function(
    IL,
    IS,
    FPS,
    SS,
    plant,
    detection,
    prm_conta,
    prm_workers,
    keptsim = c(10, 50, 100, 150),
    CVmax = 0.20,
    resampling = 100,
    plotseed = NULL  
) {
  
  all_seed <- unique(IS$seed)
  
  writeLines(">>> Indicator 1 - Cumulative number of infected workers")
  f_plotConvCumulContaWorkers(IL = IL, IS = IS,
                              keptsim = keptsim,
                              resampling = resampling,
                              plotseed = plotseed,
                              CVmax = CVmax) -> g_ConvCumulContaWorkers
  
  writeLines(">>> Indicator 2 - Transmission rate")
  f_plotConvRt(IS = IS, prm_conta = Parms_Conta, prm_workers = Parms_Workers,
               keptsim = keptsim,
               CVmax = CVmax,
               resampling = resampling,
               plotseed = plotseed) -> g_ConvRt
  
  writeLines("\n>>> Indicator 3 - Average food contamination ratio")
  f_plotConvFoodContaRatio(FPS,
                           detection = 5,
                           keptsim = keptsim,
                           resampling = resampling,
                           plotseed = plotseed,
                           CVmax = CVmax) -> g_ConvFoodContaRatio
  
  writeLines(">>> Indicator 4 - Average surface contamination ratio")
  f_plotConvSurfacesContaRatio(SS = SS, 
                               plant = plant,
                               detection = detection,
                               keptsim = keptsim,
                               resampling = resampling,
                               plotseed = plotseed,
                               CVmax = CVmax) -> g_ConvSurfacesContaRatio
  
  ggpubr::ggarrange(plotlist = list(g_ConvCumulContaWorkers,
                                    g_ConvRt,
                                    g_ConvFoodContaRatio,
                                    g_ConvSurfacesContaRatio
  ),
  nrow = 1) %>%
    annotate_figure(.,
                    top = text_grob("Convergence analysis with different output indicators", face = "bold", size = 20),
                    left = text_grob("Probability density (area under curves), mean values (vertical thick lines) and acceptability band", rot = 90,  face = "bold", size = 12),
                    bottom = text_grob(paste("Convergence analysis based on a total number of ", length(all_seed), " successful independent simulations\nIndicator distributions obtained by repeatedly (", resampling, " times) keeping n independent simulations\nAcceptability range (gray) determined by fixing the maximum coefficient of variation at ", CVmax, "\n (green: indicator within range, red: indicator out of range)" , sep = ""),
                                       face = "italic", size = 8)
    ) -> g_Convergence
  
  return(g_Convergence)
  
}









# f_checkout : extract raw output from different (sub) directories of simulation_output
f_checkout <- function(
    scenario
) {
  
  list.files(path = "simulation_output/", pattern =".RData", recursive = TRUE) %>%
    .[str_detect(., pattern = scenario)] %>% 
    paste("simulation_output/", ., sep = "") %>%
    lapply(., function(x) {
      load(file = x)
      return(OUTPUT_seedx)
    }) %>%
    `names<-`(., sapply(., function(x) x$seed)) 
  
}



# f_summaryOutput : combine extracted raw output into one object containing multiple simulations
f_summaryOutput <- function(
    rawoutput
) {
  
  ## all seed number
  sapply(rawoutput, function(x) x$seed) %>%
    as.numeric() -> all_seeds
  
  ## summarize infection log for all seeds
  lapply(rawoutput, function(x) {
    data.frame(seed = rep(x$seed, nrow(x$InfectionLog)),
               x$InfectionLog)
  }) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(., seed = as.factor(seed)) -> IL 
  
  ## summarize infection summary for all seeds
  lapply(rawoutput, function(x) x$InfectionSummary) %>%
    data.table::rbindlist() %>%
    dplyr::relocate(., seed) %>%
    dplyr::mutate(., seed = as.factor(seed)) -> IS
  
  ## summarize surfaces contamination for all seeds
  lapply(rawoutput, function(x) {
    data.frame(seed = rep(x$seed, nrow(x$S_summary)),
               x$S_summary)
  }) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(., seed = as.factor(seed)) -> SS
  
  ## summarize food portions contamination for all seeds
  lapply(rawoutput, function(x) {
    data.frame(seed = rep(x$seed, nrow(x$FP_summary)),
               x$FP_summary)
  }) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(., seed = as.factor(seed)) -> FPS
  
  return(list(all_seeds = all_seeds,
              IL = IL,
              IS = IS,
              SS = SS,
              FPS = FPS))
}


























