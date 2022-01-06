f_initStatusCounterDay1 <- function(
  W,
  prm_workers,
  prm_time,
  seed = NULL
) {
  if (!is.null(seed)) {set.seed(seed)}
  
  W1 <- subset(W, Day == 1)
  Wcomp <- subset(W, Day != 1)
  
  by(W1, ## for the considered day
     INDICES = W1$W_ID, ## processing by worker
     FUN = function(x) {
       return(f_replicateIndividualtime2time(Agent = x, Invariant = "W_status", time_begin = c(0,0), time_end = c(23,55), dt = prm_time$Step))
     }) %>%
    data.table::rbindlist() %>%
    rbind(Wcomp) %>%
    dplyr::arrange(t_ind, W_ID) -> W
  
  W$W_status <- as.character(W$W_status)
  
  ## Random state (in day) of the first initialized contaminated worker
  W$W_statusCounter[which(W$Day == 1 & W$W_status == "initialised as infected")] <- sample(prm_workers$InfectedDay:prm_workers$ContaEndDay, size = 1)
  W$W_statusCounter[which(W$Day == 1 & W$W_status == "susceptible")] <- 0
  
  W$W_status[which(W$Day == 1 &
                     W$W_statusCounter %in% prm_workers$InfectedDay:(prm_workers$InfectiousDay-1))] <- "infected"
  W$W_status[which(W$Day == 1 &
                     W$W_statusCounter %in% prm_workers$InfectiousDay:(prm_workers$SymptomDay-1))] <- "infectious"
  W$W_status[which(W$Day == 1 &
                     W$W_statusCounter %in% prm_workers$SymptomDay:(prm_workers$NonInfectiousDay-1))] <- sample(c("symptomatic", "asymptomatic"),
                                                                                                                size=1,
                                                                                                                prob=c(prm_workers$pSymptom, 1-prm_workers$pSymptom))
  W$W_status[which(W$Day == 1 &
                     W$W_statusCounter %in% prm_workers$NonInfectiousDay:prm_workers$ContaEndDay)] <- "non-infectious"
  W$W_status[which(W$Day == 1 &
                     W$W_statusCounter > prm_workers$ContaEndDay)] <- "recovered"
  
  return(W)
}