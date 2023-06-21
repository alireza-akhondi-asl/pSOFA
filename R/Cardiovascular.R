
#' Cardiovascular function that calculates a score based on DOPamine,EPINEPHrine,norepinephrine,DOBUTamine, and MAP data
#'
#' @param DT  A numeric vector representing event times represented as days from ICU admission (e.g. for an event at 36 hours after admission, DT=1.5)
#' @param V A numeric vector representing the results value of Clinical Events associated with DT
#' @param E A character vector representing the type of Event assocaited with DT
#' @param Age A numeric value representing the age in months
#' @param day_of_interest The day for which the renal score should be calculated (non-negative whole number, starts from 0 for the first 24 hours of ICU stay)
#' @param clinical_events A list of 5 string vectors representing the name of the clinical events corresponding to DOPamine, EPINEPHrine, norepinephrine, DOBUTamine, and MAP
#' @return An integer between 0 and 4
#' @examples
#'
Cardiovascular <- function(DT, V, E, Age, day_of_interest = 0, clinical_events = list(DOPamine=c("DOPamine"),EPINEPHrine=c("EPINEPHrine"),norepinephrine=c("norepinephrine"),DOBUTamine=c("DOBUTamine"),MAP=c("MAP"))) {
  
  # Check that DT, V, and E have the same length
  if (length(DT) != length(V) || length(V) != length(E)) {
    stop("DT, V, and E must have the same length")
  }
  
  # Check that DT is a numeric vector
  if (!is.numeric(DT)) {
    stop("DT must be a numeric vector")
  }
  
  # Check that V is a numeric vector
  if (!is.numeric(V)) {
    stop("V must be a numeric vector")
  }
  
  # Check that E is a character vector
  if (!is.character(E)) {
    stop("E must be a character vector")
  }
  
  # Check that day_of_interest is a non-negative whole number
  if (!is.numeric(day_of_interest) || day_of_interest %% 1 != 0 || day_of_interest < 0) {
    stop("day_of_interest must be a non-negative whole number. Starts from 0 for the first 24 hours of ICU stay")
  }

  # check that clinical_events are defined
  if (!identical(sort(names(clinical_events)), sort(c("DOPamine","EPINEPHrine","norepinephrine","DOBUTamine","MAP")))) {
    stop("clinical_events must have DOPamine,EPINEPHrine,norepinephrine,DOBUTamine, and MAP components.")
  } 


  # Check that Age is a non-negative  number
  if (!is.numeric(Age) || Age < 0) {
    stop("Age must be a non-negative number")
  }

  calculate_dose <- function(clinical_event)
 {
  data_t <- DT[E %in% clinical_event]
  if (length(data_t)==0) return(list(0,0)) #No data available
  data_v <- V[E %in% clinical_event]
  # We add 0 infusion rate at a negative time
  data_t <- c(-0.01,data_t)
  data_v <- c(0,data_v)
  
  # We find the last infusion rate time before start of the day of interest
  ts <-max(data_t[data_t<day_of_interest])
  data <- data_v[(data_t<day_of_interest+1)&(data_t>=ts)]
  t <-data_t[(data_t<day_of_interest+1)&(data_t>=ts)]
  #Since the last infusion rate might cross over the next day, we need to add the last infusion rate to the end of day of interest.
  t <- c(t,day_of_interest+1)
  data <-c(data,data[length(data)])
  dt=diff(t)
  data <- data[1:length(data)-1]
  return(list(dt,data))
 }
  out <- calculate_dose(clinical_events$DOPamine)
  dt_dopamine <- out[[1]]
  dopamine <- out[[2]]
  out <- calculate_dose(clinical_events$EPINEPHrine)
  dt_epinephine <- out[[1]]
  epinephine <- out[[2]]
  out <- calculate_dose(clinical_events$DOBUTamine)
  dt_dobutamine <- out[[1]]
  dobutamine <- out[[2]]
  out <- calculate_dose(clinical_events$norepinephrine)
  dt_norepinephrine <- out[[1]]
  norepinephrine <- out[[2]]
  
  C1 <- sum((dopamine>15)*dt_dopamine)>=1/24.0
  C2 <- sum((epinephine>0.1)*dt_epinephine)>=1/24.0
  C3 <- sum((norepinephrine>0.1)*dt_norepinephrine)>=1/24.0
  C <- (C1|C2|C3)
  if (C)  return(score <- 4)
  
  C1 <- sum((dopamine>5)*dt_dopamine)>=1/24.0
  C2 <- sum((epinephine>0)*dt_epinephine)>=1/24.0
  C3 <- sum((norepinephrine>0)*dt_norepinephrine)>=1/24.0
  C <- (C1|C2|C3)
  if (C) return(score <- 3)
  
  C1 <- sum((dopamine>0)*dt_dopamine)>=1/24.0
  C2 <- sum((dobutamine>0)*dt_dobutamine)>=1/24.0
  C <- (C1|C2)
  if (C) return(score <- 2)
  # Infusions are lower than defined thresholds so we check the MAP based on Age
  MAP <-   (V[(E %in% clinical_events$MAP) & (as.integer(DT)==day_of_interest)])
  if (length(MAP)==0) return(score <- 0)
  MAP=min(MAP)
  if (Age<1)
  {
    if(MAP<46) return(score <- 1) else return (score <- 0)
  }
  else if (Age <12)
  {
    if(MAP<55) return(score <- 1) else return (score <- 0)
    
  }
  else if (Age <24)
  {
    if(MAP<60) return(score <- 1) else return (score <- 0)
    
  }
  else if (Age <60)
  {
    if(MAP<62) return(score <- 1) else return (score <- 0)
    
  }
  else if (Age <144)
  {
    if(MAP<65) return(score <- 1) else return (score <- 0)
    
  }
  else if (Age <=216)
  {
    if(MAP<67) return(score <- 1) else return (score <- 0)
    
  }
  else if (Age>216)
  {
    if(MAP<70) return(score <- 1) else return (score <- 0)
    
  }

}
