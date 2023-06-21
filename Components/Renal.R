
#' Renal function that calculates a score based on Creatinine data
#'
#' @param DT  A numeric vector representing event times represented as days from ICU admission (e.g. for an event at 36 hours after admission, DT=1.5)
#' @param V A numeric vector representing the results value of Clinical Events associated with DT
#' @param E A character vector representing the type of Event assocaited with DT
#' @param Age A numeric value representing the age in months
#' @param day_of_interest The day for which the renal score should be calculated (non-negative whole number, starts from 0 for the first 24 hours of ICU stay)
#' @param clinical_events A character string vector representing the name of the clinical events corresponding to Creatinine (default value is "Creatinine")
#' @return An integer between 0 and 4
#' @examples
#'
Renal <- function(DT, V, E, Age, day_of_interest = 0, clinical_events = c("Creatinine")) {
  
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

  # Check that Age is a non-negative  number
  if (!is.numeric(Age) || Age < 0) {
    stop("Age must be a non-negative number")
  }

  # Filter the V vector by the specified day of interest and test name of E
  V <- (V[(E %in% clinical_events) & (as.integer(DT) == day_of_interest)])
  
  # If there are no data available for the specified test name and day of interest, return a score of 0
  if (length(V) == 0) return(score <- 0)
  
  # Calculate the renal score based on the values of the filtered V vector
  V=max(V)
  if (Age<1)
  {
    score <- cut(V,breaks=c(-Inf,0.8,1.0,1.2,1.6,Inf),labels=FALSE,right=FALSE)-1
  }
  else if (Age <12)
  {
    score <- cut(V,breaks=c(-Inf,0.3,0.5,0.8,1.2,Inf),labels=FALSE,right=FALSE)-1
    
  }
  else if (Age <24)
  {
    score <- cut(V,breaks=c(-Inf,0.4,0.6,1.1,1.5,Inf),labels=FALSE,right=FALSE)-1
    
  }
  else if (Age <60)
  {
    score <- cut(V,breaks=c(-Inf,0.6,0.9,1.6,2.3,Inf),labels=FALSE,right=FALSE)-1
    
  }
  else if (Age <144)
  {
    score <- cut(V,breaks=c(-Inf,0.7,1.1,1.8,2.6,Inf),labels=FALSE,right=FALSE)-1
    
  }
  else if (Age<=216)
  {
    score <- cut(V,breaks=c(-Inf,1.0,1.7,2.9,4.2,Inf),labels=FALSE,right=FALSE)-1
    
  }
  else
  {
    score <- cut(V,breaks=c(-Inf,1.2,2.0,3.5,5.0,Inf),labels=FALSE,right=FALSE)-1
    
  }
  
  # Return the calculated score
  return(score)
}
