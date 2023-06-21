
#' Hepatic function that calculates a score based on Bilirubin data
#'
#' @param DT  A numeric vector representing event times represented as days from ICU admission (e.g. for an event at 36 hours after admission, DT=1.5)
#' @param V A numeric vector representing the results value of Clinical Events associated with DT
#' @param E A character vector representing the type of Event assocaited with DT
#' @param day_of_interest The day for which the hepatic score should be calculated (non-negative whole number, starts from 0 for the first 24 hours of ICU stay)
#' @param clinical_events A character string vector representing the name of the clinical events corresponding to Bilirubin (default value is "Bilirubin")
#' @return An integer between 0 and 4
#' @examples
#'
Hepatic <- function(DT, V, E, day_of_interest = 0, clinical_events = c("Bilirubin")) {
  
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
  
  # Filter the V vector by the specified day of interest and test name of E
  V <- (V[(E %in% clinical_events) & (as.integer(DT) == day_of_interest)])
  
  # If there are no data available for the specified test name and day of interest, return a score of 0
  if (length(V) == 0) return(score <- 0)
  
  # Calculate the hepatic score based on the minimum value of the filtered V vector
  score <- cut(max(V),breaks=c(-Inf,1.2,2.0,6.0,12.0,Inf),labels=FALSE,right=FALSE)-1  
  # Return the calculated score
  return(score)
}
