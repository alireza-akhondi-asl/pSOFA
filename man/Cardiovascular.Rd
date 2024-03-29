% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cardiovascular.R
\name{Cardiovascular}
\alias{Cardiovascular}
\title{Cardiovascular function that calculates a score based on DOPamine,EPINEPHrine,norepinephrine,DOBUTamine, and MAP data}
\usage{
Cardiovascular(
  DT,
  V,
  E,
  Age,
  day_of_interest = 0,
  clinical_events = list(DOPamine = c("DOPamine"), EPINEPHrine = c("EPINEPHrine"),
    norepinephrine = c("norepinephrine"), DOBUTamine = c("DOBUTamine"), MAP = c("MAP"))
)
}
\arguments{
\item{DT}{A numeric vector representing event times represented as days from ICU admission (e.g. for an event at 36 hours after admission, DT=1.5)}

\item{V}{A numeric vector representing the results value of Clinical Events associated with DT}

\item{E}{A character vector representing the type of Event assocaited with DT}

\item{Age}{A numeric value representing the age in months}

\item{day_of_interest}{The day for which the renal score should be calculated (non-negative whole number, starts from 0 for the first 24 hours of ICU stay)}

\item{clinical_events}{A list of 5 string vectors representing the name of the clinical events corresponding to DOPamine, EPINEPHrine, norepinephrine, DOBUTamine, and MAP}
}
\value{
An integer between 0 and 4
}
\description{
Cardiovascular function that calculates a score based on DOPamine,EPINEPHrine,norepinephrine,DOBUTamine, and MAP data
}
