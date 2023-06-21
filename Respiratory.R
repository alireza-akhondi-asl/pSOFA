
#' Respiratory function that calculates a score based on based on Respiratory , SPO2,PaO2, and FiO2 data 
#'
#' @param DT  A numeric vector representing event times represented as days from ICU admission (e.g. for an event at 36 hours after admission, DT=1.5)
#' @param V A numeric vector representing the results value of Clinical Events associated with DT
#' @param E A character vector representing the type of Event assocaited with DT
#' @param day_of_interest The day for which the respiratory score should be calculated (non-negative whole number, starts from 0 for the first 24 hours of ICU stay)
#' @param maxlag A positive number for the windwo size in hours to check for FiO2 and  Respiratory support information.
#' @param clinical_events A list of 4 string vectors representing the name of the clinical events corresponding to Respiratory, SPO2, PaO2, and FiO2.
#' @return An integer between 0 and 4
#' @examples
#'
Respiratory <- function(DT, V, E, day_of_interest = 0,maxlag=6.0, clinical_events = list(Respiratory=c("Respiratory"),SPO2=c("SPO2"),PaO2=c("PaO2"),FiO2=c("FiO2"))) {
  
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
    # Check that Age is a non-negative  number
  if (!is.numeric(maxlag) || maxlag < 0) {
    stop("maxlag must be a non-negative number")
  }

  # Check that day_of_interest is a non-negative whole number
  if (!is.numeric(day_of_interest) || day_of_interest %% 1 != 0 || day_of_interest < 0) {
    stop("day_of_interest must be a non-negative whole number. Starts from 0 for the first 24 hours of ICU stay")
  }
  # check that clinical_events are defined
  if (!identical(sort(names(clinical_events)), sort(c("Respiratory","SPO2","PaO2","FiO2")))) {
    stop("clinical_events must have Respiratory, SPO2, PaO2, and FiO2 components.")
  } 

   calculate_respiratory_score <- function()
  {
    tf <- max(et_FiO2[(et_FiO2<=ts)&(et_FiO2>=ts-(maxlag/24.0))])
    td <- max(et_respiratory[(et_respiratory<=ts)&(et_respiratory>=ts-(maxlag/24.0))])
    if (tf>=0)
    {
      f <- FiO2[et_FiO2==tf]
      if (length(f)>1) {
        if (length(unique(f)) == 1) f=f[1] else return(score <- 0)
      }
      if (f>1) f <- f/100.0 # Probably they entered the numbers incorrectly.
      #Now if even after dividing by 100 it is larger than 1, it is wrong. 
      if (f>1) return(score <- 0)
      if (f<0.21) return(score <- 0)
      r <- s/f
    }
    if (td>=0)
    {
      d <- respiratory[et_respiratory==td]
      if (length(d)>1){
        if (length(unique(d)) == 1) d=d[1] else return(score <- 0)
      }
    }
    
    if      ((tf<0)   & (td<0)) {d <- 0;f <- 0.21;r <- s/f} #1
    else if (td<0) {
      if (f>0.21) {return(score <- 0)} #2
      else if ((f==0.21) & (r<th_3)) {return(score <- 0)} #3
      else if ((f==0.21) & (r>=th_3)) {d <- 0;f <- 0.21;r <- s/f} #3
    }
    else if (tf <0)
    {
      if (d %in% c(1,4)) {return (score <- 0)} #4,7
      else if (d==2) {d <- 1 ;f <- 0.21;r <- s/f} #5
      else if (d==0) {d <- 0 ;f <-0.21;r <- s/f } #6
    }
    else if ((tf>td)  & (f==0.21) & (d==0)) {d <- 0 ;f <-0.21;r <- s/f }#8
    else if ((tf>td)  & (f==0.21) & (d==4) & (r<th_3)) {return(score <- 0)}#9
    else if ((tf>td)  & (f==0.21) & (d==4) & (r>=th_3)) {d <- 4 ;f <-0.21;r <- s/f }#9
    else if ((tf>td)  & (f==0.21) & (d==1)) {d <- 1 ;f <-0.21;r <- s/f }#10
    else if ((tf>td)  & (f==0.21) & (d==2)) {d <- 0 ;f <-0.21;r <- s/f  }#11
    else if ((tf==td) & (f==0.21) & (d==0)) {d <- 0 ;f <-0.21;r <- s/f }#12
    else if ((tf==td) & (f==0.21) & (d==4) & (r<th_3)) return(score <- 0)#13
    else if ((tf==td) & (f==0.21) & (d==4) & (r>=th_3)) {d <- 4 ;f <-0.21;r <- s/f }#13
    else if ((tf==td) & (f==0.21) & (d==1)) {d <- 1 ;f <-0.21;r <- s/f }#14
    else if ((tf==td) & (f==0.21) & (d==2)) {d <- 0 ;f <-0.21;r <- s/f }#15
    
    else if ((tf<td) & (f==0.21) & (d==0)) {d <- 0 ;f <-0.21;r <- s/f }#16
    else if ((tf<td) & (f==0.21) & (d==4) & (r<th_3)) return(score <- 0)#17
    else if ((tf<td) & (f==0.21) & (d==4) & (r>=th_3)) {d <- 4 ;f <-0.21;r <- s/f }#17
    else if ((tf<td) & (f==0.21) & (d==1)) {d <- 1 ;f <-0.21;r <- s/f }#18
    else if ((tf<td) & (f==0.21) & (d==2)) {d <- 0 ;f <-0.21;r <- s/f }#19
    
    else if ((tf>td)  & (f>0.21) & (d==0)) {d <- 0 ;f <-0.21;r <- s/f }#20
    else if ((tf>td)  & (f>0.21) & (d==4)) { d <- 0 ;f <-0.21;r <- s/f }#21
    else if ((tf>td)  & (f>0.21) & (d==1)) {d <- 1 ;f <- f;r <- s/f }#22
    else if ((tf>td)  & (f>0.21) & (d==2)) {d <- 0 ;f <-0.21;r <- s/f}#23
    
    else if ((tf==td)  & (f>0.21) & (d==0)) {d <- 0 ;f <- 0.21;r <- s/f }#24
    else if ((tf==td)  & (f>0.21) & (d==4)) {return(score <- 0) }#25
    else if ((tf==td)  & (f>0.21) & (d==1)) {d <- 1 ;f <- f;r <- s/f }#26
    else if ((tf==td)  & (f>0.21) & (d==2)) {return(score <- 0) }#27
    
    else if ((tf<td)  & (f>0.21) & (d==0)) {d <- 0 ;f <- 0.21;r <- s/f }#28
    else if ((tf<td)  & (f>0.21) & (d==4)) {return(score <- 0) }#29
    else if ((tf<td)  & (f>0.21) & (d==1)) {d <- 1 ;f <- f;r <- s/f }#30
    else if ((tf<td)  & (f>0.21) & (d==2)) {d <- 0 ;f <- 0.21;r <- s/f }#31
    
    if ((r<th_3)&(d>0))
    {
      if (d==1)
      {
        if(r<th_4) return(score <- 4)
        else return(score <- 3)
      }
      else {
        if(r<th_4) return(score <- 3.5)
        else return(score <- 2.5)
      }          
    }
    else
    {
      if (r<th_2) return(score <- 2)
      else if (r<th_1) return(score <- 1)
      else return(score <- 0)
    }
  }
  # First we select the SPO2 and PaO2 events in the day of interest.
  et_SPO2 <- DT[(E %in% clinical_events$SPO2) & (as.integer(DT)==day_of_interest)]
  SPO2t <- V[(E %in% clinical_events$SPO2) & (as.integer(DT)==day_of_interest)]
  SPO2 <- SPO2t[SPO2t<=97]
  et_SPO2 <- et_SPO2[SPO2t<=97]
  et_PaO2 <- DT[(E %in% clinical_events$PaO2) & (as.integer(DT)==day_of_interest)]
  PaO2 <- V[(E %in% clinical_events$PaO2) & (as.integer(DT)==day_of_interest)]
  et_FiO2 <- DT[E %in% clinical_events$FiO2]
  FiO2 <- V[E %in% clinical_events$FiO2]
  et_respiratory <- DT[E %in% clinical_events$Respiratory]
  respiratory <- V[E %in% clinical_events$Respiratory]
  
  score <- 0
  time_e <- -1
  th_1 <- 292
  th_2 <- 264
  th_3 <- 221
  th_4 <- 148
  
  for (i in seq_along(et_SPO2))
  {
    ts <- et_SPO2[i]
    s <- SPO2[i]
    score_t <- calculate_respiratory_score()
    if(score_t>score) time_e <- ts
    score <- max(score_t,score)
    
  }
  th_1 <- 400
  th_2 <- 300
  th_3 <- 200
  th_4 <- 100
  for (i in seq_along(et_PaO2))
  {
    ts <- et_PaO2[i]
    s <- PaO2[i]
    score_t <- calculate_respiratory_score()
    if(score_t>score) time_e <- ts
    score <- max(score_t,score)
    
  }
  return(score)
 
  

}
