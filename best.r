best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ## Outcomes can be "heart attack", "heart failure", or "pneumonia"
  colData=c(17,11,23)[outcome==c('heart failure','heart attack','pneumonia')]
 
  if(length(colData)==0) {
    ## Outcome invalid
    stop("invalid outcome")
  }
  
  states<-factor(data[ , 7])
  if(sum(state==states)<1) {
    ## State invalid
    stop("invalid state")
  }
  
  ## Select data by state we want and 30 day mortality
  good<-data[ ,7]==state
  data<-data[good,c(2,colData)]
  ## Remove the 'Not Available's
  data<-data[data[ , 2] != 'Not Available', ]
  
  ## Find min
  rowNum<-which.min(data[ , 2])
  data[rowNum,1]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
}
