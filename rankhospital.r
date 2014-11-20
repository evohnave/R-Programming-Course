rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states<-factor(data[ , 7])
  if(sum(state==states)<1) {
    ## State invalid
    stop("invalid state")
  }
  
  ## Outcomes can be "heart attack", "heart failure", or "pneumonia"
  colData=c(17,11,23)[outcome==c('heart failure','heart attack','pneumonia')]
  
  if(length(colData)==0) {
    ## Outcome invalid
    stop("invalid outcome")
  }
  
  ## Select data by state and outcome we want
  good<-data[ ,7]==state
  data<-data[good,c(2,colData)]
  ## Remove the 'Not Available's
  data<-data[data[ , 2] != 'Not Available', ]
  
  ## Sort by outcome
  o<-order(data[ , 2], data[ , 1])
  data<-data[o,]
  
  if(num=='best') num<-1
  if(num=='worst') num<-length(o)
  if(num>length(o)) {
    ret<-NA}
  else{
    ret<-data[num, 1]}
  ret
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
