rankall <- function(outcome, num = "best") {
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  states<-unique(read.csv("outcome-of-care-measures.csv", colClasses = "character")[ , 7])
  ## Order states
  states<-states[order(states)]
  
  ## Check that state and outcome are valid
  ## Note that the template was just copied... no state passed as an argument
  
  ## Check outcome
  ## Outcomes can be "heart attack", "heart failure", or "pneumonia"
  colData<-c(17,11,23)[outcome==c('heart failure','heart attack','pneumonia')]
  
  if(length(colData)==0) {
    ## Outcome invalid
    stop("invalid outcome")
  }
 
  ## Strip data down to the columns you care about
  ##   Hospital, State, Outcome
  data<-data[ , c(2, 7, colData)]
  ## Remove the 'Not Available's
  data<-data[data[ , 3] != 'Not Available', ]

  ## Initialize hospitals
  hospital<-states
  
  for(i in 1:length(states)) {
    state<-states[i]
    ## Look at state data only
    good<-data[ , 2]==state
    temp<-data[good,c(1,3) ]
    ## Sort by outcome then hospital
    o<-order(temp[ , 2])
    temp<-temp[o, ]
    
    ## Now look at num
    if(num=='best') {nu<-1
      } else if(num=='worst') {nu<-length(o)
      } else {nu<-num}
    if(nu>length(o)) {ret<-NA
      } else {ret<-temp[nu, 1]}
    hospital[i]<-ret
  }
  state<-states
  ret<-(cbind(hospital,state))
  as.data.frame(ret[complete.cases(ret), ])
}
