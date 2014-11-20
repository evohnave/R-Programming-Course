complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## As before, we assume the directory is a sub-directory
  ##  of the working directory, getwd()
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Returns a data frame of the form:
  
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  
  ## Where 'id' is the monitor ID number and 'nobs' is the
  ##   number of complete cases
  
  ## 'Complete cases' are those where both observations are not
  ##   NA
  
  ## Initialize list for storing observations
  ##   Don't need one for id... id works!
  nobs <- NULL
  
  ## Loop through files, add data to Results
  for(i in id) {
    
    ## Create Filename
    fn=sprintf(paste("./",directory,"/%03d.csv", sep=""), i)
    
    ## Get number of observations, nobs
    nobs <- c(nobs, sum(complete.cases(read.csv(fn))))
    
  }
  data.frame(id, nobs)
}
