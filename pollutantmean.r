pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## For this exercise the location is assumed to be in a 
  ##  sub-directory of the working directory, getwd()
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  ## Create Filenames
  fn=sprintf(paste("./",directory,"/%03d.csv", sep=""), id)
  
  ## Read the csv files using lapply
  MyTable = lapply(fn,read.csv)

  ## Convert MyTable to a data frame
  MyTable = do.call(rbind.data.frame, MyTable)
  
  ## Calculate the mean, removing NA's
  mean(MyTable[ , pollutant], na.rm=TRUE)
}
