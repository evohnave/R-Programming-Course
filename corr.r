corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ##   the location of the CSV files
  
  ##  Again, we assume the directory is a sub-directory of the
  ##    working directory, getwd()
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ##   number of completely observed observations (on all
  ##   variables) required to compute the correlation between
  ##   nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Determine the number of files in directory
  l <- list.files(directory)
  ids <- (1:length(l))
  
  ## Check complete on the directory to get the list of nobs
  res <- complete(directory, ids)
  
  ## Determine files to check
  ftc <- l[res$nobs > threshold]

  ## initialize return
  ret <- NULL

  ## Get the correlation of sulfate to nitrate for ftc
  for(i in ftc) {
    fn <- paste("./",directory,"/", i, sep="")
    cc <- na.omit(read.csv(fn))
    sulfate <- cc[['sulfate']]
    nitrate <- cc[['nitrate']]
    temp <- cor(sulfate, nitrate)
    ret <- c(ret, temp)
  }
  
  ret
}
