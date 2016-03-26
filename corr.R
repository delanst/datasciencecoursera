corr <- function(directory, threshold = 0) {
  
  sulNitrCorr <- function(fname) {
    data <- read.csv(file.path(directory, fname))
    sumCompleteCases <- sum(complete.cases(data))
    if (sumCompleteCases > threshold) {
      return (cor(data$nitrate, data$sulfate, use="complete.obs"))
    }
  }
  correlationWithNull <- sapply(list.files(directory), sulNitrCorr) 
  correlationNoNull <- unlist(correlationWithNull[!sapply(correlationWithNull, is.null)])
  return (correlationNoNull)
}