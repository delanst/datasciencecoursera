## calculate the means of a given pollutant from a a given set of monitor ids. 
## each monitor id corresponds to a csv file.
## all selected files are read in a list that is converted to a data frame
## as last we calculate the mean for the given pollutant with NA values removed.
pollutantmean <- function(directory,pollutant, id = 1:332) {
  fileList <- list.files(directory)
  fileNames <- as.numeric(sub("\\.csv$","",fileList))
  fileSelected <- fileList[match(id,fileNames)] 
  dataList <- lapply(file.path(directory,fileSelected), read.csv)
  data <- do.call(rbind.data.frame,dataList)
  mean(data[,pollutant],na.rm=TRUE)
}
