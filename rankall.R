rankall <- function(outcome,num = "best") {
  ## read outcome data
  outcomeData <- read.csv("./rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character",na.strings="No value")
  
  ## check the state and outcome are valid
  validStates = sort(unique(outcomeData[,7]))
  validOutcomes = c("heart attack","heart failure","pneumonia")
  if(!tolower(outcome) %in% validOutcomes) {
    stop("invalid outcome")
  }
  
  ## 30-day death column names
  fullOutcomes <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  ## match the actuall 30-day death column with the valid outcome
  outcomeName <- fullOutcomes[match(outcome,validOutcomes)]
  
  hospital <- character(0)
  
  ## loop for all valid states
  for (i in seq_along(validStates)) {
    ## get all the rows for the current state
    outcomeState <- outcomeData[outcomeData$State==validStates[i],]
    ## sorts all the rows by by outcomeName
    sortedOutcomeState <- outcomeState[order(as.numeric(outcomeState[[outcomeName]]),outcomeState[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    ## The num variable can take values “best”, “worst”, or an integer indicating 
    ## the ranking (smaller numbers are better).
    this.num = num
    if (this.num=="best") {
      this.num = 1 
    }
    if (this.num=='worst') {
      this.num = nrow(sortedOutcomeState)
    } 
    
    ## gets ranking by hospital 
    hospital[i] <- sortedOutcomeState[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital=hospital,state=validStates,row.names=validStates)
}