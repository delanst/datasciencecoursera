best <- function(state,outcome) {
  ## Read outcome data
  outcomeData <- read.csv("./rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character",na.strings="No value")
  
  ## check the state and outcome are valid
  validStates = unique(outcomeData[,7])
  if(!state %in% validStates) {
    stop("invalid state")
  }
  validOutcomes = c("heart attack","heart failure","pneumonia")
  if(!tolower(outcome) %in% validOutcomes) {
    stop("invalid outcome")
  }
  
  ## 30-day death column names
  fullOutcomes <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  ## match the actuall 30-day death column with the valid outcome
  outcomeName <- fullOutcomes[match(outcome,validOutcomes)]
  
  # get the 30-day rows by given state
  outcomeState <- outcomeData[outcomeData$State==state,]
  print(outcomeState)
  ## caculate the lowest 30-day death rate on the outcomeState
  lowestByOutcome <- which.min(as.double(outcomeState[,outcomeName]))
  
  ## Return hospital name with the lowest 30-day death rate
  outcomeState[lowestByOutcome,"Hospital.Name"]

}

# best("TX", "heart attack") ## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# best("TX", "heart failure") ## [1] "FORT DUNCAN MEDICAL CENTER"
# best("MD", "heart attack") ## [1] "JOHNS HOPKINS HOSPITAL, THE"
# best("MD", "pneumonia") ## [1] "GREATER BALTIMORE MEDICAL CENTER"
# best("BB", "heart attack") ## Error in best("BB", "heart attack") : invalid state
# best("NY", "hert attack") ## Error in best("NY", "hert attack") : invalid outcome
  