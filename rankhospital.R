rankhospital <- function(state,outcome,num = "best") {
  ## read outcome data
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
  
  ## get the 30-day rows by given state
  outcomeState <- outcomeData[outcomeData$State==state,]
  ## get outcome rows ordered 
  orderedOutcomeState <- outcomeState[order(as.numeric(outcomeState[[outcomeName]]),outcomeState[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  if(num == "best") {
    num = 1
  }
  if (num=='worst') {
    num = nrow(orderedOutcomeState)
  }
  
  orderedOutcomeState[num,"Hospital.Name"]
}

# rankhospital("TX", "heart failure", 4) ## [1] "DETAR HOSPITAL NAVARRO"
# rankhospital("MD", "heart attack", "worst") ## [1] "HARFORD MEMORIAL HOSPITAL"
# rankhospital("MN", "heart attack", 5000) ## [1] NA