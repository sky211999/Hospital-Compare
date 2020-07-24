best <- function(state, outcome) {
  ## Read outcome data
  object <- read.csv("outcome-of-care-measures.csv")
  Var<-"invalid"
  val<-match(state,object$State)
  
  ## Check that state and outcome are valid
  if (!is.na(val) && (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
      var<- "valid"
  }
  
  ## Return hospital name in that state with lowest 30-day death
  if (var == "invalid") {
      print("Invalid Values Passed")
  }
  else if (var == "valid") {
    obj<-unique(unlist(strsplit(object$State, " ")))
    val2<-match(state,obj)+1
    if (val2 == 55) {
      val3<-nrow(object)
      }
    else if (val2<=54) {
      val3<-match(obj[val2],object$State)
      val3<-val3-1
      }
    if (outcome == "heart attack"){
      object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      loc<-which(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[val:val3] == min(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[val:val3], na.rm =TRUE))      
      min(object$Hospital.Name[loc+val-1])
      }
    else if (outcome == "heart failure") {
      object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      loc<-which(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[val:val3] == min(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[val:val3], na.rm =TRUE))      
      min(object$Hospital.Name[loc+val-1])
      }
    else if (outcome == "pneumonia") {
      object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      loc<-which(object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[val:val3] == min(object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[val:val3], na.rm =TRUE))      
      min(object$Hospital.Name[loc+val-1])
      }
    }
    
  }
 
  ## rate