rankhospital <- function(state, outcome, num = "best") {
  object <- read.csv("outcome-of-care-measures.csv")
  Var<-"invalid"
  val<-match(state,object$State)
  
  ## Check that state and outcome are valid
  if (!is.na(val) && (outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
    var<- "valid"
  }
  
  ## Objective Processing
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
    object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    data<-data.frame(object$Hospital.Name[val:val3], object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[val:val3], object$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[val:val3], object$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[val:val3])
    data<-data[order(data[,1]),]
    if (outcome == "heart attack"){
      good<-complete.cases(data$object.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      data<-data[good,]
      data<-arrange(data, data$object.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    }
    else if (outcome == "heart failure") {
      good<-complete.cases(data$object.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      data<-data[good,]
      data<-arrange(data, data$object.Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    }
    else if (outcome == "pneumonia") {
      good<-complete.cases(data$object.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      data<-data[good,]
      data<-arrange(data, data$object.Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    }
    if (num == "best") {
      data$object.Hospital.Name[1]
    }
    else if (num == "worst") {
      data$object.Hospital.Name[nrow(data)]
    }
    else if (is.numeric(num)) {
      data$object.Hospital.Name[num]
    }
  }
}