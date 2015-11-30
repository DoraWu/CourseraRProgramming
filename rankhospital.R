rankhospital <- function(state, outcome, num = "best") {
  
  infile<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospitals <- subset(infile, infile$State==state)
  if (nrow(hospitals)==0) stop("invalid state")
  else {
    if (outcome=="heart attack"){
      sortedhos<-subset(hospitals, hospitals[,11]!="Not Available")
      sortedhos<-sortedhos[order(as.numeric(sortedhos[,11]), sortedhos[,2]),]
    }
    else if (outcome=="heart failure") {
      sortedhos<-subset(hospitals, hospitals[,17]!="Not Available")
      sortedhos<-sortedhos[order(as.numeric(sortedhos[,17]), sortedhos[,2]),]
    }
    else if (outcome=="pneumonia") {
      sortedhos<-subset(hospitals, hospitals[,23]!="Not Available")
      sortedhos<-sortedhos[order(as.numeric(sortedhos[,23]), sortedhos[,2]),]
    }
    else stop("invalid outcome")
  }
  if (num=="best") a<-sortedhos$Hospital.Name[1]
  else if (num=="worst") a<-sortedhos$Hospital.Name[length(sortedhos$Hospital.Name)]
  else a<-sortedhos$Hospital.Name[num]
  
  a
}