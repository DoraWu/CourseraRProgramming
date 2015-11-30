setwd("C:\\Scripts\\R")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
ncol(outcome)
hist(outcome[, 11])

best <- function(state, outcome) {
  
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

  sortedhos$Hospital.Name[1]
  
}