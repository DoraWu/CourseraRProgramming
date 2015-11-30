rankall <- function(outcome, num = "best") {
    
    # return the name of the hospital based on its rank
    getHospital<-function(x, num = "best"){
        if (num=="best") hospital<-x[1]
        else if (num=="worst") hospital<-x[length(x)]
        else if (num>length(x)) hospital<-NA
        else hospital<-x[num]
        hospital
    }
  
    infile<- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  
    # sort hospitals based on the outcome and hospital name, remove NA rows
    if (outcome=="heart attack") {
        infile<-infile[order(as.numeric(infile[,11]),infile[,2], na.last=NA),]
    }
    else if (outcome=="heart failure"){
        infile<-infile[order(as.numeric(infile[,17]),infile[,2], na.last=NA),]
    }
    else if (outcome=="pneumonia"){
        infile<-infile[order(as.numeric(infile[,23]),infile[,2], na.last=NA),]
    }
    else stop("invalid outcome")
  
    # split hospitals by their state, and apply getHospital function
    output<-tapply(infile$Hospital.Name, infile$State, getHospital,num)
    
    # organize output dataset
    df<-data.frame(output, names(output))
    colnames(df)=c("hospital","state")
    df
  
}