best <- function(istate, ioutcome) {
        ##Read outcome data
        
        ##subdata <- read.csv("outcome-of-care-measures.csv",colClasses="character")
        data <- read.csv("outcome-of-care-measures.csv",colClasses="character", na.strings = c("Not Available"))
        
        ##Take the columns relevant to output
        subdata <- data[, c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Readmission.Rates.from.Pneumonia")]
        
        ##Omit NA rows
        subdataWithoutNA <- na.omit(subdata)
        
        
        colnames(subdataWithoutNA)[colnames(subdataWithoutNA)=="Hospital.Name"] <- "HospitalName"
        colnames(subdataWithoutNA)[colnames(subdataWithoutNA)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "heart attack"
        colnames(subdataWithoutNA)[colnames(subdataWithoutNA)=="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "heart failure"
        colnames(subdataWithoutNA)[colnames(subdataWithoutNA)=="Hospital.30.Day.Readmission.Rates.from.Pneumonia"] <- "pneumonia"
        
        subdataWithoutNA[, "heart attack"] <- as.numeric(subdataWithoutNA[, "heart attack"])
        subdataWithoutNA[, "heart failure"] <- as.numeric(subdataWithoutNA[, "heart failure"])
        subdataWithoutNA[, "pneumonia"] <- as.numeric(subdataWithoutNA[, "pneumonia"])
        
        
        ##Check that parameters passed in are valid:
        
        states <- unique(subdataWithoutNA$State)
        if(!any(states == istate)) stop("invalid state passed in parameter")
        
        outcomes <- c("heart attack","heart failure","pneumonia")
        if(!any(outcomes == ioutcome)) stop("invalid outcome passed in parameter")
        
        ##Return hospital name in that state with lowest 30 da death rate
        
        filterState <- subset(subdataWithoutNA, State==istate)
       
 
        filterStateOrdered <- filterState[order(filterState$HospitalName),]
        minOutcome <- min(filterStateOrdered[, ioutcome]) ##needs work
        final <- subset(filterStateOrdered, filterStateOrdered[, ioutcome] == minOutcome)
        hospital <- final$HospitalName
        
        hospital

}