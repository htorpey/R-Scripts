rankhospital <- function(stateName, outcome, num = 'best') {
        
        # Read outcome data into a dataframe
        outcomeData <- read.csv("outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
        
        ## First check that the state variable is valid
        if (stateName %in% outcomeData[, 7] == FALSE) {stop("invalid state")}
        
        ## Next check that the outcome variable is valid
        if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
        
        ## Return hospital name in that state with the given rank
        ## Pare down the table to just the rows corresponding to the 
        ## stateName variable and the columns containing the 
        ## Hospital Name, State, Heart Attack Rates, Heart Failure 
        ## Rates, and Pneumonia Rates
        subsetOutcomes <- outcomeData[ which(outcomeData$State == stateName), c(2, 7, 11, 17, 23)]
        
        ## Assign the column number corresponding to the outcome variable
        ## to the colNum variable representing the column position in the 
        ## subsetOutcomes dataframe.
        if (outcome == "heart attack") {
                colNum <- 3
                colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
                colNum <- 4
                colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else {
                ## outcome must be pneumonia
                colNum <- 5
                colName <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
        
        ## Convert columns from factors to character
        apply(subsetOutcomes, 2, as.character)
        
        ## Convert outcome columns from character to numeric
        subsetOutcomes[, 3] <- as.numeric(subsetOutcomes[, 3])
        subsetOutcomes[, 4] <- as.numeric(subsetOutcomes[, 4])
        subsetOutcomes[, 5] <- as.numeric(subsetOutcomes[, 5])
        
        ## Order in ascending or descending order by outcome (if the
        ## num variable is worst sort descending, in all other cases
        ## sort ascending)
        if (num == "worst") {
                subsetOutcomes <- subsetOutcomes[order(-subsetOutcomes[ ,colNum], subsetOutcomes$Hospital.Name), ]
                num <- 1
        } else {
                subsetOutcomes <- subsetOutcomes[order(subsetOutcomes[ , colNum], subsetOutcomes$Hospital.Name), ]
        }
        
        if (num == "best") {num <- 1}
        subsetOutcomes[num, 1]
}
