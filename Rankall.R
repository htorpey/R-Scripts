rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Next check that the outcome variable is valid
        if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
        
        ## Pare down the table to just the rows corresponding to the 
        ## stateName variable and the columns containing the 
        ## Hospital Name, State, Heart Attack Rates, Heart Failure 
        ## Rates, and Pneumonia Rates
        subsetOutcomes <- outcomeData[ , c(2, 7, 11, 17, 23)]
        
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
        
        ## Convert outcome columns from character to numeric
        subsetOutcomes[, 3] <- as.numeric(subsetOutcomes[, 3])
        subsetOutcomes[, 4] <- as.numeric(subsetOutcomes[, 4])
        subsetOutcomes[, 5] <- as.numeric(subsetOutcomes[, 5])
        
        ## Order ascending or descending by outcome (if the num 
        ## variable is "worst," sort descending, in all other cases
        ## sort ascending)
        if (num == "worst") {
                subsetOutcomes <- subsetOutcomes[order(subsetOutcomes$State, -subsetOutcomes[ ,colNum], subsetOutcomes$Hospital.Name), ]
                num <- 1
        } else {
                subsetOutcomes <- subsetOutcomes[order(subsetOutcomes$State, subsetOutcomes[ , colNum], subsetOutcomes$Hospital.Name), ]
        }
        
        if (num == "best") {num <- 1}
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        ## Get a vector of state abbreviations
        statesList <- unique(subsetOutcomes[, 2])
        
        ## Initialize a data frame with two columns and states as row names
        resultsDF <- data.frame(matrix(ncol = 2, nrow = length(statesList)))
        colnames(resultsDF) <- c("hospital", "state")
        row.names(resultsDF) = statesList
        
        s <- 1
        for (s in 1:length(statesList)) {
          stateResult <- subset(subsetOutcomes[ which( subsetOutcomes[, 2] == statesList[s]), c(1:2)])
          resultRowDF <- stateResult[num, 1]
          names(resultRowDF) <- NULL
          resultRowVector <- unlist(c(resultRowDF))
          resultRowVector <- c(resultRowVector, statesList[s])
          resultsDF[s, ] <- resultRowVector 
          s <- s + 1
          ##resultsVector <- as.numeric(resultsDF)
          ##print(resultRow)
          ##resultsDF <- rbind(resultsDF, c(statesList[s], stateResult[]))
          
        }
        
        resultsDF
        ##For every s in c(1:length(StatesList) {
          ##Subset(outcomeSubset[ (which (outcomeSubser[2, ] == statesList[s]), c(2, 1)]
            ##     s <- s + 1
                 ## figure out how to append this to a results data frame 
        ##}
       
}
