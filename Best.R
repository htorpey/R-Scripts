# Best.R
best <- function(stateName, outcome) {
        
        ## Read outcome data into a dataframe
        outcomeData <- read.csv("outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE)
        
        ## First check that the state variable is valid
        if (stateName %in% outcomeData[, 7] == FALSE) {stop("invalid state")}
        
        ## Next check that the outcome variable is valid
        if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {stop("invalid outcome")}
        
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
        } else if (outcome == "heart failure") {
                colNum <- 4
        } else {
                ## outcome must be pneumonia
                colNum <- 5}
        
        ## Convert columns from factors to character
        apply(subsetOutcomes, 2, as.character)
        
        ## Convert outcome columns from character to numeric
        subsetOutcomes[, 3] <- as.numeric(subsetOutcomes[, 3])
        subsetOutcomes[, 4] <- as.numeric(subsetOutcomes[, 4])
        subsetOutcomes[, 5] <- as.numeric(subsetOutcomes[, 5])
        
        ## Return hospital name in the state with lowest 30-day death rate
        ##resultSet <- subset(subsetOutcomes, subsetOutcomes[stateName, colNum] == min(subsetOutcomes[stateName, colNum]))
        
        ## Find the minimum value 
        minOutcomeValue <- min(subsetOutcomes[, colNum], na.rm = TRUE)
        message(minOutcomeValue)
        message(names(subsetOutcomes))
        
        ## subset only those records containing the minimum value for the
        ## appropriate Outcome column
        resultSet <- subsetOutcomes[which(subsetOutcomes[, colNum] == minOutcomeValue), ]
        
        ## Sort resultSet alphabetically by hospital name 
        resultSet <- resultSet[order(resultSet$Hospital.Name), ]
        
        ## Return the first hospital name in the list
        resultSet[1, 1]
}
