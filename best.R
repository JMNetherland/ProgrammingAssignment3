best <- function(state, outcome) {
        ## Read outcome data setting "Not Available" to NA
        data <- read.csv("outcome-of-care-measures.csv"
                            , na.strings="Not Available" 
                            , stringsAsFactors=FALSE )
        
        ##Assign new column names
        colnames(data)[11] <- "heart attack"
        colnames(data)[17] <- "heart failure"
        colnames(data)[23] <- "pneumonia"
                
        ## Check that state is valid
        if(state %in% data[, 7]) {
                
                ## Check that outcome is valid
                if(outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                        ## Create a data.frame of only Name, State, and outcome
                        ## where the state equals state provided
                        sub1 <- data[which(data$State==state), c("Hospital.Name", "State", outcome)]
                        ## Find the record with the minimum outcome
                        sub2 <- sub1[which(sub1[[outcome]] == min(sub1[,outcome], na.rm = TRUE)),]
                        ## Order list of hospitals incase there is a tie
                        sub3 <- sub2[order(sub2$Hospital.Name),]
                        ## Return hospital name in that state with lowest 30-day death
                        ## rate
                        sub3[1,]$Hospital.Name
                }
                else {
                        ## Print invalid outcome message
                        stop("invalid outcome")
                }
                
        }
        ## Print invalid state message
        else {
                stop("invalid state")
        }
}