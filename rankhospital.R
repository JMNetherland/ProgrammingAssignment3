rankhospital <- function(state, outcome, num = "best") {
        
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
                        
                        ## Return hospital name in that state with the given rank
                        ## 30-day death rate
                        
                        ## Order by outcome then Hospital.Name and drop NA
                        sub2 <- sub1[order(sub1[[outcome]], sub1$Hospital.Name, na.last = NA),]
                        
                        ## Find the number of rows in the subset
                        worstnum <- nrow(sub2)

                        ## Display hosital name based on the num input
                        if(num == "best") sub2[1,]$Hospital.Name
                        else if(num == "worst") sub2[worstnum,]$Hospital.Name
                        else sub2[num,]$Hospital.Name
                                                        
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