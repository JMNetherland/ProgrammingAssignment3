rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        

        
        ## Read outcome data setting "Not Available" to NA
        data <- read.csv("outcome-of-care-measures.csv"
                         , na.strings="Not Available" 
                         , stringsAsFactors=FALSE )
        
        ##Assign new column names
        colnames(data)[11] <- "heart attack"
        colnames(data)[17] <- "heart failure"
        colnames(data)[23] <- "pneumonia"
        
        ## Check that outcome is valid
        if(outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                ## Do Something
                
                st <- unique(data$State)
                
                df1 <- data.frame(stringsAsFactors = FALSE)
                
                for(i in 1:length(st)) {

                        
                        sub1 <- data[which(data$State==st[i]), c("Hospital.Name", "State", outcome)]
                        
                        ## Return hospital name in that state with the given rank
                        ## 30-day death rate
                        
                        ## Order by outcome then Hospital.Name and drop NA
                        sub2 <- sub1[order(sub1[[outcome]], sub1$Hospital.Name, na.last = NA),]
                        
                        ## Find the number of rows in the subset
                        worstnum <- nrow(sub2)
                        
                        ## Display hosital name based on the num input
                        if(num == "best") df1 <- rbind(df1,data.frame(hospital=sub2[1,]$Hospital.Name, state=st[i], stringsAsFactors = FALSE))
                        else if(num == "worst") df1 <- rbind(df1,data.frame(hospital=sub2[worstnum,]$Hospital.Name, state=st[i], stringsAsFactors = FALSE))
                        else df1 <- rbind(df1,data.frame(hospital=sub2[num,]$Hospital.Name, state=st[i], stringsAsFactors = FALSE))
                }
                
                df1[order(df1$state),]
                
        }
        else {
                ## Print invalid outcome message
                stop("invalid outcome")
        }
        
}