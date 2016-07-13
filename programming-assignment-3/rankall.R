rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")	
	states_factor <- factor(data$State)
	states_levels <- levels(states_factor)

	
	## Check that state and outcome are valid
	outcome_vector <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcome_col <- outcome_vector[outcome]
	if (is.na(outcome_col)) {
	    stop("invalid outcome")
	} 
	
	## For each state, find the hospital of the given rank
	#rank_all <- data.frame("hospital" = character(), "state" = character(), stringsAsFactors = FALSE)
	#rank_all <- data.frame()
	sub_data <- subset(data, select = c(2, 7, outcome_col))
	colnames(sub_data) <- c("hospital", "state", "rate")
	sub_data[, 3] <- as.numeric(sub_data[, 3])	
	
	sub_data <- split(sub_data, sub_data$state)
	
	for (i in 1:length(sub_data)) {
        sub <- sub_data[i]
	    sub[[1]] <- sub[[1]][order(sub[[1]][,3], sub[[1]][,1], na.last = NA), ]
	    sub_data[i][[1]] <- sub[[1]]
	}
	
	hosp = character()
	st = character()
		
	for(i in 1:length(sub_data)) {
		sub <- sub_data[i]
		st = c(st, sub[[1]][1, 2])
		
		if(num == "best") {
			hosp <- c(hosp, sub[[1]][1, 1])
		} else if(num == "worst") {
			hosp <- c(hosp, tail(sub[[1]], 1)[1, 1])	
		} else {
			hosp <- c(hosp, sub[[1]][num, 1])
		}
	}
	
	
	## Return a data frame with the hospital names and the (abbreviated) state name
	rank_all = data.frame("hospital" = hosp, "state" = st)
	rank_all
}