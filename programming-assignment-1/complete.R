complete <- function(directory, id = 1:332) {
	file_list <- list.files(path=directory, pattern=".csv")
    
    data <- data.frame()

    for(i in id) {
    	temp = read.csv(paste(directory, file_list[i], sep="/"), header = TRUE)
    	good <- complete.cases(temp)	
        data <- rbind(data, c(i, nrow(temp[good,][,])))
    }

    colnames(data) <- c("id", "nobs")
    data
	
}