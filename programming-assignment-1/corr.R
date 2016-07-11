corr <- function(directory, threshold = 0) {
	
	file_list <- list.files(path=directory, pattern=".csv")
    correlation <- vector()

    for(file_item in file_list) {
    	temp = read.csv(paste(directory, file_item, sep="/"), header = TRUE)
    	good <- complete.cases(temp)
    	data <- temp[good, ]
        count <- nrow(data)
        
        if (count > threshold) {
        	correlation <- c(correlation, cor(data[, "sulfate"], data[, "nitrate"]))
        }
    }

    correlation
	
}