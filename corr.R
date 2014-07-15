##PART 3##
#Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.
corr <- function(directory, threshold = 0) {

		correlation <- vector()

		complete_obs <- complete(directory)
		
		for(i in subset(complete_obs, nobs > threshold)$id) {

			data_file <- read.csv(paste(directory,"/",sprintf("%03d.csv", i), sep=""))
			
			complete <- data_file[complete.cases(data_file),]
			
			if(nrow(complete) > threshold) {
				correlation <- c(correlation, cor(complete$sulfate, complete$nitrate))
			}
		}

		correlation	
}
