##PART 1##
#Function that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. Given a vector monitor ID numbers, it reads that monitors' particulate matter data from the directory specified and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(directory, pollutant, id = 1:332) {

		data_pollute = numeric()

		for(i in id) {
			data_file = read.csv(paste(directory,"/",sprintf("%03d.csv", i), sep=""))
			
			data_pollute = c(data_pollute, data_file[[pollutant]])
		}

		round(mean(data_pollute,na.rm=TRUE), 3)
}
