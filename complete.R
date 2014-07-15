##PART 2##
#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {

		nobs = numeric()

		for(i in id) {
			data_file = read.csv(paste(directory,"/",sprintf("%03d.csv", i), sep=""))
			
			nobs = c(nobs, sum(complete.cases(data_file)))
		}

		data.frame(id, nobs)		
}
