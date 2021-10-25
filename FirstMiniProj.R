#pollutantmean
pollutantmean <- function(directory, pollutant, id=1:322)
{
  #loading all files into a list of files called "FileNames"
  fileNames <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  #initialized to store collections of data
  collection <- numeric()
  
  for (i in id) 
  {
    #read per line of file
    data <- read.csv(fileNames[i], header=TRUE)
    
    #collection of all data of a certain pollutant for id ranges
    collection <- c(collection, data[[pollutant]])
  }
  
  #mean of the filtered data with NA removed.
  mean(collection, na.rm=TRUE)
}

#complete
complete <- function(directory, id =1:332)
{
  #loading all files into a list of files called "FileNames"
  fileNames <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  observations = c()
  ids= c()
  
  for (i in id) 
  {
    data = read.csv(fileNames[i])
    
    #stores id given
    ids = c(ids, i)
    
    #stores observation per id given that aren't NA in any fields
    observations = c(observations, nrow(data[complete.cases(data),]) )
  }
  #prints the complete data per id with header
  data.frame(id=ids, nobs=observations)
  
}


#correlation 
corr <- function(directory, threshold =0)
{
  #loading all files into a list of files called "FileNames"
  fileNames <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  correlation = numeric()
  
  for (i in 1:322) {
    #data of each csv
    data = read.csv(fileNames[i])
    
    #if threshold is reached
    if (sum(complete.cases(data))>threshold) 
    {
      correlation <- c(correlation, cor(data[["sulfate"]], data[["nitrate"]],use = "complete.obs"))
    }
    
  }
  
  correlation
}



#run the function
pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata","nitrate",70:72)
pollutantmean("specdata","nitrate",23)

complete("specdata",c(2,4,8,10,12))

