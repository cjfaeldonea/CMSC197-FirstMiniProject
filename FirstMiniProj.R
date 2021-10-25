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
  #loading all files into a list of files called "FileNames" with formulat specdata\xxx.csv
  fileNames <- list.files(path = directory, pattern = ".csv", full.name = TRUE)
  
  #variable for completes 
  df <- complete(directory)
  
  #calculate and store the IDs of nobs that are over the threshold
  ids <- df[df["nobs"] > threshold, ]$id
  
  #initialized empty correlation as numeric
  corrr <- numeric()
  
  
  #for each CSV files in ID
  for (i in ids){
    #read the csv
    mydata <- read.csv(fileNames[i])
    
    #calculate and store the number of complete cases 
    dff <- mydata[complete.cases(mydata),]
    
    #calculate the complete cases that reached the threshold
    corrr <- c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}


#Histogram
# Ploting the 30-day mortality rates for heart attack

# Reading in data from CSV and specify that we need to first read the data as character
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

#we convert the data to numeric
outcome[, 11] <- as.numeric(outcome[, 11])

#this outputs the historgram with a title, x-label, and with color.
hist(outcome[, 11],
     xlab = paste("Deaths"),
     main=paste("Hospital 30-Day Death (Mortality) Rates from Heart Attack"),
     col="lightblue")







#example test cases




#run the function
pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata","nitrate",70:72)
pollutantmean("specdata","nitrate",23)

complete("specdata",1)
complete("specdata",c(2,4,8,10,12))
complete("specdata",30:25)
complete("specdata",3)

cr <- corr("specdata", 150)
head(cr); summary(cr)

cr <- corr("specdata", 400)
head(cr); summary(cr)

cr <- corr("specdata", 5000)
head(cr); summary(cr)