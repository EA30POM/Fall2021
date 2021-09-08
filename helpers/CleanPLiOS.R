###===================================================
### R script to clean up messy iOS PL Air Data
###===================================================

library(dplyr)

### Now we are going to start the clean up process
### Note that when R pulled in the data, it came
### as this rather unpleasant "list" structure.
my_unlisting_function <- function(j) { # function to get R's unlisting to work
  sapply(j, function(x) ifelse(is.null(x),NA,x)) # if there is "no data" in a row, replace it with NA
}

cleanup_air_function <- function(airDF) {
  # Now we are going to use several cool "Tidyverse" features
  # to automagically convert all of the columns from list to vectors
  airDF <- airDF %>% 
    mutate( across( where(is.list), my_unlisting_function)) # we're applying this function (my_unlisting function) to each column that is a list (which is each column)
  
  ### Storing the row locations that have added-on data
  indices <- which(airDF$Lat=="Lat") # we're using the fact that every time PocketLab
  # adds on another variable from the sensor, it tacks on "Lat" at the start of the row.
  
  ### We are going to instantiate a clean data table and build from there
  airCleanDF <- airDF[1:(indices[1]-2),c(1:8)]
  
  ### Now we are going to create a set of individual data tables
  ### to store our data more cleanly.
  ### The for loop below is going to "walk" through each value of the indices object
  ### and it is going to use the row values in indices to extract the sensor data we
  ### want and tack it on as added columns to our clean airCleanDF data table.
  var_name <- c()
  for (i in c(1:length(indices))) {
    #print(i)
    index_start <- indices[i]+1
    if (i==1) {
      index_end <- indices[(i+1)]-2
      var_name <- append(var_name, c(airDF[indices[i],5],airDF[indices[i],6]), after=length(var_name))
      airCleanDF[,9] <- airDF[index_start:index_end,5]
      airCleanDF[,10] <- airDF[index_start:index_end,6]
    } else if (i==2) {
      index_end <- indices[(i+1)]-2
      var_name <- append(var_name, c(airDF[indices[i],5],airDF[indices[i],6]), after=length(var_name))
      airCleanDF[,11] <- airDF[index_start:index_end,5]
      airCleanDF[,12] <- airDF[index_start:index_end,6]
    } else if (i==3) {
      index_end <- indices[(i+1)]-2
      var_name <- append(var_name, c(airDF[indices[i],5],airDF[indices[i],6],airDF[indices[i],7],
                                     airDF[indices[i],8],airDF[indices[i],9],airDF[indices[i],10],
                                     airDF[indices[i],11]), after=length(var_name))
      airCleanDF[,13] <- airDF[index_start:index_end,5]
      airCleanDF[,14] <- airDF[index_start:index_end,6]
      airCleanDF[,15] <- airDF[index_start:index_end,7]
      airCleanDF[,16] <- airDF[index_start:index_end,8]
      airCleanDF[,17] <- airDF[index_start:index_end,9]
      airCleanDF[,18] <- airDF[index_start:index_end,10]
      airCleanDF[,19] <- airDF[index_start:index_end,11]
    } else if (i==4) {
      index_end <- indices[(i+1)]-2
      var_name <- append(var_name, c(airDF[indices[i],9],airDF[indices[i],10]), after=length(var_name))
      airCleanDF[,20] <- airDF[index_start:index_end,9]
      airCleanDF[,21] <- airDF[index_start:index_end,10]
    }
  }
  
  ### Cleaning up our airCleanDF data table
  names(airCleanDF)[9:21] <- unlist(var_name) # we are going to give our tacked-on columns more informative names
  
  return(airCleanDF)
}