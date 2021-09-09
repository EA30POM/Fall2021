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

index_checker <- function(index_start, index_end,nrows) {
  # Takes in the index_start position, index_end position, and the number of rows
  segment <- c(index_start:index_end) # store the integer start and end points of subset sensor data
  seg_length <- length(segment) # how long is the segment (how many entries)
  if (seg_length > nrows) { # subset data are too many
    index_end <- index_start + nrows -1
  } else if (seg_length < nrows) { # too few data
    index_end <- index_start + nrows -1
  } else {
    index_end <- index_end # don't make any changes
  }
  return(index_end) # return modified index
}

cleanup_air_function <- function(airDF,airDF_struct) {
  # Please provide both airDF and airDF_struct to this function
  
  # Now we are going to use several cool "Tidyverse" features
  # to automagically convert all of the columns from list to vectors
  airDF <- airDF %>% 
    mutate( across( where(is.list), my_unlisting_function)) # we're applying this function (my_unlisting function) to each column that is a list (which is each column)
  
  ### Storing the row locations that have added-on data
  indices <- which(airDF$Lat=="Lat") # we're using the fact that every time PocketLab
  # adds on another variable from the sensor, it tacks on "Lat" at the start of the row.
  ### We had to use the unstructured airDF object to find those indices
  ### Now we're going to proceed with the cleaner, structured data
  airDF <- airDF_struct %>%
    mutate( across( where(is.list), my_unlisting_function))
  
  ### We are going to instantiate a clean data table and build from there
  airCleanDF <- airDF[1:(indices[1]-2),c(1:8)]
  nrows_clean <- nrow(airCleanDF)
  
  ### Now we are going to create a set of individual data tables
  ### to store our data more cleanly.
  ### The for loop below is going to "walk" through each value of the indices object
  ### and it is going to use the row values in indices to extract the sensor data we
  ### want and tack it on as added columns to our clean airCleanDF data table.
  var_name <- c()
  for (i in c(1:length(indices))) {
    #print(i)
    index_start <- indices[i]+1
    if (i==1) { # Ozone sensor data
      index_end <- indices[(i+1)]-2
      index_end <- index_checker(index_start,index_end,nrows_clean)
      var_name <- append(var_name, c("OzonePPB","MeanOzonePPB"), after=length(var_name))
      airCleanDF[,1] <- airDF[index_start:index_end,1] # updating lat
      airCleanDF[,2] <- airDF[index_start:index_end,2] # updating lng
      airCleanDF[,3] <- airDF[index_start:index_end,3] # updating time stamp
      airCleanDF[,9] <- airDF[index_start:index_end,5]
      airCleanDF[,10] <- airDF[index_start:index_end,6]
    } else if (i==2) { # CO2 sensors
      index_end <- indices[(i+1)]-2
      index_end <- index_checker(index_start,index_end,nrows_clean)
      var_name <- append(var_name, c("CO2ppm","MeanCO2ppm"), after=length(var_name))
      airCleanDF[,11] <- airDF[index_start:index_end,5]
      airCleanDF[,12] <- airDF[index_start:index_end,6]
    } else if (i==3) { # PM sensors - seems like there are some read-in issues for the names sometimes
      index_end <- indices[(i+1)]-2
      index_end <- index_checker(index_start,index_end,nrows_clean)
      var_name <- append(var_name, c("PM1ugm3","MeanPM1","PM2_5ugm3","MeanPM2_5ugm3","PM10ugm3","MeanPM10ugm3","AQI"),after=length(var_name))
      airCleanDF[,13] <- airDF[index_start:index_end,5]
      airCleanDF[,14] <- airDF[index_start:index_end,6]
      airCleanDF[,15] <- airDF[index_start:index_end,7]
      airCleanDF[,16] <- airDF[index_start:index_end,8]
      airCleanDF[,17] <- airDF[index_start:index_end,9]
      airCleanDF[,18] <- airDF[index_start:index_end,10]
      airCleanDF[,19] <- airDF[index_start:index_end,11]
    } else if (i==4) { # thermal sensors
      index_end <- indices[(i+1)]-2
      index_end <- index_checker(index_start,index_end,nrows_clean)
      var_name <- append(var_name, c("HeatIndexC","DewPointC"), after=length(var_name))
      if (length(c(index_start:index_end)) > dim(airCleanDF)[1]) {
        index_end <- index_start + dim(airCleanDF)[1]-1
      }
      airCleanDF[,20] <- airDF[index_start:index_end,9]
      airCleanDF[,21] <- airDF[index_start:index_end,10]
    }
  }
  
  ### Cleaning up our airCleanDF data table
  names(airCleanDF)[9:21] <- unlist(var_name) # we are going to give our tacked-on columns more informative names
  
  return(airCleanDF)
}