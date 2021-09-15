###==================================================================
### R script to run through and perform analyses for week 3
###==================================================================
## Libaries we need to load
library(dplyr)
library(ggplot2)
library(googlesheets4)

## Functions for this week's session
source("https://sebastiansauer.github.io/Rcode/clt_1.R") # we are going to use this piece of code
source("https://raw.githubusercontent.com/EA30POM/Fall2021/main/helpers/simuPval.R") # we are going to use this function as well

###==================================================================
### 1: Simulating AQI observations
###==================================================================

sim_AQI_data(true_mean=75, true_sd=25, n_obs=30, how_many=9) # trial 1

sim_AQI_data(true_mean=75, true_sd=25, n_obs=50, how_many=20) # trial 2

sim_AQI_data(true_means= , true_sd= , n_obs= , how_many= ) # provide the values yourself for true_means, etc.

###==================================================================
### 2: Moving from samples to probabilities (of mean values)
###==================================================================

simu_p(n_samples = 1000, mean=75, sd=25, n=10, distribution="normal", sample_mean=80)

simu_p(n_samples = 1000, mean=75, sd=25, n=10, distribution="normal", sample_mean=90)

simu_p(n_samples = 1000, mean=75, sd=40, n=10, distribution="normal", sample_mean=80)

simu_p(n_samples = 1000, mean=75, sd=25, n=20, distribution="normal", sample_mean=80)

simu_p(n_samples = 1000, mean=75, sd=25, n=20, distribution="normal", sample_mean=90)

###==================================================================
### 3: Calculating a confidence interval for the mean
###==================================================================

samp_mean <- 90 # our sample mean
samp_sd <- 25 # our sample standard deviation
samp_n <- 20 # our sample number of observations
samp_df <- samp_n - 1 # we are estimating 1 statistic, so we lose 1 degree of freedom from our sample size
samp_se <- samp_sd / sqrt(samp_n) # calculating our standard error
AQI_95_CI_min <- samp_mean - qt(p=0.975,df = samp_df)*samp_se # lower limit
# why p=0.975? 
# we take 1 - (1-0.95)/2 to get a symmetric distribution, equivalent to 0.025 and 0.975, 
# which means that we have 0.95 in between these two values
AQI_95_CI_max <- samp_mean + qt(p=0.975,df = samp_df)*samp_se # upper limit

# Print out values:
paste0("The 95% CI for mean AQI is [",signif( AQI_95_CI_min, 3),",",signif(AQI_95_CI_max,3),"].",collapse="") # print output; use signif to round to 3 significant figures

###==================================================================
### 4: Performing a t-test
###==================================================================

## Read in data
farmDF <- googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1Tezs6c3JoCtrdKysVZ02-G7fETLll--v0ZX0Jo9OaB0/edit?usp=sharing",sheet="Farm_Clean") # storing the clean Farm data in farmDF
roadDF <- googlesheets4::read_sheet(ss="https://docs.google.com/spreadsheets/d/1GIJL9RvfXFiES_bA6rqdpFEX8V24FxX0dfSYmSGTCfw/edit?usp=sharing",sheet="Clean_Roadside") # storing the clean roadside data in roadDF

## Clean up column names
name_vec <- c("Lat","Lng","Date","tSec","Pressure_mBar","TempC","RelHumPerc","Lux",
              "OzonePPB","MeanOzonePPB","CO2ppm","MeanCO2ppm","PM1ugm3","MeanPM1",
              "PM2_5ugm3","MeanPM2_5ugm3","PM10ugm3","MeanPM10ugm3","AQI",
              "HeatIndexC","DewPointC") # creating a vector with cleaner column names
names(farmDF) <- name_vec # changing the column names for farmDF
names(roadDF) <- name_vec # changing the column names for farmDF

## Take a quick look at our data
farmDF %>% head() %>% View()

## Two ways to summarize the AQI data
print("Summary of AQI values for farm")
# Here, we're going to use more modern R "syntax" (grammar/commands)
farmDF %>% # feed farm DF forward into the next function
  dplyr::select(AQI) %>% # pulling out the column AQI
  summary() # display the summary of this value 

print("Summary of AQI values for road")
summary( roadDF$AQI ) # a different way to call summary using old-school R 
# we extract the column AQI from roadDF by using the $ operator (?`$` FMI)
# and that is fed into the command summary()

## Running a t-test
my_t_test <- t.test(roadDF$AQI, farmDF$AQI) # run a t-test and store it in my_t_test
my_t_test # print out information about the t-test

## One-sided t-test
one_sided_t_test <- t.test(roadDF$AQI, farmDF$AQI, alternative = "greater") # specify a different H_a: road AQI > farm AQI
one_sided_t_test # display test results

## Same test, but just reversing order from above
one_sided_t_test <- t.test(farmDF$AQI, roadDF$AQI, alternative="less") # farm AQI < road AQI
one_sided_t_test # display test

###==================================================================
### 5: Comparing more than two means
###==================================================================

## Reading in data
df3 <- read_sheet(ss="https://docs.google.com/spreadsheets/d/1P1orGfuKIG2E4IuCoE1HKyPS09d6x4pu_OlV3IWGPFY/edit?usp=sharing",sheet="Clean") # read in the Clean datasheet from this Googlesheet and store it in df3
df2 <- read_sheet(ss="https://docs.google.com/spreadsheets/d/1V79fkoO-CoHtiqtLKYnVc1MTwuyonhRoWNJfaOQChKU/edit?usp=sharing",sheet="Clean") # read in another Googlesheet's Clean sheet and store in df2
df1 <- read_sheet(ss="https://docs.google.com/spreadsheets/d/1iznRXt4vFcFlkLCRNHHAEXG19CAPMc6r14K9oLoiDkY/edit?usp=sharing",sheet="Clean") # same as above, but store another Googlesheet's Clean sheet as df1

## Renaming data
birdDF <- bind_rows(df1,df2,df3) # combine the 3 data sets row by row
birdDF <- birdDF %>% na.omit() # remove rows that are all missing values
names(birdDF) <- name_vec # we will rename the columns using name_vec from before

## Categorizing data into distance bins
dist_labels <- c(96.6, 207, 300) # the distance classes to the gym associated with the time slices
birdDF <- birdDF %>%
  mutate(Distances = cut(birdDF$Date,breaks="20 min",labels=dist_labels)) # slice the date times into categories based on 20 minute intervals and label each slice based on the distance labels, store this variable in a new column called Distances using the command, mutate 

# Here, we can see the counts of each categorical break by time
# How many observations does each grouped set of data have?
birdDF %>% # feed birdDF forward
  dplyr::select(Distances) %>% # pull out the column Distances; equivalent to birdDF$Distances
  table() # show us the counts by each unique value in Distances

# We can also see the mean values for AQI for the different distances this way
birdDF %>% # feed birdDF forward to the next function
  group_by(Distances) %>% # bucket the data into different Distance bins
  summarize(minAQI = min(AQI), medianAQI = median(AQI), meanAQI = mean(AQI), maxAQI = max(AQI)) # produce a data summary based on the AQI values observed in each distance class

## Performing the ANOVA test
aov_test <- aov( lm(AQI~Distances, data=birdDF )) # perform an ANOVA test of AQI as a function of Distances, which are both found in the object birdDF and store it in aov_test
summary( aov_test ) # show our output for aov_test using the command summary( ); note that you don't need to use summary( ) for the t-test output though 

## Performing the Tukey HSD
TukeyHSD( aov_test,"Distances") # tell the command the name of your ANOVA model (aov_test) and the column used to categorize the data (Distances)

###==================================================================
### 6: Performing a linear regression
###==================================================================

### First, we need to convert Distances from a factor (categorical)
### data to numeric data
birdDF <- birdDF %>% # feed birdDF forward to next function/command
  mutate(Distances = as.numeric(Distances)) # make Distances a number instead of a factor using mutate

### Running the lm command
AQImodel <- lm(AQI ~ Distances, data=birdDF)

### Showing the coefficient estimates
summary(AQImodel)