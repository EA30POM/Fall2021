###================================================================
### Group 1
###================================================================

Results_table <- tally(~ Victimhood + Country, data=group1sum2 )
SimulateChiSq <- do(1000) * chisq( tally( ~ Victimhood + shuffle(Country), data=group1sum2))
sim_p_val <- length(which(SimulateChiSq$X.squared < obs_chi))/nrow(SimulateChiSq)

###================================================================
### Group 3
###================================================================

### 3.3: Assessing differences in sentiment for native and non-native countries
  # 3.3.1: Calculate our observed difference for sentiment for the native and non-native range
obs_diff <- diff( mean( Sentiment ~ ElephantRange, data = group3sub1 , na.rm=T)) # calculate the difference between the means and store in a variable called "obs_diff"
obs_diff # display the value of obs_diff
  # 3.3.2: Shuffle the data and calculate the simulated differences in mean Sentiment
randomizing_diffs <- do(1000) * diff( mean( Sentiment ~ shuffle(ElephantRange),na.rm=T, data = group3sub1) ) # calculate the mean in SoilDensity when we're shuffling the plant community around 1000 times
  # Clean up our shuffled data
names(randomizing_diffs)[1] <- "DiffMeans" # change the name of the variable
  # View first few rows of data
head(randomizing_diffs)
  # View the mean of the simulated differences
mean(randomizing_diffs$DiffMeans) # is this larger or smaller than our observed difference

### 3.4: Visualizing the distribution of differences in mean
  # THINK ABOUT if it should be DiffMeans > obs_diff or DiffMeans < obs_diff
gf_histogram(~ DiffMeans, fill = ~ (DiffMeans > obs_diff),
             data = randomizing_diffs,
             xlab="Difference in ... under null",
             ylab="Count")
# What proportion of simulated values were "more extreme" than our value?
prop( ~ DiffMeans > obs_diff, data = randomizing_diffs)

###================================================================
### Group 5
###================================================================

### 5.3 - Analyzing differences in means
  # 5.3.1: Calculate our observed difference for sentiment for the native and non-native range
obs_diff <- diff( mean( Sentiment ~ Taxon, data = group5sub , na.rm=T)) # calculate the difference between the means and store in a variable called "obs_diff"
obs_diff # display the value of obs_diff
  # 5.3.2: Shuffle the data and calculate the simulated differences in mean Sentiment
randomizing_diffs <- do(1000) * diff( mean( Sentiment ~ shuffle(Taxon),na.rm=T, data = group5sub) ) # calculate the mean in SoilDensity when we're shuffling the plant community around 1000 times
  # Clean up our shuffled data
names(randomizing_diffs)[1] <- "DiffMeans" # change the name of the variable
  # View first few rows of data
head(randomizing_diffs)
  # View the mean of the simulated differences
mean(randomizing_diffs$DiffMeans) # is this larger or smaller than our observed difference

### 5.4: Visualizing the distribution of differences in mean
  # THINK ABOUT if it should be DiffMeans > obs_diff or DiffMeans < obs_diff
gf_histogram(~ DiffMeans, fill = ~ (DiffMeans > obs_diff),
             data = randomizing_diffs,
             xlab="Difference in ... under null",
             ylab="Count")
  # What proportion of simulated values were "more extreme" than our value?
prop( ~ DiffMeans < obs_diff, data = randomizing_diffs)

### 5.8: Evaluating differences between countries
anova(lm(Sentiment~Country,data=group5sub1))