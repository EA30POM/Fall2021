sim_AQI_data <- function(true_mean=50, true_sd=50, n_obs=20, how_many=30) {
  ### In this function, we provide the true_mean (mean AQI value for our pretend area)
  ### true_sd, the true standard deviation for our pretend area,
  ### n_obs, the number of observations we take with an air quality device, and
  ### how_many, the number of pretend worlds we create. I recommend staying between 3-30 for ease of viewing the output
  
  simAQI <- c(); which_draw <- c()
  for (i in c(1:30)) {
    sim_i <- rnorm(n=n_obs, mean=true_mean, sd=true_sd) # simulating a set of AQI observations
    simAQI <- append(simAQI, sim_i, after=length(simAQI)) # storing that set of values in a vector
    which_draw <- append(which_draw, rep(i,n_obs),after=length(which_draw)) # storing which draw we've run in a vector
  }
  
  simDF <- tibble::tibble(AQI=simAQI,Draw=which_draw)
  
  p <- ggplot(simDF, aes(x=AQI,lab=Draw))
  p <- p + geom_histogram()
  p <- p + facet_wrap(~Draw)
  p <- p + geom_vline(data = plyr::ddply(simDF, "Draw", summarize, wavg = mean(AQI)), aes(xintercept=wavg),col="blue")
  p <- p + labs(title = paste("AQI samples for",how_many,"samples where\neach sample has",n_obs,"observations"),y="Count")
  p <- p + theme_bw()
  print(p)
}