### Confidence Intervals for t-tests

wd <-“.”

# finds the t-level confidence interval
t_confidence_interval <- function(data, sd, level, n){
  mean <- mean(data)
  samp_sd <- sd
  alpha <- 1-level
  error <- qt(1-alpha/2,df=n-1)*samp_sd/sqrt(n)
  left <- mean-error
  right <- mean+error
  ci <- as.list(round(c(mean,left,right),3))
  names(ci) <- c("Mean","Left_95_CI","Right_95_CI")
  ci
}

# finds the t-level confidence interval of all columns in a df
find_all_t_confidence_intervals <- function(df, sds, n){
  ci95 <- t(mapply(t_confidence_interval, data = df, 
                   sd = sds, level = 0.95, n = n))
  ci95[c("Alpha","Lambda"),c("Left_95_CI", "Right_95_CI")] <- NA
  ci95
}

# Each distribution is normal with a mean and sd
# Combine these distributions as an average by adding 
# variances and scaling variance by sample size n
average_sds <- function(sds, n){
  vars <- sds^2
  sqrt(sum(vars)/n)
}

# finds confidence intervals of the behavioral model
setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Runs/Model_Replication/"))
hyperparameter_runs <- read.csv("elastic_net_logistic_regression_behavioral_runs.csv")
num_repl <- nrow(hyperparameter_runs)
runs_uncertainty <- read.csv("elastic_net_logistic_regression_behavioral_runs_uncertainty.csv")
average_sds <- lapply(runs_uncertainty, average_sds, num_repl)
setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Confidence_Intervals/"))
ci95_summary <- find_all_t_confidence_intervals(hyperparameter_runs, unlist(average_sds), num_repl)
write.csv(ci95_summary,"Behavioral_Confidence_Interval_Summary.csv")