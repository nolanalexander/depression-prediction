### Permutation testing of Elastic Net Model

wd <-“.”
folder <- "Behavioral"

setwd(paste0(wd,"/Results/Logistic_Regression/", folder, "/Runs/Model_Replication"))
runs_df <- read.csv("elastic_net_logistic_regression_behavioral_runs.csv")
setwd(paste0(wd,"/Results/Logistic_Regression/", folder, "/Runs/Permutations"))
perm_df <- read.csv("permutations_elastic_net_logistic_regression_behavioral_runs.csv")
ttest_validation <- t.test(runs_df$Accuracy, perm_df$Accuracy)
ttest_validation