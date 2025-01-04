### Elastic Net Logistic Regression Model

library(tidyverse)
library(caret)
library(glmnet)
library(DMwR)
library(gtools)
library(pROC)
library(mice)

# install.packages("tidyverse")
# install.packages("caret")
# install.packages("glmnet")
# install.packages("DMwR")
# install.packages("gtools")
# install.packages("ROSE")
# install.packages("mice)

# rm(list = ls())

wd <-“.”

# values to change based on which data you want to tests
setwd(paste0(wd,"/Data/Behavioral/Prepared_Data"))
data_filename = "dim_reduced_depressive_unique_behavioral.csv"
dep_var <- "Depressive"
# dep_var <- "Anxiety"
control_vars <- c("age","sex","race","dehq_16","ethnicity","ses_score")
tuning_two_params <- F # tuning both parameters with a narrowed grid or tunelength
tuning_one_param <- F # tuning only one parameter by holding another constant
generate_new_split <- T # a new split of training/test
set.seed(1) # for reproducability
training_split_proportion <- 0.7
num_imps <- 20 # number of imputations
num_runs <- ifelse(tuning_two_params | tuning_one_param, 1, 1000) # 20 imputations per run
tuned_hyperparameters <- list("alpha" = 0.2, "lambda" = 10^-0.5) # set to NULL until you tune it

# error checking
if(tuning_two_params & tuning_one_param){ stop("Tuning one and two parameters are both TRUE") }
# prepare parameters
save_coefs <- !tuning_two_params & !tuning_one_param
tuned_alpha <- tuned_hyperparameters$alpha
tuned_lambda <- tuned_hyperparameters$lambda
model_def <- as.formula(paste(dep_var, "~", ".")) # the model we are using (DVs and IVs)
# empty dataframes/lists for storage -- need to translate to use lapply instead
hyperparameter_runs <- hyperparameter_runs_uncertainty <- coefs_df <- data.frame()
model_list <- num_parameters_list <- num_parameters_sd_list <- accuracy_list <- 
  accuracy_sd_list <- kappa_list <- kappa_sd_list <- sensitivity_list <- sensitivity_sd_list <- 
  specificity_list <- specificity_sd_list <- auc_list <- auc_sd_list <- list()

# regress out (control for) variables
# outputs residuals of a dataframe once the control variables 
# have been regressed out of all independent vars
regress_out_control_vars <- function(df, control_vars, dep_var){
  dep_var_df <- as.data.frame(df[,dep_var])
  names(dep_var_df) <- dep_var
  df <- df[,-which(names(df) %in% c(dep_var))]
  
  # compute residuals once control variables have been regressed out
  residuals_df <- data.frame(matrix(nrow=nrow(df),ncol= 0))
  for(col_name in names(df[,-which(names(df) %in% c(control_vars, dep_var))])){
    model_def <- as.formula(paste(col_name, "~", paste(control_vars, collapse="+")))
    model <- lm(model_def, data = df)
    residuals_df[,col_name] <- residuals(model)
  }
  residuals_df <- cbind(dep_var_df, residuals_df)
  residuals_df
}

# generates a new split of the training and test set 70/30
if(generate_new_split){
  # read in data
  setwd(paste0(wd,"/Data/Behavioral/Prepared_Data"))
  behavioral_df <- read.csv(data_filename, stringsAsFactors = FALSE)
  behavioral_df <- behavioral_df[,-which(names(behavioral_df) %in% 
                                           c("Identifiers","ursi","visit"))]
  behavioral_df <- as.data.frame(lapply(behavioral_df, as.numeric))
  
  training_samples <- behavioral_df[,dep_var] %>% 
    createDataPartition(p = training_split_proportion, list = FALSE)
  # Give binary dependent variable levels
  if(length(unique(behavioral_df[,dep_var])) == 2){
    behavioral_df[,dep_var] <- factor(behavioral_df[,dep_var])
    levels(behavioral_df[,dep_var]) <- c("Healthy","Depressed")
  }else{
    stop("dependent variable is NOT binary")
  }
  # Create training data
  train_data  <- behavioral_df[training_samples, ]
  # scale training data
  train_data[,-which(names(train_data) %in% c(dep_var,control_vars))] <- 
    data.frame(scale(train_data[,-which(names(train_data) %in% c(dep_var,control_vars))]))
  scale_params <- preProcess(train_data[,-which(names(train_data) %in% c(dep_var,control_vars))])
  train_data_no_SMOTE <- train_data
  # Create test data
  test_data <- behavioral_df[-training_samples, ]
  # scale test data
  test_data[,-which(names(test_data) %in% c(dep_var,control_vars))] <- 
    predict(scale_params, test_data[,-which(names(test_data) %in% c(dep_var,control_vars))])
  test_data[,-which(names(test_data) %in% c(dep_var,control_vars))] <- 
    scale(test_data[,-which(names(test_data) %in% c(dep_var,control_vars))])
  # multiple imputation with MICE
  train_imputations <- mice(train_data, m = num_imps, printFlag = FALSE)
  test_imputations <- mice(test_data, m = num_imps, printFlag = FALSE)
}

# replicate model
for(run in 1:num_runs){
  print(paste("Run:", run))
  
  # reset training data
  train_data <- train_data_no_SMOTE
  
  # empty dataframes/lists for storage -- need to translate to use lapply instead
  imp_coefs_df <- data.frame()
  train_data_imp <- test_data_imp <- imp_coefs <- imp_num_parameters <- imp_accuracy <-
  imp_kappa <- imp_sensitivity <- imp_specificity <- imp_auc <- list()
  
  # fits elastic net logistic regression for each imputation
  for(i in 1:num_imps){
    # prepare train and test data from imputations
    train_data_imp[[i]] <- complete(train_imputations, i)
    test_data_imp[[i]] <- complete(test_imputations, i)
    # regress out age and sex of train and test set
    train_data_imp[[i]] <- regress_out_control_vars(as.data.frame(train_data_imp[[i]]), control_vars, dep_var)
    test_data_imp[[i]] <- regress_out_control_vars(as.data.frame(test_data_imp[[i]]), control_vars, dep_var)
    
    # Handle imbalanced training data with SMOTE
    length_is_0 <- length(train_data_imp[[i]][which(train_data_imp[[i]][,dep_var] == "Healthy"), dep_var])
    length_is_1 <- length(train_data_imp[[i]][which(train_data_imp[[i]][,dep_var] == "Depressed"), dep_var])
    imbalanced_threshold <- 3 # at least 3/4 and 1/4 or 75% and 25%
    # If dependent variable is imbalanced
    if(length_is_0 > length_is_1 * imbalanced_threshold |
       length_is_1 > length_is_0 * imbalanced_threshold ) {
      # print("imbalanced data, using SMOTE synthesis")
      #train_data <- ROSE(model_def, data = train_data)$data # ROSE synthesis
      #smote_data <- smotefamily::SMOTE(train_data[,-which(colnames(train_data) %in% dep_var)], 
      #                                 train_data[,dep_var])$data # smote family synthesis
      #colnames(smote_data)[colnames(smote_data) == "class"] <- dep_var
      train_data_imp[[i]] <- DMwR::SMOTE(model_def, data = train_data_imp[[i]], 
                                         perc.over = 200, perc.under = 150) # smote from DMwR undersamples as well
    }else{ # dependent variable is balanced
      # print("balanced data")
    }
    table(train_data_imp[[i]][dep_var])
    
    # Creates predictor and predictand
    x <- model.matrix(model_def, train_data_imp[[i]])[,-1]
    y <- train_data_imp[[i]][,dep_var]
    x_test <- model.matrix(model_def, test_data_imp[[i]])[,-1]
    
    # run logistic regression with elastic net
    
    # select grid
    if(tuning_two_params){
      grid = expand.grid(alpha = seq(0,1,0.1), lambda = round(10^seq(-3,1,0.5),4)) # narrowed grid
    }else if(tuning_one_param){
      grid = expand.grid(alpha = seq(0,1,0.1), lambda = tuned_lambda) # hold lambda constant to find alpha
    }else{
      grid = NULL
    }
    
    # if tuning, outputs graphs
    if(tuning_two_params | tuning_one_param){
      model <- train(
        model_def, data = train_data_imp[[i]], method = "glmnet", family = "binomial",
        trControl = trainControl("cv", number = 10, classProbs = TRUE, 
                                 summaryFunction = twoClassSummary), # k-fold cv resampling, k = 10, to find MSE approx
        # tuneLength = 10, # randomly choose hyperparameters from range to view graphs
        tuneGrid = grid, metric = "ROC"
      )
      # Visualizations
      if(tuning_two_params){ # Visualize alpha and lambda
        setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Tuning/Lambda_and_Alpha"))
        png(file = paste0("behavioral_parameters_imp_", i,"_run_",run,".png"), width = 500, height = 300)
        print(plot(model))
        dev.off()
      }else if(tuning_one_param){ # Visualize alpha
        setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Tuning/Alpha"))
        png(file = paste0("behavioral_parameters_imp_", i,"_run_",run,".png"), width = 500, height = 300)
        print(plot(model))
        dev.off()
      }
    }else{ # if hyperparameters determined, tests on test set, outputs results
      alpha = tuned_alpha
      lambda = tuned_lambda
      model <- glmnet::glmnet(x, y, family = "binomial", alpha = alpha, lambda = lambda)
      model_list[[i]] <- model
      
      # finds coefficients
      imp_coefs <- coef(model)
      imp_num_parameters[[i]] <- nrow(summary(imp_coefs))
      # adds imp_coefs to imp_coefs_df
      tmp <- imp_coefs
      summary_imp_coefs <- summary(imp_coefs)
      imp_coefs <- as.list(summary_imp_coefs$x)
      names(imp_coefs) <- rownames(tmp)[summary_imp_coefs$i]
      cur_run_imp_coefs <- as.data.frame(imp_coefs)
      imp_coefs_df <- smartbind(imp_coefs_df, cur_run_imp_coefs)
      
      # use metrics to compare to prediction from test set
      predictions <- factor(predict(model, newx = x_test, type = "class"))
      predictions <- factor(predictions, levels = c("Healthy","Depressed"))
      imp_confusion_matrix <- confusionMatrix(predictions, test_data[,dep_var], positive = "Depressed")
      imp_accuracy[[i]] <- as.list(imp_confusion_matrix$overall)$Accuracy
      imp_kappa[[i]] <- as.list(imp_confusion_matrix$overall)$Kappa
      imp_sensitivity[[i]] <- as.list(imp_confusion_matrix$byClass)$Sensitivity
      imp_specificity[[i]] <- as.list(imp_confusion_matrix$byClass)$Specificity
      imp_auc[[i]] <- auc(roc(test_data[,dep_var], as.vector(predict(model, newx = x_test, type = "response")), quiet=TRUE))
    }
  }
  if(!tuning_two_params & !tuning_one_param){
    # pool imputation results
    mean_imp_coefs <- t(as.data.frame(colMeans(imp_coefs_df,na.rm = T)))
    rownames(mean_imp_coefs) <- c(run)
    coefs_df <- smartbind(coefs_df, mean_imp_coefs)
    num_parameters_list[[run]] <- mean(unlist(imp_num_parameters))
    num_parameters_sd_list[[run]] <- sd(unlist(imp_num_parameters))
    accuracy_list[[run]] <- mean(unlist(imp_accuracy))
    accuracy_sd_list[[run]] <- sd(unlist(imp_accuracy))
    kappa_list[[run]] <- mean(unlist(imp_kappa))
    kappa_sd_list[[run]] <- sd(unlist(imp_kappa))
    sensitivity_list[[run]] <- mean(unlist(imp_sensitivity))
    sensitivity_sd_list[[run]] <- sd(unlist(imp_sensitivity))
    specificity_list[[run]] <- mean(unlist(imp_specificity))
    specificity_sd_list[[run]] <- sd(unlist(imp_specificity))
    auc_list[[run]] <- mean(unlist(imp_auc))
    auc_sd_list[[run]] <- sd(unlist(imp_auc))
    
    # Add current runs to hyperparameter_runs (means) and hyperparameter_runs uncertainty (sds)
    summary <- t(data.frame(c(alpha, lambda, 
                              num_parameters_list[[run]], 
                              accuracy_list[[run]], 
                              kappa_list[[run]], 
                              sensitivity_list[[run]], 
                              specificity_list[[run]],
                              auc_list[[run]])))
    rownames(summary) <- c(run)
    summary_uncertainty <- t(data.frame(c(alpha, lambda, 
                                          num_parameters_sd_list[[run]], 
                                          accuracy_sd_list[[run]], 
                                          kappa_sd_list[[run]], 
                                          sensitivity_sd_list[[run]], 
                                          specificity_sd_list[[run]],
                                          auc_sd_list[[run]])))
    # print(summary)
    rownames(summary_uncertainty) <- c(run)
    hyperparameter_runs <- rbind(hyperparameter_runs, summary)
    hyperparameter_runs_uncertainty <- rbind(hyperparameter_runs_uncertainty, summary_uncertainty)
  }
}
if(!tuning_two_params & !tuning_one_param){
  # add column names to runs and uncertainty
  runs_col_names <- c("Alpha","Lambda","Num_Parameters", "Accuracy", 
                      "Kappa", "Sensitivity", "Specificity", "AUC")
  colnames(hyperparameter_runs) <- runs_col_names
  colnames(hyperparameter_runs_uncertainty) <- runs_col_names
  # save results as csv files
  setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Runs/Model_Replication/"))
  write.csv(hyperparameter_runs,"elastic_net_logistic_regression_behavioral_runs.csv", row.names = F)
  write.csv(hyperparameter_runs_uncertainty,
            "elastic_net_logistic_regression_behavioral_runs_uncertainty.csv", row.names = F)
  #tail(hyperparameter_runs)
}
# reformats and creates csv files of coefficients
factor_to_numeric <- function(x){ as.numeric(as.character(x)) } # converts factor to numeric
if(save_coefs){
  setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Coefficients"))
  #coef_rownames <- coefs_df$name
  coefs_df <- as.data.frame(lapply(coefs_df,factor_to_numeric))
  #rownames(coefs_df) <- rownames
  coefs_df[is.na(coefs_df)] <- 0
  write.csv(coefs_df,"repl_coefs.csv", na="", row.names = F)
}
print("Finished all Runs")
colMeans(hyperparameter_runs)