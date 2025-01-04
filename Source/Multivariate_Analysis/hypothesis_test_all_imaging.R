### Hypothesis Testing for Imaging Data

library(mgcv)

wd <-“.”

# Read in data
setwd(paste0(wd,"/Data/Imaging/Prepared_Data"))
data_filename = "dim_reduced_depressive_unique_avg_volume_global.csv"
dep_var <- "Depressive"
control_vars <- c("age","sex","race","dehq_16","ethnicity","ses_score")
indep_vars <- c(dep_var, control_vars, "Depressive_X_age")
imaging_df <- read.csv(data_filename, stringsAsFactors = FALSE)
imaging_df <- imaging_df[,-which(names(imaging_df) %in% 
                                   c("Identifiers","ursi","visit"))]
imaging_df["Depressive_X_age"] <- imaging_df$Depressive * imaging_df$age
imaging_features = colnames(imaging_df)[!colnames(imaging_df) %in% c(dep_var, control_vars, "Depressive_X_age")]
pvals = data.frame(row.names=imaging_features)

# Calcuate p values
dep_lm_pvals = interac_lm_pvals = dep_gam_pvals = interac_gam_pvals = c()
for(imaging_feature in imaging_features){
  model1 <-lm(as.formula(paste(imaging_feature, "~ Depressive + Depressive*age + age + sex + race + ethnicity + ses_score + dehq_16")), data=imaging_df, na.action=na.omit)
  interac_lm_pval <- summary(model1)$coefficients["Depressive:age","Pr(>|t|)"]
  dep_lm_pval <- summary(model1)$coefficients["Depressive","Pr(>|t|)"]
  interac_lm_pvals = c(interac_lm_pvals, interac_lm_pval)
  dep_lm_pvals = c(dep_lm_pvals, dep_lm_pval)
  
  model2 <- gam(as.formula(paste(imaging_feature, "~ Depressive + s(age, by=Depressive) + s(age) + sex + race + ethnicity + s(ses_score) + s(dehq_16)")), data=imaging_df, na.action=na.omit)
  interac_gam_pval <- summary(model2)$s.pv[1] # Depressive:age p value
  dep_gam_pval <- summary(model2)$p.pv["Depressive"]
  interac_gam_pvals = c(interac_gam_pvals, interac_gam_pval)
  dep_gam_pvals = c(dep_gam_pvals, dep_gam_pval)
}

# save results
pvals["interac_lm_pvals"] = interac_lm_pvals
pvals["dep_lm_pvals"] = dep_lm_pvals
pvals["interac_gam_pvals"] = interac_gam_pvals
pvals["dep_gam_pvals"] = dep_gam_pvals
pvals = round(pvals, 2)
setwd(paste0(wd,"/Results/"))
write.csv(pvals,"Imaging_Beta_Test_Results.csv")

# # Calcuate p values
# for(indep_var in indep_vars){
#   indep_var_pvals = c()
#   for(imaging_feature in imaging_features){
#     # Binary -> hypothesis test
#     if(n_distinct(imaging_df[indep_var]) == 2){
#       cur_pval = t.test(imaging_df[which(imaging_df[indep_var] != 1),imaging_feature], 
#                         imaging_df[which(imaging_df[indep_var] != 0),imaging_feature])$p.value 
#     }
#     # Continuous -> regression, test beta
#     else{
#       linear_model <- lm(as.formula(paste(imaging_feature, "~", indep_var)), data=imaging_df)
#       cur_pval = summary(linear_model)$coefficients[2,4]
#     }
#     indep_var_pvals = c(indep_var_pvals, cur_pval)
#   }
#   pvals[indep_var] = indep_var_pvals
# }
