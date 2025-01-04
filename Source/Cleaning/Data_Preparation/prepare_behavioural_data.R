### Preprocessing behavioral data

wd <-“.”

# Read in data
setwd(paste0(wd,"Data/Behavioral"))
masterfile_df <- read.csv("behavioral_covariates_group.csv", stringsAsFactors = FALSE, na.strings="")
masterfile_df <- masterfile_df[match(unique(masterfile_df$ursi), masterfile_df$ursi),] # unique patients only
masterfile_df[masterfile_df == ""] <- NA
behavioral_predictors_df <- read.csv("behavioral_predictors.csv", stringsAsFactors = FALSE, na.strings=c(""))
behavioral_predictors <- behavioral_predictors_df$behavioral_predictors

identifiers <- c("Identifiers","ursi","visit")
dep_var <- "Depressive" # Change to modify the dependent variable
# dep_var <- "Anxiety" # Change to modify the dependent variable
control_vars <- c("age","sex","race","dehq_16","ethnicity","ses_score")
filter <- TRUE

df <- masterfile_df
df <- df[which(!is.na(df$age) & !is.na(df$sex)),] # remove all patients without age or sex data
df <- df[which(df$sex != "MD"),] # remove MD
df <- df[which((df$Group != 3) & (df$Group != 5) & (df$Group != 6)),]
if(filter) {
  # df <- df[which((df$Group == 0) | (df$Group == 1)),] # HC vs Dep
  # df <- df[which((df$Group == 0) | (df$Group == 4)),] # HC vs Anx
  # df <- df[which((df$Group == 0) | (df$Group == 2) | (df$Group == 4)),] # HC vs Anx or Anx+Dep
  df <- df[which((df$Group == 0) | (df$Group == 1) | (df$Group == 2)),] # HC vs Dep or Anx+Dep
  # df <- df[which((df$Group == 1) | (df$Group == 4)),] # Dep vs Anx
}

df <- df[,c(identifiers,dep_var, control_vars, behavioral_predictors)]
df[,-which(names(df) %in% c(identifiers,dep_var))] <- 
  sapply(df[,-which(names(df) %in% c(identifiers,dep_var))], as.numeric)

# Recode race as white/not (white=5)
df$race <- ifelse(df$race==5,1,0)
# Convert male/female to binary
df$sex <- as.numeric(as.character(df$sex))
df$sex <- as.factor(ifelse(df$sex==2,0,1))

# Stats on missing data
percent_missing <- function(x){sum(is.na(x))/length(x)*100}
percent_missing_cols <- data.frame()
percent_missing_rows <- round(data.frame(apply(df, 1, percent_missing)),1)
percent_missing_cols <- round(data.frame(apply(df, 2, percent_missing)),1)
# Remove missing data
df <- df[,which(percent_missing_cols < 20)]
df <- df[which(percent_missing_rows < 50),]

setwd(paste0(wd,"/Data/Behavioral/Prepared_Data/"))
write.csv(df,"dim_reduced_depressive_unique_behavioral.csv", row.names = FALSE, na = "")
