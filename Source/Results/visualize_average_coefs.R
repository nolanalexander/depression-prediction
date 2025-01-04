### Visualize the coefficients of the Elastic Net model

library(ggplot2)

# install.packages("ggplot2")

wd <-"/Users/nolanalexander/Desktop/Internship/College/Summer_2020/R/Depressive_Prediction"

# read in coefs df
setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Coefficients"))
coefs_df <- read.csv("repl_coefs.csv", row.names = 1, stringsAsFactors = FALSE)
setwd(paste0(wd,"/Data/Behavioral"))
abbr_df <- read.csv("abbreviation_variable_names.csv", stringsAsFactors = FALSE)

coef_means <- colMeans(coefs_df)
setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Confidence_Intervals/"))
ci95_summary <- read.csv("Imaging_Behavioral_Confidence_Interval_Summary.csv", row.names=1)
avg_num_parameters <- round(ci95_summary["Num_Parameters", "Mean"])
coef_means <- coef_means[order(abs(coef_means), decreasing = TRUE)]
coef_means <- coef_means[1:avg_num_parameters]

# sets up coefs for plotting
coefs_df_for_plotting <- data.frame(cbind(names(coef_means), coef_means, ifelse(coef_means > 0,1,0)), stringsAsFactors=FALSE)
colnames(coefs_df_for_plotting) <- c("predictor","coef_value","positive")
coefs_df_for_plotting$coef_value <- as.numeric(as.character(coefs_df_for_plotting$coef_value))
# coefs_df_for_plotting <- coefs_df_for_plotting[order(abs(coefs_df_for_plotting$coef_value), decreasing = TRUE),]
# coefs_df_for_plotting$abbreviation <- factor(coefs_df_for_plotting$predictor, levels=as.character(coefs_df_for_plotting$predictor))
# coefs_df_for_plotting <- coefs_df_for_plotting[-which(coefs_df_for_plotting$predictor=="X.Intercept."),] # remove intercept
coefs_df_for_plotting <- merge(coefs_df_for_plotting, abbr_df, by.x="predictor", by.y="Abbreviation", all.x=TRUE)
coefs_df_for_plotting[is.na(coefs_df_for_plotting$Variable_Name), "Variable_Name"] <- 
  coefs_df_for_plotting[is.na(coefs_df_for_plotting$Variable_Name), "predictor"]

# visualize coefficients with ggplot's box plot
setwd(paste0(wd,"/Results/Logistic_Regression/Behavioral/Coefficients"))
png(file = "Behavioral_Model_Coefficients_avg.png", width = 600, height = 600)
coef_plot <- ggplot() + 
  geom_bar(data = coefs_df_for_plotting, aes(x=reorder(Variable_Name, -abs(coef_value)), y=coef_value, fill=positive), stat = "identity") +
  scale_fill_manual(values = c("dodgerblue4","firebrick")) + 
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1), legend.position = "none",
        plot.title = element_text(hjust = 0.5), text = element_text(size=20)) +
  xlab("Predictor") + ylab("Coefficient Value") + ggtitle("Depressive Model Coefficients")
print(coef_plot)
dev.off()

