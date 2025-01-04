## Overview
This project contains the R code for the paper "Personality traits as predictors of depression across the lifespan” published in the [Journal of Affective Disorders](https://doi.org/10.1016/j.jad.2024.03.073).

## Publications
1. [Personality traits as predictors of depression across the lifespan](https://scholar.google.com/citations?view_op=view_citation&hl=en&user=vm_4mhoAAAAJ&authuser=1&citation_for_view=vm_4mhoAAAAJ:UeHWp8X0CEIC)

## Files

1) **prepare_behavioral_data.R**
    - Location: Source/Cleaning
    - Purpose: Remove patients and features from the dataset.  Creates the dim_reduced_depressive_unique_behavioral.csv file.

2) **elastic_net_logistic_regression_behavioral.R**
    - Location: Source/Multivariate_Analysis/
    - Purpose: Runs 20 imputations of 1000 replications of an elastic net model and pools the results.  Saves runs as elastic_net_logistic_regression_behavioral_runs.csv and elastic_net_logistic_regression_behavioral_runs_uncertainty.csv.  Takes a couple hours to run since it is doing 20,000 runs total (so our confidence intervals are accurate and stable).

3) **permutation_elastic_net_logistic_regression_behavioral.R**
    - Location: Source/Multivariate_Analysis/
    - Purpose: Runs a permutations model, same code as elastic net, but now sets the dependent variable to random binary.  Saves runs as permutations_elastic_net_logistic_regression_behavioral_runs.csv and permutations_elastic_net_logistic_regression_behavioral_runs_uncertainty.csv.

4) **hypothesis_test_all_imaging.R**
    - Location: Source/Multivariate_Analysis/
    - Purpose: Runs t-tests for imaging data

5) **chance_level_testing.R**
    - Location: Source/Chance_Level/
    - Runs a t-test on the model and the permutations model accuracies.  Displays results to console.

6) **confidence_intervals.R**
    - Location: Source/Results/
    - Purpose: Creates confidence intervals of the model metrics for all runs, saved as Behavioral_Confidence_Interval_Summary.csv in the Confidence_Intervals folder.

7) **visualize_average_coefs.R**
    - Location: Results/
    - Purpose: Creates a bar graph of the coefficient magnitudes, saved as Behavioral_Model_Coefficients_avg.png in the Coefficients folder.
