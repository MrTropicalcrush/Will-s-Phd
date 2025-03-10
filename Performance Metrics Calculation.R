#### Calculating Performance metrics ####
# Calculate simulated contemporaneous relationships using Sigma matrices 
# Extract the Sigma matrices
sigma_matrices <- lapply(VAR_fit_results, function(individual_data) {
  individual_data$sigma  # Adjust if the element name for sigma is different
})

# Calculate correlations based of sigma matrices
validate_correlations_range <- function(adjusted_sigma_list) {
  # Initialize an empty list to store data for each individual
  correlation_data <- list()
  
  for (id in names(adjusted_sigma_list)) {
    sigma_matrix <- adjusted_sigma_list[[id]]
    cor_matrix <- cov2cor(sigma_matrix)
    cor_values <- cor_matrix[1, 2:6]  # Correlations of variables 2-6 with variable 1
    
    # Store the correlations along with the ID in a list
    correlation_data[[id]] <- c(ID = id, cor_values)
  }
  
  # Combine all individual results into a data frame
  correlation_df <- do.call(rbind, correlation_data)
  correlation_df <- as.data.frame(correlation_df, stringsAsFactors = FALSE)
  
  # Convert the correlation columns to numeric
  correlation_df[-1] <- lapply(correlation_df[-1], as.numeric)
  
  # Rename the columns for clarity
  colnames(correlation_df) <- c("ID", paste0("Correlation_with_V", 2:6))
  
  return(correlation_df)
}

simulated_correlations <- validate_correlations_range(sigma_matrices)

# Rename columns
library(dplyr)
simulated_correlations <- simulated_correlations %>%
  rename(loneliness = Correlation_with_V2,
         socintsatisfaction = Correlation_with_V3,
         responsiveness = Correlation_with_V4,
         selfdisclosure = Correlation_with_V5,
         otherdisclosure = Correlation_with_V6)

#### Calculating i-ARIMAX performance metrics ####
# Rearrange so column align with simualuated correlations 
average_t_statistics_10 <- average_t_statistics_10[,c(1,2,6,4,5,3)]

# Function to calculate sensitivity, specificity, and precision for i-ARIMAX based on t-statistics
calculate_iarimax_performance_tstat <- function(expected_correlations, t_statistics, correlation_threshold = 0.2, tstat_threshold = 1.96) {
  
  # Check if inputs are the same length
  if (length(expected_correlations) != length(t_statistics)) {
    stop("Expected correlations and t-statistics must have the same length.")
  }
  
  # Define expected importance based on expected correlations
  expected_important <- ifelse(abs(expected_correlations) > correlation_threshold, 1, 0)
  
  # Convert t-statistics to absolute values
  abs_t_statistics <- abs(t_statistics)
  
  # Define detected importance based on t-statistics
  detected_important <- ifelse(abs_t_statistics > tstat_threshold, 1, 0)
  
  # Calculate True Positives, False Negatives, False Positives, and True Negatives
  TP <- sum(expected_important == 1 & detected_important == 1)  # True Positives
  FN <- sum(expected_important == 1 & detected_important == 0)  # False Negatives
  FP <- sum(expected_important == 0 & detected_important == 1)  # False Positives
  TN <- sum(expected_important == 0 & detected_important == 0)  # True Negatives
  
  # Calculate Sensitivity, Specificity, and Precision
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)       # Sensitivity: TP / (TP + FN)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)       # Specificity: TN / (TN + FP)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)         # Precision: TP / (TP + FP)
  
  # Calculate F1 Score
  f1_score <- ifelse((precision + sensitivity) > 0, 
                     2 * (precision * sensitivity) / (precision + sensitivity), 
                     NA)  # F1 Score: 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
  
  # Return a data frame with the results
  results <- data.frame(
    Metric = c("Sensitivity", "Specificity", "Precision", "F1 Score"),
    Value = c(sensitivity, specificity, precision, f1_score)
  )
  
  return(results)
}

# Example usage

# Threshold for expected importance (correlation threshold) and detected importance (t-statistic threshold)
correlation_threshold <- 0.2 # Absolute correlation > 0.2 is considered reliabe
tstat_threshold <- 1.96       # Absolute t-statistic > 1.96 is considered reliable

# Run the function with expected correlations and t-statistics
iarimax_tstat_performance <- calculate_iarimax_performance_tstat(
  expected_correlations = simulated_correlations[,-c(1)],  # Replace with your actual correlation data
  t_statistics = average_t_statistics_10[,-c(1)],           # Replace with your t-statistics data
  correlation_threshold = correlation_threshold,
  tstat_threshold = tstat_threshold
)

# Print results
print(iarimax_tstat_performance)

#### Calculating BORUTA performance ####
# Function to calculate sensitivity and specificity for BORUTA
calculate_boruta_performance <- function(expected_correlations, boruta_decisions, correlation_threshold = 0.3) {
  
  # Check if inputs are the same length
  if (length(expected_correlations) != length(boruta_decisions)) {
    stop("Expected correlations and BORUTA decisions must have the same length.")
  }
  
  # Define expected importance based on expected correlations
  expected_important <- ifelse(abs(expected_correlations) > correlation_threshold, 1, 0)
  
  # Define detected importance based on BORUTA decisions
  detected_important <- ifelse(boruta_decisions == 1, 1, 0)
  
  # Calculate True Positives, False Negatives, False Positives, and True Negatives
  TP <- sum(expected_important == 1 & detected_important == 1)  # True Positives
  FN <- sum(expected_important == 1 & detected_important == 0)  # False Negatives
  FP <- sum(expected_important == 0 & detected_important == 1)  # False Positives
  TN <- sum(expected_important == 0 & detected_important == 0)  # True Negatives
  
  # Calculate Sensitivity and Specificity
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)  # Sensitivity: TP / (TP + FN)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)  # Specificity: TN / (TN + FP)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)         # Precision: TP / (TP + FP)
  
  # Calculate F1 Score
  f1_score <- ifelse((precision + sensitivity) > 0, 
                     2 * (precision * sensitivity) / (precision + sensitivity), 
                     NA)  # F1 Score: 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
  
  # Return a data frame with the results
  results <- data.frame(
    Metric = c("Sensitivity", "Specificity", "Precision", "F1 Score"),
    Value = c(sensitivity, specificity, precision, f1_score)
  )
  
  return(results)
}

# Example usage
correlation_threshold <- 0.2 # Absolute correlation > 0.2 is considered reliable

# Run the function with expected correlations and BORUTA decisions
BORUTA_performance_results <- calculate_boruta_performance(
  expected_correlations = sigma_correlations[,-c(1)],  # Replace with your actual correlation data
  boruta_decisions = majority_decisions_confirmed[,-c(1)],       # Replace with BORUTA decisions data
  correlation_threshold = correlation_threshold
)

# Print results
print(BORUTA_performance_results)

#### Calculating GIMME performance ####
# Function to calculate sensitivity, specificity, and precision for GIMME
calculate_GIMME_performance <- function(expected_correlations, GIMME_decisions, correlation_threshold = 0.1) {
  
  # Check if inputs are the same length
  if (length(expected_correlations) != length(GIMME_decisions)) {
    stop("Expected correlations and GIMME decisions must have the same length.")
  }
  
  # Define expected importance based on expected correlations
  expected_important <- ifelse(abs(expected_correlations) > correlation_threshold, 1, 0)
  
  # Define detected importance based on GIMME decisions (value present = important)
  detected_important <- ifelse(!is.na(GIMME_decisions), 1, 0)
  
  # Calculate True Positives, False Negatives, False Positives, and True Negatives
  TP <- sum(expected_important == 1 & detected_important == 1)  # True Positives
  FN <- sum(expected_important == 1 & detected_important == 0)  # False Negatives
  FP <- sum(expected_important == 0 & detected_important == 1)  # False Positives
  TN <- sum(expected_important == 0 & detected_important == 0)  # True Negatives
  
  # Calculate Sensitivity, Specificity, and Precision
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)  # Sensitivity: TP / (TP + FN)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)  # Specificity: TN / (TN + FP)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)    # Precision: TP / (TP + FP)
  
  # Calculate F1 Score
  f1_score <- ifelse((precision + sensitivity) > 0, 
                     2 * (precision * sensitivity) / (precision + sensitivity), 
                     NA)  # F1 Score: 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
  
  # Return a data frame with the results
  results <- data.frame(
    Metric = c("Sensitivity", "Specificity", "Precision", "F1 Score"),
    Value = c(sensitivity, specificity, precision, f1_score)
  )
  
  return(results)
}

# Example usage
correlation_threshold <- 0.2 # Absolute correlation > 0.2 is considered important

# Create dataframe with only contemporaneous paths 
average_paths <- average_paths_per_individual[,c()] # PUT BASED ON WHICH COLUMNS REPRESENT CONTEMP VARIABLE PATHS

# Run the function with expected correlations and GIMME decisions
GIMME_results <- calculate_GIMME_performance(
  expected_correlations = sigma_correlations[,-c(1)],  # Replace with your actual correlation data
  GIMME_decisions = average_paths,      # Replace with GIMME decision data
  correlation_threshold = correlation_threshold
)

# Print results
print(GIMME_results)

#### indSEM results ####
# Function to calculate sensitivity, specificity, and precision for IndSEM
calculate_indsem_performance <- function(expected_correlations, indsem_decisions, correlation_threshold = 0.1) {
  
  # Check if inputs are the same length
  if (length(expected_correlations) != length(indsem_decisions)) {
    stop("Expected correlations and IndSEM decisions must have the same length.")
  }
  
  # Define expected importance based on expected correlations
  expected_important <- ifelse(abs(expected_correlations) > correlation_threshold, 1, 0)
  
  # Define detected importance based on IndSEM decisions (value present = important)
  detected_important <- ifelse(!is.na(indsem_decisions), 1, 0)
  
  # Calculate True Positives, False Negatives, False Positives, and True Negatives
  TP <- sum(expected_important == 1 & detected_important == 1)  # True Positives
  FN <- sum(expected_important == 1 & detected_important == 0)  # False Negatives
  FP <- sum(expected_important == 0 & detected_important == 1)  # False Positives
  TN <- sum(expected_important == 0 & detected_important == 0)  # True Negatives
  
  # Calculate Sensitivity, Specificity, and Precision
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)  # Sensitivity: TP / (TP + FN)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)  # Specificity: TN / (TN + FP)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)    # Precision: TP / (TP + FP)
  
  # Calculate F1 Score
  f1_score <- ifelse((precision + sensitivity) > 0, 
                     2 * (precision * sensitivity) / (precision + sensitivity), 
                     NA)  # F1 Score: 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
  
  # Return a data frame with the results
  results <- data.frame(
    Metric = c("Sensitivity", "Specificity", "Precision", "F1 Score"),
    Value = c(sensitivity, specificity, precision, f1_score)
  )
  
  return(results)
}

# Example usage
correlation_threshold <- 0.2 # Absolute correlation > 0.2 is considered important

# Create dataframe with only contemporaneous paths 
average_betas <- average_betas_per_individual[,c()] # PUT BASED ON WHICH COLUMNS REPRESENT CONTEMP VARIABLE PATHS

# Run the function with expected correlations and IndSEM decisions
indsem_results <- calculate_indsem_performance(
  expected_correlations = sigma_correlations[,-c(1)],  # Replace with your actual correlation data
  indsem_decisions = average_betas,      # Replace with IndSEM decision data
  correlation_threshold = correlation_threshold
)

# Print results
print(indsem_results)

