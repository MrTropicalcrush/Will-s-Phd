#### Calculating Performance Metrics BORUTA/tsBoruta ####
library(dplyr)
library(tidyr)
library(Boruta)

# Extract confirmed results
generate_confirmed_results <- function(BORUTA_results_all) {
  # Initialize a list to store data frames for each simulation
  confirmed_results_simulations <- vector("list", length(BORUTA_results_all))
  
  # Loop through each simulation
  for (sim in seq_along(BORUTA_results_all)) {
    
    # Get the number of individuals in the current simulation
    num_individuals <- length(BORUTA_results_all[[sim]])
    
    # Initialize a data frame to store the results for all individuals in this simulation
    confirmed_results_individuals <- data.frame(
      ID = numeric(num_individuals)
    )
    
    # Loop through each individual in the simulation
    for (i in seq_along(BORUTA_results_all[[sim]])) {
      # Add individual ID (assuming it is present in the data)
      confirmed_results_individuals$ID[i] <- i  # Replace this with actual IDs if available
      
      # Check and record the final decision for each predictor
      confirmed_results_individuals$loneliness_state_pmc[i] <- ifelse(
        BORUTA_results_all[[sim]][[i]]$finalDecision["loneliness_state_pmc"] == "Confirmed", 1, 0)
      
      confirmed_results_individuals$socintsatisfaction_state_pmc[i] <- ifelse(
        BORUTA_results_all[[sim]][[i]]$finalDecision["socintsatisfaction_state_pmc"] == "Confirmed", 1, 0)
      
      confirmed_results_individuals$responsiveness_state_pmc[i] <- ifelse(
        BORUTA_results_all[[sim]][[i]]$finalDecision["responsiveness_state_pmc"] == "Confirmed", 1, 0)
      
      confirmed_results_individuals$selfdisclosure_state_pmc[i] <- ifelse(
        BORUTA_results_all[[sim]][[i]]$finalDecision["selfdisclosure_state_pmc"] == "Confirmed", 1, 0)
      
      confirmed_results_individuals$otherdisclosure_state_pmc[i] <- ifelse(
        BORUTA_results_all[[sim]][[i]]$finalDecision["otherdisclosure_state_pmc"] == "Confirmed", 1, 0)
      
    }
    
    # Store the results for this simulation
    confirmed_results_simulations[[sim]] <- confirmed_results_individuals
  }
  
  # Return the list of data frames
  return(confirmed_results_simulations)
}

# Example usage with BORUTA_results_all_70
BORUTA_Confirmed_all <- generate_confirmed_results(BORUTA_results_all_70)

# New column names (adjust to match your variables)
new_colnames <- c("ID", "loneliness","socintsatisfaction","responsiveness","selfdisclosure","otherdisclosure")

# Apply renaming across the list
BORUTA_Confirmed_all <- lapply(BORUTA_Confirmed_all, function(df) {
  colnames(df) <- new_colnames
  return(df)
})

# Function to calculate performance metrics
calculate_boruta_performance <- function(expected_correlations, boruta_decisions, correlation_threshold = 0.2) {
  
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

# Running function across all simulations
calculate_boruta_performance_all_sims <- function(boruta_confirmed_list, expected_correlation_df, corr_threshold = 0.2) {
  results_list <- list()
  
  # Identify variable names (exclude ID column)
  variable_names <- setdiff(colnames(expected_correlation_df), "ID")
  
  for (sim in seq_along(boruta_confirmed_list)) {
    sim_data <- boruta_confirmed_list[[sim]] %>%
      select(all_of(c("ID", variable_names)))
    
    expected_corrs <- expected_correlation_df %>%
      select(all_of(c("ID", variable_names)))
    
    # Flatten both to numeric vectors
    boruta_vec <- as.numeric(as.matrix(sim_data %>% select(-ID)))
    cor_vec <- as.numeric(as.matrix(expected_corrs %>% select(-ID)))
    
    # Calculate performance
    perf <- calculate_boruta_performance(
      expected_correlations = cor_vec,
      boruta_decisions = boruta_vec,
      correlation_threshold = corr_threshold
    )
    
    # Transpose and name columns by metric
    perf_transposed <- as.data.frame(t(perf$Value))
    colnames(perf_transposed) <- perf$Metric
    perf_transposed$simulation <- sim
    
    results_list[[sim]] <- perf_transposed
  }
  
  # Combine all simulation results into a single data frame
  performance_df <- dplyr::bind_rows(results_list)
  return(performance_df)
}

# Example usage
boruta_performance_per_sim <- calculate_boruta_performance_all_sims(
  boruta_confirmed_list = BORUTA_Confirmed_all,
  expected_correlation_df = sigmacorrelations,
  corr_threshold = 0.2
)

# Averaging across simulations
average_results <- boruta_performance_per_sim %>%
  select(-simulation) %>%        # Remove the simulation column
  summarise(across(everything(), mean, na.rm = TRUE))  # Compute average for each metric


print(average_results)

# Calculating 95% confidence interval
# 1. Remove the 'simulation' column to focus on metric columns
metrics_only <- boruta_performance_per_sim %>% select(-simulation)

# 2. Calculate mean and 95% confidence intervals
average_metrics <- metrics_only %>%
  summarise(across(everything(), list(
    mean = ~mean(.),
    lower_CI = ~mean(.) - 1.96 * sd(.) / sqrt(n()),
    upper_CI = ~mean(.) + 1.96 * sd(.) / sqrt(n())
  )))

# 3. Optional: reshape to clearer format
# Combine results into a long-format summary
average_metrics_long <- average_metrics %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Metric", ".value"),
    names_sep = "_"
  )

# 4. View the result
print(average_metrics_long)