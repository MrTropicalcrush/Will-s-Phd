#### Running i-ARIMAX parallel ####
library(dplyr)
library(idionomics)
library(MTS)
library(parallel)

# Function to standardize the data and run i-ARIMAX with parallel processing
run_iarimax_on_simulated_data_parallel <- function(simulated_data_list_10, IV, imputeThese) {
  
  # Set up the number of cores (use all except one to avoid overloading)
  num_cores <- detectCores() - 4
  cl <- makeCluster(num_cores)
  
  # Export necessary libraries and objects to the cluster
  clusterEvalQ(cl, {
    library(dplyr)
    library(idionomics)
    library(MTS)
  })
  
  # Export variables and functions to each worker
  clusterExport(cl, c("simulated_data_list_10", "IV", "imputeThese", "i_standarbot_300", "IARIMAXoid_Pro"))
  
  # Run the parallel processing using parLapply
  all_iarimax_results <- parLapply(cl, seq_along(simulated_data_list_10), function(i) {
    
    current_data <- as.data.frame(simulated_data_list_10[[i]])
    
    # Within-person standardization
    zDatasim <- i_standarbot_300(current_data, imputeThese, "ID", explanation = TRUE)
    
    # Create a list to store i-ARIMAX results for each independent variable in the current dataset
    iarimax_results <- list()
    
    # Loop through each independent variable (IV)
    for (j in seq_along(IV)) {
      # Generate a model name
      model_name <- paste0("Sim_", i, "_IV_", j)
      
      # Run IARIMAXoid_Pro for each IV
      iarimax_results[[model_name]] <- IARIMAXoid_Pro(
        zDatasim,
        x_series = IV[[j]],  # Current IV
        y_series = "depressedmood_state_PSD",  # Dependent variable
        id_var = "ID",
        hlm_compare = FALSE,
        timevar = "time",
        metaanalysis = TRUE
      )
    }
    
    return(iarimax_results)
  })
  
  # Stop the cluster after processing
  stopCluster(cl)
  
  # Return the list of all results
  return(all_iarimax_results)
}

# List of independent variables
IV <- c("loneliness_state_pmc_PSD", "socintsatisfaction_state_pmc_PSD", "responsiveness_state_pmc_PSD", 
        "selfdisclosure_state_pmc_PSD","otherdisclosure_state_pmc_PSD")

# Variables to be imputed
imputeThese <- c("loneliness_state_pmc", "socintsatisfaction_state_pmc", 
                 "responsiveness_state_pmc", "depressedmood_state", 
                 "selfdisclosure_state_pmc", "otherdisclosure_state_pmc")

# Run the parallel function on the list of simulated datasets
iarimax_results <- run_iarimax_on_simulated_data_parallel(simulated_data_list_10, IV, imputeThese)

#### Extracting i-ARIMAX results ####
library(tidyr)

# Function to extract xreg coefficients for one variable in one simulation
extract_xreg_coefficients <- function(sim_results, variable_name) {
  tryCatch({
    # Extract xreg coefficients for the current variable
    xreg_coefficients <- sim_results$results_df$xreg
    return(data.frame(ID = sim_results$results_df$ID, variable = variable_name, xreg = xreg_coefficients))
  }, error = function(e) {
    message(paste("Error extracting xreg for variable:", variable_name))
    return(NULL)
  })
}
# Function to extract xreg coefficients across all simulations
extract_xreg_all_simulations <- function(iarimax_results_list, variable_names) {
  all_results <- list()
  
  # Loop through each simulation
  for (sim in seq_along(iarimax_results_list)) {
    sim_results <- iarimax_results_list[[sim]]
    
    # Loop through each variable and extract xreg coefficients
    sim_xregs <- lapply(seq_along(variable_names), function(j) {
      # Extract the xreg results for each variable
      variable_result <- sim_results[[paste0("Sim_", sim, "_IV_", j)]]
      if (!is.null(variable_result)) {
        xreg_data <- extract_xreg_coefficients(variable_result, variable_names[j])
        return(data.frame(simulation = sim, xreg_data))
      } else {
        return(NULL)
      }
    })
    
    # Combine results for this simulation
    sim_xregs_combined <- do.call(rbind, sim_xregs)
    if (!is.null(sim_xregs_combined)) {
      all_results[[sim]] <- sim_xregs_combined
    }
  }
  
  return(do.call(rbind, all_results))  # Combine all simulation results into one data frame
}

# Example variables (replace with your actual variable names)
variable_names <- c("loneliness", "socialintsatisfaction","responsiveness","selfdisclosure","otherdisclosure")

# Extract xreg coefficients across all simulations
xreg_coefficients_all <- extract_xreg_all_simulations(iarimax_results, variable_names)

# Calculate the average xreg coefficient for each individual and variable across all simulations
average_xreg_coefficients <- xreg_coefficients_all %>%
  group_by(ID, variable) %>%
  summarize(avg_xreg = mean(xreg, na.rm = TRUE), .groups = "drop")

# Reshape the data so that variables are columns and individuals are rows
average_xreg_coefficients_wide <- average_xreg_coefficients %>%
  pivot_wider(names_from = variable, values_from = avg_xreg)

# Ensure the ID column is ordered numerically
average_xreg_coefficients_wide <- average_xreg_coefficients_wide %>%
  arrange(as.numeric(ID))

average_xreg_coefficients_10 <- average_xreg_coefficients_wide

# Extracting t-statistic scores
library(tidyr)
library(dplyr)

# Function to calculate t-statistics for one variable in one simulation
calculate_t_statistics <- function(sim_results, variable_name) {
  tryCatch({
    # Extract xreg coefficients and standard errors
    xreg_coefficients <- sim_results$results_df$xreg
    standard_errors <- sim_results$results_df$stderr_xreg
    
    # Calculate t-statistics
    t_statistics <- xreg_coefficients / standard_errors
    
    # Return a dataframe with ID, variable, and t-statistic
    return(data.frame(
      ID = sim_results$results_df$ID,
      variable = variable_name,
      t_stat = t_statistics
    ))
  }, error = function(e) {
    message(paste("Error calculating t-statistics for variable:", variable_name))
    return(NULL)
  })
}

# Function to calculate t-statistics across all simulations
calculate_t_statistics_all_simulations <- function(iarimax_results_list, variable_names) {
  all_results <- list()
  
  # Loop through each simulation
  for (sim in seq_along(iarimax_results_list)) {
    sim_results <- iarimax_results_list[[sim]]
    
    # Loop through each variable and calculate t-statistics
    sim_t_stats <- lapply(seq_along(variable_names), function(j) {
      # Extract results for the current variable
      variable_result <- sim_results[[paste0("Sim_", sim, "_IV_", j)]]
      if (!is.null(variable_result)) {
        t_stat_data <- calculate_t_statistics(variable_result, variable_names[j])
        return(data.frame(simulation = sim, t_stat_data))
      } else {
        return(NULL)
      }
    })
    
    # Combine t-statistics for this simulation
    sim_t_stats_combined <- do.call(rbind, sim_t_stats)
    if (!is.null(sim_t_stats_combined)) {
      all_results[[sim]] <- sim_t_stats_combined
    }
  }
  
  return(do.call(rbind, all_results))  # Combine all simulation results into one data frame
}

# Example variables (replace with your actual variable names)
variable_names <- c("loneliness", "socialintsatisfaction", "responsiveness", "selfdisclosure", "otherdisclosure")

# Calculate t-statistics across all simulations
t_statistics_all_simulations <- calculate_t_statistics_all_simulations(iarimax_results, variable_names)

# Calculate the average t-statistic for each individual and variable across all simulations
average_t_statistics <- t_statistics_all_simulations %>%
  group_by(ID, variable) %>%
  summarize(avg_t_stat = mean(t_stat, na.rm = TRUE), .groups = "drop")

# Reshape the data so that variables are columns and individuals are rows
average_t_statistics_wide <- average_t_statistics %>%
  pivot_wider(names_from = variable, values_from = avg_t_stat)

# Ensure the ID column is ordered numerically
average_t_statistics_wide <- average_t_statistics_wide %>%
  arrange(as.numeric(ID))

# Save the final results
average_t_statistics_10 <- average_t_statistics_wide

