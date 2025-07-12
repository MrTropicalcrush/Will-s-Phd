#### Running i-ARIMAX and MV i-ARIMAX on simulated data ####

# Running i-ARIMAX parallel
library(dplyr)
library(idionomics)
library(MTS)
library(parallel)

# Function to standardize the data and run i-ARIMAX with parallel processing
run_iarimax_on_simulated_data_parallel <- function(simulated_data_list, IV, imputeThese) {
  
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
  clusterExport(cl, c("simulated_data_list", "IV", "imputeThese", "i_standarbot_300", "IARIMAXoid_Pro"))
  
  # Run the parallel processing using parLapply
  all_iarimax_results <- parLapply(cl, seq_along(simulated_data_list), function(i) {
    
    current_data <- as.data.frame(simulated_data_list[[i]])
    
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
iarimax_results <- run_iarimax_on_simulated_data_parallel(simulated_data_list, IV, imputeThese)

#####################################################################################################
#### Running MV i-ARIMAX on simualted data ####
run_simulated_arimax_parallel <- function(simulated_data_list, impute_vars, id_col = "ID") {
  # Load required packages
  library(parallel)
  library(forecast)
  library(idionomics)  # Ensure this package is installed and contains i_standarbot_300
  
  # Set up the number of cores (adjust as needed)
  num_cores <- detectCores() - 4
  cl <- makeCluster(num_cores)
  
  # Export required objects and functions to the cluster nodes
  clusterExport(cl, varlist = c("i_standarbot_300", "impute_vars", "id_col"), envir = environment())
  
  # Ensure the necessary libraries are loaded on each worker
  clusterEvalQ(cl, {
    library(forecast)
    library(idionomics)
  })
  
  # Process each simulation in parallel
  simulation_results <- parLapply(cl, simulated_data_list, function(sim_data) {
    # Split the simulation data by individual ID
    individual_list <- split(sim_data, sim_data[[id_col]])
    
    # Optionally, order individuals numerically by their ID
    individual_list <- individual_list[order(as.numeric(names(individual_list)))]
    
    # For each individual, run the ARIMAX model
    arimax_results <- lapply(individual_list, function(ind_data) {
      # Standardize the individual's data using i_standarbot_300
      standardized_data <- i_standarbot_300(ind_data, impute_vars, id_col, explanation = TRUE)
      
      # Convert the dependent variable to a time series object
      standardized_data$depressedmood_state_PSD <- ts(standardized_data$depressedmood_state_PSD, frequency = 1)
      
      # Set up the predictor matrix (xreg)
      xreg <- standardized_data[, c("loneliness_state_pmc_PSD", 
                                    "socintsatisfaction_state_pmc_PSD", 
                                    "responsiveness_state_pmc_PSD",
                                    "selfdisclosure_state_pmc_PSD",
                                    "otherdisclosure_state_pmc_PSD")]
      xreg <- as.matrix(xreg)
      
      # Run the ARIMAX model using auto.arima from the forecast package
      model <- auto.arima(standardized_data$depressedmood_state_PSD, xreg = xreg)
      
      # Return the fitted model
      return(model)
    })
    
    # Return the list of ARIMAX model objects for this simulation
    return(arimax_results)
  })
  
  # Stop the cluster
  stopCluster(cl)
  
  return(simulation_results)
}

# Example usage:
# simulated_data_list should be a list of 100 simulated datasets,
# each dataset being a data frame with (for example) 102 individuals identified by "ID".
imputeThese <- c("loneliness_state_pmc", "socintsatisfaction_state_pmc", 
                 "responsiveness_state_pmc", "depressedmood_state","selfdisclosure_state_pmc",
                 "otherdisclosure_state_pmc")

# Run the function on your simulated data list
MVarimax_results <- run_simulated_arimax_parallel(simulated_data_list, imputeThese, id_col = "ID")

