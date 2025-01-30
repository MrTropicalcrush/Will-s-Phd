#### Time series BORUTA ####
library(forecast)

# Initialize an empty list to store residuals for all simulations
all_residuals_list <- list()

# Loop through each simulated dataset in the list (simulated_data_list_10)
for (dataset_index in 1:length(simulated_data_list_10)) {
  
  # Initialize an empty list to store residuals for this dataset (simulation)
  residuals_list <- list()
  
  # Loop through each individual (based on 'ID') in the current dataset
  for (id in unique(simulated_data_list_10[[dataset_index]]$ID)) {
    
    # Filter the data for the current individual
    individual_data <- subset(simulated_data_list_10[[dataset_index]], ID == id)
    
    # Empty data frame to store residuals for this individual
    individual_residuals <- data.frame(ID = rep(id, nrow(individual_data)), time = individual_data$time)
    
    # Loop through each variable (excluding 'ID' and 'time')
    for (var in colnames(individual_data)[!colnames(individual_data) %in% c("ID", "time")]) {
      # Fit ARIMA model (without xreg)
      arima_model <- auto.arima(individual_data[[var]])
      
      # Extract the residuals
      residuals <- residuals(arima_model)
      
      # Add the residuals as a new column to the individual_residuals data frame
      individual_residuals[[var]] <- residuals
    }
    
    # Add the individual residuals to the residuals_list for the current dataset
    residuals_list[[id]] <- individual_residuals
  }
  
  # Add the residuals for this dataset (simulation) to the overall list
  all_residuals_list[[dataset_index]] <- residuals_list
}

#### Running BORUTA on residual data ####
# Function to apply BORUTA to each individual in simulations using parallel processing
apply_boruta_parallel <- function(all_residuals_list) {
  
  # Set up the number of cores
  num_cores <- detectCores() - 4  # Use all cores except four
  cl <- makeCluster(num_cores)
  
  # Export libraries and variables to the cluster
  clusterEvalQ(cl, library(Boruta))
  clusterExport(cl, c("all_residuals_list"))
  
  # Define the parallel function
  BORUTA_results_simulations <- parLapply(cl, seq_along(all_residuals_list), function(sim_index) {
    
    # Access the current simulation
    current_simulation <- all_residuals_list[[sim_index]]
    num_individuals <- length(current_simulation)
    
    # Initialize a list to store Boruta results for this simulation
    BORUTAsim <- vector("list", num_individuals)
    
    # Loop through each individual in the simulation
    for (i in seq_along(current_simulation)) {
      
      # Check if the individual data frame has the required columns
      individual_data <- current_simulation[[i]]
      
      # Convert to data frame if it's a ts object
      if (inherits(individual_data, "ts")) {
        individual_data <- as.data.frame(individual_data)
        # If the ts object has no column names, add generic names
        colnames(individual_data) <- paste("V", 1:ncol(individual_data), sep = "")
      }
      
      # Ensure it's a data frame and has the required columns
      if (is.data.frame(individual_data) && all(c("depressedmood_state", "loneliness_state_pmc", 
                                                  "socintsatisfaction_state_pmc", "responsiveness_state_pmc", 
                                                  "selfdisclosure_state_pmc", "otherdisclosure_state_pmc") %in% colnames(individual_data))) {
        
        # Handle missing values (optional: remove rows with NA, or use imputation)
        individual_data <- na.omit(individual_data)  # Remove rows with missing data
        
        # Apply Boruta to the individual's data
        BORUTAsim[[i]] <- Boruta(
          depressedmood_state ~ loneliness_state_pmc + socintsatisfaction_state_pmc + responsiveness_state_pmc + 
            selfdisclosure_state_pmc + otherdisclosure_state_pmc,  # Formula
          data = individual_data, maxRuns = 500, doTrace = 3
        )
        
      } else {
        # Handle case where required columns are missing or it's not a data frame
        warning(paste("Skipping individual", i, "due to missing required columns or invalid data frame."))
        BORUTAsim[[i]] <- NULL
      }
    }
    
    # Return Boruta results for this simulation
    return(BORUTAsim)
  })
  
  # Stop the cluster
  stopCluster(cl)
  
  # Return all Boruta results
  return(BORUTA_results_simulations)
}

# Assuming BORUTA_sim_repeat_70 is already created using convert_to_individual_list
BORUTA_results_residuals <- apply_boruta_parallel(all_residuals_list)
