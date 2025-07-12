###########################################################
### Change size of autocorrelation of outcome variable ####
###########################################################
#### Changing autocorrelations ####
# Changing only depressedmood_state autocorrelation
# Extract phi matrices
phi_matrices <- lapply(VAR_fit_results, function(individual_data) {
  individual_data$phi  
})

#### Manipulating Phi ####
# Small autocorrelation size
adjust_phi_matrices_for_variable1 <- function(phi_list, autocorrelation_range = c(0.1,0.3)) {
  adjusted_phi_list <- list()
  
  for (id in names(phi_list)) {
    # Extract the current Phi matrix
    phi_matrix <- phi_list[[id]]
    
    # Ensure the matrix is valid
    if (!is.matrix(phi_matrix) || ncol(phi_matrix) < 1) {
      message(paste("Skipping individual", id, "due to invalid Phi matrix"))
      next
    }
    
    # Randomly select a new autocorrelation within the specified range
    new_autocorrelation <- runif(1, autocorrelation_range[1], autocorrelation_range[2])
    
    # Adjust the autocorrelation for variable 1
    phi_matrix[1, 1] <- new_autocorrelation
    
    # Store the adjusted Phi matrix
    adjusted_phi_list[[id]] <- phi_matrix
  }
  
  return(adjusted_phi_list)
}

# Adjust autocorrelations for variable 1 in Phi matrices
adjusted_phi_matrices_depressedlow <- adjust_phi_matrices_for_variable1(phi_matrices, autocorrelation_range = c(0.1, 0.3))

print(adjusted_phi_matrices_depressedlow)

###########################################
# Large autocorellation size
adjust_phi_matrices_all_autocorrelations <- function(phi_list, autocorrelation_range = c(0.7, 0.9)) {
  adjusted_phi_list <- list()
  
  for (id in names(phi_list)) {
    # Extract the current Phi matrix
    phi_matrix <- phi_list[[id]]
    
    # Ensure the matrix is valid
    if (!is.matrix(phi_matrix)) {
      message(paste("Skipping individual", id, "due to invalid Phi matrix"))
      next
    }
    
    # Loop through each diagonal element (autocorrelation) and adjust it
    for (var in 1:ncol(phi_matrix)) {
      # Randomly select a new autocorrelation within the specified range
      new_autocorrelation <- runif(1, autocorrelation_range[1], autocorrelation_range[2])
      
      # Update the diagonal element
      phi_matrix[var, var] <- new_autocorrelation
    }
    
    # Store the adjusted Phi matrix
    adjusted_phi_list[[id]] <- phi_matrix
  }
  
  return(adjusted_phi_list)
}

# Adjust autocorrelations for all variables in Phi matrices
adjusted_phi_matrices_depressedhigh <- adjust_phi_matrices_all_autocorrelations(phi_matrices, autocorrelation_range = c(0.7, 0.9))

print(adjusted_phi_matrices_depressedhigh)

###################################################################################
# Medium autocorrelation size
adjust_phi_matrices_all_autocorrelations <- function(phi_list, autocorrelation_range = c(0.4, 0.6)) {
  adjusted_phi_list <- list()
  
  for (id in names(phi_list)) {
    # Extract the current Phi matrix
    phi_matrix <- phi_list[[id]]
    
    # Ensure the matrix is valid
    if (!is.matrix(phi_matrix)) {
      message(paste("Skipping individual", id, "due to invalid Phi matrix"))
      next
    }
    
    # Loop through each diagonal element (autocorrelation) and adjust it
    for (var in 1:ncol(phi_matrix)) {
      # Randomly select a new autocorrelation within the specified range
      new_autocorrelation <- runif(1, autocorrelation_range[1], autocorrelation_range[2])
      
      # Update the diagonal element
      phi_matrix[var, var] <- new_autocorrelation
    }
    
    # Store the adjusted Phi matrix
    adjusted_phi_list[[id]] <- phi_matrix
  }
  
  return(adjusted_phi_list)
}

# Adjust autocorrelations for all variables in Phi matrices
adjusted_phi_matrices_depressedmedium <- adjust_phi_matrices_all_autocorrelations(phi_matrices, autocorrelation_range = c(0.4, 0.6))

print(adjusted_phi_matrices_depressedmedium)

# Simulate using Simulating data.R (change VAR_fit_results to combined_matrices_depressedlow)
# See other Github R code files for simulation code file

####################################################################################################################
#### Adding linear trend to outcome ####
# Function to add a trend to a variable
add_trend_to_variable <- function(data, variable_name, trend_slope_range) {
  # Ensure the variable exists in the dataset
  if (!variable_name %in% colnames(data)) {
    stop(paste("Variable", variable_name, "not found in dataset."))
  }
  
  # Get the number of time points
  n_time <- nrow(data)
  
  # Randomly select a trend slope within the specified range
  trend_slope <- runif(1, trend_slope_range[1], trend_slope_range[2])
  
  # Create the linear trend
  linear_trend <- seq(0, trend_slope * (n_time - 1), length.out = n_time)
  
  # Add the trend to the specified variable
  data[[variable_name]] <- data[[variable_name]] + linear_trend
  
  return(data)
}

# Define the range for the trend slope
trend_slope_range <- c(0.7, 0.9)

# Loop through each simulation in the list and apply the trend
add_trend_to_simulations <- function(simulation_list, variable_name, trend_slope_range) {
  # Apply the trend to each simulation
  updated_simulations <- lapply(simulation_list, function(sim_data) {
    add_trend_to_variable(sim_data, variable_name, trend_slope_range)
  })
  
  return(updated_simulations)
}

# Apply the trend to all simulations
simulated_100_VAR_datasets_trendlarge <- add_trend_to_simulations(
  simulation_list = simulated_100_VAR_datasets_70time,
  variable_name = "depressedmood_state",
  trend_slope_range = trend_slope_range
)
