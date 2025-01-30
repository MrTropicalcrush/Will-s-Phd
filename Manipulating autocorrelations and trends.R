#### Adding autocorrelations ####
# Changing only depressedmood_state autocorrelation
# Extract phi matrices
phi_matrices <- lapply(VAR_fit_results, function(individual_data) {
  individual_data$phi  
})

# Function to adjust phi matrices for each individual
adjust_phi_matrices_for_variable1 <- function(phi_list, autocorrelation_range = c(0.4,0.8)) {
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
adjusted_phi_matrices_depressedlow <- adjust_phi_matrices_for_variable1(phi_matrices, autocorrelation_range = c(0, 0.2))

# Combine with sigma matrices from real data 
combined_matrices_depressedlow <- lapply(names(adjusted_phi_matrices_depressedlow), function(id) {
  list(
    phi = adjusted_phi_matrices_depressedlow[[id]],
    sigma = sigma_matrices[[id]]
  )
})

# Simulate using Simulating data.R (change VAR_fit_results to combined_matrices_depressedlow)
# See other Github R code files for simulation code file

#### Adding trends to the data ####
