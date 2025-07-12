#### Running Time series Boruta on simulated data ####
# First fit an ARIMA model and extract residuals
# Load the parallel package
library(parallel)

# Set up the number of cores (use all available cores except a few for safety)
num_cores <- detectCores() - 4
cl <- makeCluster(num_cores)

# Export libraries and required variables to the cluster
clusterEvalQ(cl, library(forecast))
clusterExport(cl, varlist = c("simulated_data"), envir = environment())

# Run in parallel across simulations
residual_70time <- parLapply(cl, 1:length(simulated_data), function(dataset_index) {
  
  # Initialize list to store residuals per simulation
  residuals_list <- list()
  
  # Extract the current dataset (a combined dataframe of 102 individuals)
  dataset <- simulated_data[[dataset_index]]
  
  # For each individual in the dataset
  for (id in unique(dataset$ID)) {
    
    individual_data <- subset(dataset, ID == id)
    
    # Skip if no rows
    if (nrow(individual_data) == 0) next
    
    # Prepare container for this individual's residuals
    individual_residuals <- data.frame(
      ID = rep(id, nrow(individual_data)),
      Time = individual_data$time
    )
    
    # Variables to include (exclude meta columns)
    vars_to_model <- setdiff(colnames(individual_data), c("ID", "time"))
    
    # Loop through each selected variable and fit ARIMA
    for (var in vars_to_model) {
      series <- individual_data[[var]]
      
      # Check for constant or NA series
      if (length(unique(na.omit(series))) <= 1) {
        individual_residuals[[var]] <- rep(NA, nrow(individual_data))
        next
      }
      
      # Fit ARIMA model
      fit <- tryCatch(auto.arima(series), error = function(e) NULL)
      
      # Store residuals or NAs if model failed
      if (!is.null(fit)) {
        resids <- residuals(fit)
        # Pad residuals if shorter (ARIMA can return 1 fewer obs due to differencing)
        if (length(resids) < nrow(individual_data)) {
          resids <- c(resids, rep(NA, nrow(individual_data) - length(resids)))
        }
        individual_residuals[[var]] <- resids
      } else {
        individual_residuals[[var]] <- rep(NA, nrow(individual_data))
      }
    }
    
    # Store this individual’s residuals in the list
    residuals_list[[as.character(id)]] <- individual_residuals
  }
  
  # Return residuals for the entire simulation
  return(residuals_list)
})

# Stop the parallel cluster
stopCluster(cl)

#### Running BORUTA on residual data ####
library(Boruta)
library(parallel)
# Function to apply BORUTA to each individual in simulations using parallel processing
apply_boruta_parallel <- function(residual_70time) {
  
  # Set up the number of cores
  num_cores <- detectCores() - 4  # Use all cores except four
  cl <- makeCluster(num_cores)
  
  # Export libraries and variables to the cluster
  clusterEvalQ(cl, library(Boruta))
  clusterExport(cl, c("residual_70time"))
  
  # Define the parallel function
  BORUTA_results_simulations <- parLapply(cl, seq_along(residual_70time), function(sim_index) {
    
    # Access the current simulation
    current_simulation <- residual_70time[[sim_index]]
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
      if (is.data.frame(individual_data) && all(c("depressedmood_state", "loneliness_state_pmc", "socintsatisfaction_state_pmc", "responsiveness_state_pmc",
                                                  "selfdisclosure_state_pmc", "otherdisclosure_state_pmc") %in% colnames(individual_data))) {
        
        # Handle missing values (optional: remove rows with NA, or use imputation)
        individual_data <- na.omit(individual_data)  # Remove rows with missing data
        
        # Apply Boruta to the individual's data
        BORUTAsim[[i]] <- Boruta(
          depressedmood_state ~ loneliness_state_pmc + socintsatisfaction_state_pmc + responsiveness_state_pmc + selfdisclosure_state_pmc +
            otherdisclosure_state_pmc,  # Formula
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

# Run function
tsBoruta_results <- apply_boruta_parallel(residual_70time)