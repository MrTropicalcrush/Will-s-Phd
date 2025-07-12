#### Re-organizing data for BOURTA ####
# Function to convert simulations from a single dataframe to a list of individuals
convert_to_individual_list <- function(simulated_data_list) {
  
  # Initialize a list to store the converted data
  individual_simulation_list <- vector("list", length(simulated_data_list))
  
  # Loop through each simulation
  for (sim in seq_along(simulated_data_list)) {
    # Split the dataframe by 'ID' to create a list of individual dataframes
    individual_simulation_list[[sim]] <- split(simulated_data_list[[sim]], simulated_data_list[[sim]]$ID)
  }
  
  return(individual_simulation_list)
}

BORUTA_sim_repeat <- convert_to_individual_list(simulated_data_list)

# Function to reorder the list of individuals numerically
reorder_individual_list <- function(individual_simulation_list) {
  lapply(individual_simulation_list, function(sim_list) {
    # Reorder the list numerically by the names of the elements (IDs)
    sim_list[order(as.numeric(names(sim_list)))]
  })
}

# Apply the reordering function
BORUTA_sim_repeat <- reorder_individual_list(BORUTA_sim_repeat)

#### Running BORUTA using parallel cores ####
# Load necessary libraries
library(Boruta)
library(parallel)

# Function to apply BORUTA to each individual in simulations using parallel processing
apply_boruta_parallel <- function(BORUTA_sim_repeat) {
  
  # Set up the number of cores
  num_cores <- detectCores() - 4  # Use all cores except four
  cl <- makeCluster(num_cores)
  
  # Export libraries and variables to the cluster
  clusterEvalQ(cl, library(Boruta))
  clusterExport(cl, c("BORUTA_sim_repeat"))
  
  # Define the parallel function
  BORUTA_results_simulations <- parLapply(cl, seq_along(BORUTA_sim_repeat), function(sim_index) {
    
    # Access the current simulation
    current_simulation <- BORUTA_sim_repeat[[sim_index]]
    num_individuals <- length(current_simulation)
    
    # Initialize a list to store Boruta results for this simulation
    BORUTAsim <- vector("list", num_individuals)
    
    # Loop through each individual in the simulation
    for (i in seq_along(current_simulation)) {
      
      # Apply Boruta to the individual's data
      BORUTAsim[[i]] <- Boruta(
        depressedmood_state ~ loneliness_state_pmc + socintsatisfaction_state_pmc + responsiveness_state_pmc + selfdisclosure_state_pmc + otherdisclosure_state_pmc,  # Formula
        data = current_simulation[[i]], maxRuns = 500, doTrace = 3
      )
    }
    
    # Return Boruta results for this simulation
    return(BORUTAsim)
  })
  
  # Stop the cluster
  stopCluster(cl)
  
  # Return all Boruta results
  return(BORUTA_results_simulations)
}

BORUTA_results_all <- apply_boruta_parallel(BORUTA_sim_repeat)
