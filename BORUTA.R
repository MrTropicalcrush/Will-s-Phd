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

# Assuming simulated_data_list_repeat_50 is your list of simulations (with all individuals in one dataframe per simulation)
BORUTA_sim_repeat10 <- convert_to_individual_list(simulated_data_list_10)

# Function to reorder the list of individuals numerically
reorder_individual_list <- function(individual_simulation_list) {
  lapply(individual_simulation_list, function(sim_list) {
    # Reorder the list numerically by the names of the elements (IDs)
    sim_list[order(as.numeric(names(sim_list)))]
  })
}

# Apply the reordering function to BORUTA_sim_repeat_70
BORUTA_sim_repeat10 <- reorder_individual_list(BORUTA_sim_repeat10)

#### Running BORUTA using parallel cores ####
# Load necessary libraries
library(Boruta)
library(parallel)

# Function to apply BORUTA to each individual in simulations using parallel processing
apply_boruta_parallel <- function(BORUTA_sim_repeat10) {
  
  # Set up the number of cores
  num_cores <- detectCores() - 4  # Use all cores except four
  cl <- makeCluster(num_cores)
  
  # Export libraries and variables to the cluster
  clusterEvalQ(cl, library(Boruta))
  clusterExport(cl, c("BORUTA_sim_repeat10"))
  
  # Define the parallel function
  BORUTA_results_simulations <- parLapply(cl, seq_along(BORUTA_sim_repeat10), function(sim_index) {
    
    # Access the current simulation
    current_simulation <- BORUTA_sim_repeat10[[sim_index]]
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

# Assuming BORUTA_sim_repeat_70 is already created using convert_to_individual_list
BORUTA_results_all <- apply_boruta_parallel(BORUTA_sim_repeat10)
