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

#### Extracting BORUTA results ####
generate_confirmed_results <- function(BORUTA_results_all) {
  # Initialize a list to store data frames for each simulation
  confirmed_results_simulations <- vector("list", length(BORUTA_results_all))
  
  # Loop through each simulation
  for (sim in seq_along(BORUTA_results_all)) {
    
    # Get the number of individuals in the current simulation
    num_individuals <- length(BORUTA_results_all[[sim]])
    
    # Initialize a data frame to store the results for all individuals in this simulation
    confirmed_results_individuals <- data.frame(
      ID = numeric(num_individuals),
      loneliness_state_pmc = numeric(num_individuals),
      socintsatisfaction_state_pmc = numeric(num_individuals),
      responsiveness_state_pmc = numeric(num_individuals),
      selfdisclosure_state_pmc = numeric(num_individuals),
      otherdisclosure_state_pmc = numeric(num_individuals)
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

# Example usage with BORUTA_results_all_50
BORUTA_Confirmed_all <- generate_confirmed_results(BORUTA_results_all)

# Calculate majority confirmed 
calculate_majority_decision_per_individual <- function(confirmed_results_simulations, threshold = 0.5) {
  # Get the number of simulations and individuals
  num_simulations <- length(confirmed_results_simulations)
  num_individuals <- nrow(confirmed_results_simulations[[1]])
  
  # Initialize a data frame to store majority decisions
  majority_decisions <- data.frame(
    ID = confirmed_results_simulations[[1]]$ID  # Assuming IDs are the same across simulations
  )
  
  # Loop through each variable (excluding ID)
  variable_names <- colnames(confirmed_results_simulations[[1]])[-1]  # Exclude ID column
  
  for (variable in variable_names) {
    # Collect the binary decisions for the variable across all simulations
    decisions_matrix <- sapply(confirmed_results_simulations, function(sim) sim[[variable]])
    
    # Calculate the proportion of simulations where the variable was confirmed (1) for each individual
    proportion_confirmed <- rowMeans(decisions_matrix)
    
    # Determine the majority decision (1 = Confirmed if proportion ≥ threshold)
    majority_decisions[[variable]] <- ifelse(proportion_confirmed >= threshold, 1, 0)
  }
  
  return(majority_decisions)
}

# Example usage
# Assuming `BORUTA_Confirmed_all_70time` contains results for 100 simulations
majority_decisions_confirmed <- calculate_majority_decision_per_individual(BORUTA_Confirmed_all)

# Calculate MeanIMP 
# Function to process Boruta results and calculate average meanImp
process_boruta_results <- function(BORUTA_results_all) {
  
  # Initialize a list to store the extracted meanImp for all individuals across simulations
  meanImp_list <- list()
  
  # Loop over simulations
  for (sim_index in seq_along(BORUTA_results_all)) {
    simulation_results <- BORUTA_results_all[[sim_index]]
    
    # Loop over individuals in the simulation
    for (ind_index in seq_along(simulation_results)) {
      
      # Safely extract attStats for the individual's BORUTA result
      att_stats <- tryCatch(attStats(simulation_results[[ind_index]]), error = function(e) NULL)
      
      # Proceed only if attStats is valid and contains meanImp
      if (!is.null(att_stats) && !is.null(att_stats$meanImp) && length(att_stats$meanImp) > 0) {
        meanImp <- att_stats$meanImp
        
        # Create a temporary data frame to store the individual and meanImp values
        temp_df <- data.frame(
          Individual_ID = ind_index,                # ID for the individual
          Simulation_ID = sim_index,               # ID for the simulation
          t(as.data.frame(meanImp, stringsAsFactors = FALSE)) # Variables as columns
        )
        
        # Append the results to the list
        meanImp_list <- append(meanImp_list, list(temp_df))
      }
    }
  }
  
  # Combine all individual meanImp data into a single data frame
  if (length(meanImp_list) > 0) {
    all_results <- do.call(rbind, meanImp_list)
  } else {
    stop("No valid meanImp data extracted. Check input BORUTA results.")
  }
  
  # Calculate the average meanImp for each individual across simulations
  library(dplyr)
  average_results <- all_results %>%
    group_by(Individual_ID) %>%
    summarise(across(-Simulation_ID, mean, na.rm = TRUE)) %>%  # Average meanImp for each variable
    ungroup()
  
  return(average_results)
}

# Apply the function to your Boruta results
BORUTA_meanIMP <- process_boruta_results(BORUTA_results_all)

# Rename the columns using dplyr
BORUTA_meanIMP <- BORUTA_meanIMP %>%
  rename(otherdisclosure_meanIMP = X1,
         selfdisclosure_meanIMP = X2,
         responsiveness_meanIMP = X3,
         socintsatisfaction_meanIMP = X4,
         loneliness_meanIMP = X5) 

