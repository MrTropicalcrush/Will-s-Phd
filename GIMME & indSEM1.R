### Setting up data for GIMME/indSEM ###
# Function to restructure the data 
transform_to_individual_lists <- function(simulations_list) {
  # Create a new list to store the transformed structure
  transformed_list <- lapply(simulations_list, function(simulation_data) {
    # Split the simulation dataframe into a list by individual ID
    split(simulation_data, simulation_data$ID)
  })
  
  return(transformed_list)
}

transformed_data <- transform_to_individual_lists(simulated_data_list)

# Function to reorder individuals numerically within each simulation
reorder_individuals <- function(simulation_list) {
  # Reorder the list by numerically sorting the names (IDs)
  simulation_list[order(as.numeric(names(simulation_list)))]
}

# Apply the function to all simulations in the main list
ordered_transformed_data <- lapply(transformed_data, reorder_individuals)

# Function to remove specific columns for every individual in every simulation
remove_columns_from_simulations <- function(simulations_list, columns_to_remove) {
  lapply(simulations_list, function(simulation) {
    lapply(simulation, function(individual_data) {
      individual_data[, !colnames(individual_data) %in% columns_to_remove, drop = FALSE]
    })
  })
}

# Specify the columns you want to remove
columns_to_remove <- c("ID", "time")

# Apply the function to the list of simulations
GIMME_simdatalist <- remove_columns_from_simulations(ordered_transformed_data, columns_to_remove)

#### Running GIMME ####
# Running GIMME using scripts
# Save the function to a file
writeLines(c(
  "run_gimme_sequential <- function(simulations_list, base_output_dir) {",
  "  if (!dir.exists(base_output_dir)) {",
  "    dir.create(base_output_dir, recursive = TRUE)",
  "  }",
  "  for (i in seq_along(simulations_list)) {",
  "    current_simulation <- simulations_list[[i]]",
  "    sim_output_dir <- file.path(base_output_dir, paste0('Sim', i))",
  "    if (!dir.exists(sim_output_dir)) {",
  "      dir.create(sim_output_dir)",
  "    }",
  "    tryCatch({",
  "      gimme(",
  "        data = current_simulation,",
  "        out = sim_output_dir,",
  "        subgroup = FALSE,",
  "        groupcutoff = 0.75,",
  "        outcome = 'depressedmood_state'",
  "      )",
  "      message(sprintf('Simulation %d completed successfully', i))",
  "    }, error = function(e) {",
  "      message(sprintf('Simulation %d failed with error: %s', i, e$message))",
  "    })",
  "  }",
  "}"
), "run_gimme_sequential.R")

# Function to split simulations into batches with numbered simulation names
split_into_batches <- function(simulations_list, batch_size) {
  split(simulations_list, ceiling(seq_along(simulations_list) / batch_size))
}

# Function to save each batch as an R script with consecutive simulation numbering
save_batch_script <- function(batch, batch_index, output_dir, start_number) {
  # Define folder names for each simulation in the batch
  simulation_names <- sprintf("Sim%d", seq(start_number, start_number + length(batch) - 1))
  
  # Create a mapping of batch simulations to new simulation names
  named_batch <- setNames(batch, simulation_names)
  
  # Save batch to an RDS file
  batch_file <- file.path(output_dir, paste0("batch_", batch_index, ".rds"))
  saveRDS(named_batch, batch_file)
  
  # Generate the batch script to load and run the batch
  script_content <- paste0(
    "library(gimme)\n",
    "source('run_indsem_sequential.R')\n",
    "start_time <- Sys.time()\n",  # Log start time
    "batch <- readRDS('", batch_file, "')\n",  # Load the batch from the saved RDS file
    "run_indsem_sequential(batch, '", file.path(output_dir, paste0("batch_", batch_index)), "')\n",
    "end_time <- Sys.time()\n",  # Log end time
    "cat(sprintf('Batch ", batch_index, " completed in %s seconds\\n', as.numeric(difftime(end_time, start_time, units = 'secs'))))\n"
  )
  
  # Save the script to a file
  writeLines(script_content, paste0("batch_", batch_index, ".R"))
  
  return(named_batch)  # Return the batch with updated simulation names
}

# Function to run batch scripts in parallel with limited cores
run_batch_scripts <- function(num_cores, num_batches) {
  cl <- makeCluster(num_cores)  # Create a cluster with the specified number of cores
  results <- parLapply(cl, seq_len(num_batches), function(i) {
    start_time <- Sys.time()
    # Run the batch script and redirect output to a log file
    system(paste0("Rscript batch_", i, ".R > batch_", i, ".log 2>&1"))
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs")
    cat(sprintf("Batch %d completed in %s seconds\n", i, as.numeric(elapsed_time)))
    return(as.numeric(elapsed_time))
  })
  stopCluster(cl)  # Stop the cluster after execution
  total_time <- sum(unlist(results))
  cat(sprintf("Total time for all batches: %s seconds\n", total_time))
}

# Example Usage
# Define the size of each batch
batch_size <- 5

# Split the simulations into batches
batches <- split_into_batches(GIMME_simdatalist, batch_size)

# Define the output directory
output_dir <- "output_directory_GIMME"
dir.create(output_dir, recursive = TRUE)

# Run the batch scripts in parallel
num_cores <- 8  # Adjust based on the number of cores available
start_time <- Sys.time()
run_batch_scripts(num_cores, length(batches))
end_time <- Sys.time()

# Log total execution time
total_execution_time <- difftime(end_time, start_time, units = "secs")
cat(sprintf("Total execution time for all scripts: %s seconds\n", total_execution_time))
