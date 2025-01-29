#### Preparing data for indSEM (same process as GIMME) ####
# Function to restructure the data
transform_to_individual_lists <- function(simulations_list) {
  # Create a new list to store the transformed structure
  transformed_list <- lapply(simulations_list, function(simulation_data) {
    # Split the simulation dataframe into a list by individual ID
    split(simulation_data, simulation_data$ID)
  })
  
  return(transformed_list)
}

# Assuming `your_data` is your original dataframe of lists of simulations
transformed_data <- transform_to_individual_lists(simulated_data_list_10)

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

#### Running indSEM ####
# Save the function to a file (IndSEM)
writeLines(c(
  "run_indsem_sequential <- function(simulations_list, base_output_dir) {",
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
  "      indSEM(",
  "        data = current_simulation,",
  "        out = sim_output_dir",
  "        outcome = 'depressedmood_state'",
  "      )",
  "      message(sprintf('Simulation %d completed successfully', i))",
  "    }, error = function(e) {",
  "      message(sprintf('Simulation %d failed with error: %s', i, e$message))",
  "    })",
  "  }",
  "}"
), "run_indsem_sequential.R")

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
output_dir <- "output_directory_indSEM" # Replace with your own directory
dir.create(output_dir, recursive = TRUE)

# Run the batch scripts in parallel
num_cores <- 8  # Adjust based on the number of cores available
start_time <- Sys.time()
run_batch_scripts(num_cores, length(batches))
end_time <- Sys.time()

# Log total execution time
total_execution_time <- difftime(end_time, start_time, units = "secs")
cat(sprintf("Total execution time for all scripts: %s seconds\n", total_execution_time))

#### Extracting indSEM results ####
# Move all batches into one folder and numerically name each sim (e.g., Sim1, Sim2, etc.)
library(tidyr)
library(dplyr)

# Define the parent directory containing the batch folder
parent_directory <- "C:/Users/WillLi/Documents/mlvar simulation/output_directory_indSEM"  # Replace with your actual path

# STEP 1 - Process indSEM Folders #
process_indSEM_simulations <- function(directory) {
  # List all simulation folders
  sim_folders <- list.dirs(directory, recursive = FALSE)
  
  # Initialize a list to store results for all simulations
  all_simulation_results <- list()
  column_names <- NULL  # Initialize column names as NULL
  
  for (sim_folder in sim_folders) {
    # Construct the path to the individual folder
    individual_folder <- file.path(sim_folder, "individual")
    
    # Check if the individual folder exists
    if (dir.exists(individual_folder)) {
      message(paste("Processing folder:", individual_folder))
      
      # List all individual beta files (e.g., 1BetasStd.csv, 2BetasStd.csv, etc.)
      beta_files <- list.files(individual_folder, pattern = "\\d+BetasStd\\.csv$", full.names = TRUE)
      
      if (length(beta_files) == 0) {
        message(paste("No beta files found in:", individual_folder))
      }
      
      # Process each beta file
      for (beta_file in beta_files) {
        message(paste("Processing file:", beta_file))
        
        # Extract the individual ID from the filename
        individual_id <- sub("BetasStd\\.csv$", "", basename(beta_file))
        
        # Attempt to read the beta matrix
        beta_matrix <- tryCatch({
          read.csv(beta_file, row.names = 1)
        }, error = function(e) {
          message(paste("Error reading file:", beta_file, ":", e$message))
          NULL
        })
        
        # Skip processing if beta_matrix is NULL
        if (is.null(beta_matrix)) next
        
        # Extract column names from the first valid beta matrix
        if (is.null(column_names)) {
          column_names <- colnames(beta_matrix)
          message("Column names detected:", paste(column_names, collapse = ", "))
        }
        
        # Extract the row for depressedmood_state
        if ("depressedmood_state" %in% rownames(beta_matrix)) {
          depressedmood_row <- beta_matrix["depressedmood_state", ]
          
          # Convert the row to a long format
          beta_data <- data.frame(
            ID = as.numeric(individual_id),
            rhs = names(depressedmood_row),   # Names of the columns (variables)
            beta = as.numeric(depressedmood_row),  # Beta coefficients
            simulation = basename(sim_folder)  # Add simulation identifier
          )
          
          # Append to results
          all_simulation_results[[paste0(sim_folder, "_", individual_id)]] <- beta_data
        } else {
          message(paste("Row 'depressedmood_state' not found in file:", beta_file))
        }
      }
    } else {
      message(paste("Folder does not exist:", individual_folder))
    }
  }
  
  # Combine results from all simulations into one dataframe
  if (length(all_simulation_results) > 0) {
    combined_data <- do.call(rbind, all_simulation_results)
    
    # Ensure column names match the BetasStd.csv variables
    if (!is.null(column_names)) {
      combined_data <- combined_data %>%
        mutate(rhs = factor(rhs, levels = column_names))
    }
    
    return(combined_data)
  } else {
    message("No valid data found in the simulations.")
    return(NULL)
  }
}

# Process all folders in the batch directory
parent_directory <- "C:/Users/WillLi/Documents/mlvar simulation/output_directory_indSEM"
all_paths_indSEM <- process_indSEM_simulations(parent_directory)

# View the results
if (!is.null(all_paths_indSEM)) {
  print(head(all_paths_indSEM))
} else {
  message("No data was extracted.")
}

# Replace zeros with NA (if not already done in processing function)
all_paths_indSEM <- all_paths_indSEM %>%
  mutate(beta = ifelse(beta == 0, NA, beta))

# STEP 2 - Calculate Path Presence for Each Individual #
path_presence_per_individual <- all_paths_indSEM %>%
  group_by(ID, rhs) %>%
  summarise(
    presence_count = sum(!is.na(beta)),  # Count simulations where the path exists (non-NA)
    total_simulations = length(unique(all_paths_indSEM$simulation)),  # Total number of simulations
    presence_proportion = presence_count / total_simulations,  # Proportion of presence
    .groups = "drop"
  )

# Define the majority threshold
threshold <- 0.5  # Adjust as needed

# STEP 3 - Filter Paths Meeting the Majority Threshold #
majority_paths_per_individual <- path_presence_per_individual %>%
  filter(presence_proportion > threshold) %>%
  select(ID, rhs)

# STEP 4 - Calculate Average Beta Coefficients for Majority Paths #
filtered_paths_per_individual <- all_paths_indSEM %>%
  semi_join(majority_paths_per_individual, by = c("ID", "rhs"))

average_betas_per_individual <- filtered_paths_per_individual %>%
  group_by(ID, rhs) %>%
  summarise(
    Average_Beta = mean(beta, na.rm = TRUE),  # Calculate average beta coefficient
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = rhs, values_from = Average_Beta)

# View the final result
print(average_betas_per_individual)
