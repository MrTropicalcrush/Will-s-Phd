#################################################
#### Simulating non-linear data (quadratic) ####
################################################
# Loop to simulate 100 datasets
for (dataset_idx in 1:n_datasets) {
  # Initialize a list to store the data for all individuals in this dataset
  simulated_data <- list()
  
  # Loop through each individual
  for (i in 1:n_individuals) {
    # Simulate IVs (e.g., time-varying predictors)
    IVs <- matrix(rnorm(n_timepoints * n_IVs, mean = 0, sd = 1), nrow = n_timepoints, ncol = n_IVs)
    
    # Extract the pre-generated beta coefficients for this individual
    b2_coefficients <- effect_size_df[effect_size_df$ID == i, "Effect_Size"]
    
    # Simulate DV using purely quadratic relationships (no linear term, no random noise)
    DV <- b2_coefficients[1] * IVs[, 1]^2 +  # Purely quadratic relationship with IV1
      b2_coefficients[2] * IVs[, 2]^2 +  # Purely quadratic relationship with IV2
      b2_coefficients[3] * IVs[, 3]^2 +  # Purely quadratic relationship with IV3
      b2_coefficients[4] * IVs[, 4]^2 +  # Purely quadratic relationship with IV4
      b2_coefficients[5] * IVs[, 5]^2    # Purely quadratic relationship with IV5
    
    # Combine into a data frame for this individual
    individual_data <- data.frame(
      ID = i,
      time = 1:n_timepoints,
      DV = DV,
      IV1 = IVs[, 1],
      IV2 = IVs[, 2],
      IV3 = IVs[, 3],
      IV4 = IVs[, 4],
      IV5 = IVs[, 5]
    )
    
    # Add to the list of simulated data for this dataset
    simulated_data[[i]] <- individual_data
  }
  
  # Combine all individuals' data into one data frame for this dataset
  simulated_data <- do.call(rbind, simulated_data)
  
  # Add this dataset to the list of all datasets
  simulated_datasets[[dataset_idx]] <- simulated_data
}

### Checking data ####
# Load ggplot2 for visualization
library(ggplot2)

# Loop through each individual
for (i in 1:3) {
  # Subset data for the current individual
  individual_data <- simulated_data[simulated_data$ID == i, ]
  
  # Loop through each IV
  for (j in 1:n_IVs) {
    # Get the name of the current IV
    iv_name <- paste0("IV", j)
    
    # Create the plot
    p <- ggplot(individual_data, aes(x = .data[[iv_name]], y = DV)) +
      geom_point() +  # Scatterplot of the data
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +  # Quadratic fit
      ggtitle(paste("Quadratic Relationship: DV ~", iv_name, "(Individual", i, ")")) +
      xlab(iv_name) +
      ylab("DV")
    
    # Display the plot
    print(p)
    
    # Optionally, save the plot to a file
    ggsave(paste0("Individual_", i, "_", iv_name, ".png"), plot = p, width = 6, height = 4)
  }
}

# Check simulated/true quadratic coefficient with calculated coefficent
# Load necessary libraries
library(ggplot2)

# Initialize an empty list to store the comparison results for all individuals
all_comparisons <- list()

# Get the unique IDs from the dataset
unique_ids <- unique(simulated_datasets[[2]]$ID)

# Loop through each individual
for (id in unique_ids) {
  # Subset the data for the current individual
  individual_data <- simulated_datasets[[2]][simulated_datasets[[2]]$ID == id, ]
  
  # Manually create quadratic terms for each IV
  individual_data$IV1_sq <- individual_data$IV1^2
  individual_data$IV2_sq <- individual_data$IV2^2
  individual_data$IV3_sq <- individual_data$IV3^2
  individual_data$IV4_sq <- individual_data$IV4^2
  individual_data$IV5_sq <- individual_data$IV5^2
  
  # Fit a quadratic model for each IV using manually created quadratic terms
  quadratic_models <- list(
    IV1 = lm(DV ~ IV1 + IV1_sq, data = individual_data),
    IV2 = lm(DV ~ IV2 + IV2_sq, data = individual_data),
    IV3 = lm(DV ~ IV3 + IV3_sq, data = individual_data),
    IV4 = lm(DV ~ IV4 + IV4_sq, data = individual_data),
    IV5 = lm(DV ~ IV5 + IV5_sq, data = individual_data)
  )
  
  # Extract the quadratic coefficients (b2) from the models
  estimated_b2 <- sapply(names(quadratic_models), function(iv) {
    model <- quadratic_models[[iv]]
    # Construct the correct column name for the quadratic term
    quadratic_term <- paste0(iv, "_sq")
    # Extract the coefficient for the quadratic term
    coef(summary(model))[quadratic_term, "Estimate"]
  })
  
  # Get the true coefficients for the current individual
  true_b2 <- effect_size_df[effect_size_df$ID == id, "Effect_Size"]
  
  # Create a comparison dataframe for the current individual
  comparison <- data.frame(
    ID = id,
    IV = paste0("IV", 1:5),
    True_b2 = true_b2,
    Estimated_b2 = estimated_b2
  )
  
  # Append the comparison to the list of all comparisons
  all_comparisons <- append(all_comparisons, list(comparison))
}

# Combine all individual comparisons into a single dataframe
final_comparison <- do.call(rbind, all_comparisons)

# View the final comparison dataframe
print(final_comparison)
