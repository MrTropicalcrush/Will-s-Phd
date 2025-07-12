#### Simulating cross over interaction ####
# Load necessary package
library(dplyr)

set.seed(123)

# Parameters
n_datasets <- 100
n_individuals <- 102
n_time <- 70
phi <- 0.1
error_sd <- 1

#### Simulate XZ interaction only ####
# Function to simulate an individual
simulate_individual <- function(id, n_time, phi, error_sd, interaction = TRUE) {
  Z_raw <- rbinom(n_time, 1, 0.5)
  X_raw <- rnorm(n_time)
  e <- rnorm(n_time, mean = 0, sd = error_sd)
  
  # Simulate Y based on interaction or no effect
  Y <- numeric(n_time)
  if (interaction) {
    Y[1] <- ifelse(Z_raw[1] == 0, X_raw[1], -X_raw[1]) + e[1]
    for (t in 2:n_time) {
      interaction_effect <- ifelse(Z_raw[t] == 0, X_raw[t], -X_raw[t])
      Y[t] <- phi * Y[t - 1] + interaction_effect + e[t]
    }
  } else {
    Y[1] <- e[1]
    for (t in 2:n_time) {
      Y[t] <- phi * Y[t - 1] + e[t]  # AR(1) + noise only; no X or Z involved
    }
  }
  
  # Center X and Z
  X <- scale(X_raw, center = TRUE, scale = FALSE)
  Z <- scale(Z_raw, center = TRUE, scale = FALSE)
  XZ <- X * Z
  
  data.frame(
    ID = id,
    Time = 1:n_time,
    Y = Y,
    X = as.numeric(X),
    Z = as.numeric(Z),
    XZ = as.numeric(XZ),
    has_interaction = interaction
  )
}

# Simulate one dataset (list of 102 individuals, 51 with interaction, 51 with no effect)
simulate_dataset_list <- function(n_individuals, n_time, phi, error_sd) {
  half <- floor(n_individuals / 2)
  c(
    lapply(1:half, function(i) simulate_individual(i, n_time, phi, error_sd, interaction = TRUE)),
    lapply((half + 1):n_individuals, function(i) simulate_individual(i, n_time, phi, error_sd, interaction = FALSE))
  )
}

# Simulate 100 datasets
simulation_100_combined <- lapply(1:n_datasets, function(i) {
  simulate_dataset_list(n_individuals, n_time, phi, error_sd)
})

#### Simulate XZ interaction and Z main effect ####
# Function to simulate one individual
simulate_individual_main_and_cross <- function(id, n_time, phi, error_sd, has_effect = TRUE) {
  Z_raw <- rbinom(n_time, 1, 0.5)
  X_raw <- rnorm(n_time)
  e <- rnorm(n_time, mean = 0, sd = error_sd)
  
  Y <- numeric(n_time)
  
  if (has_effect) {
    # Main effect of Z and cross-over interaction
    Y[1] <- ifelse(Z_raw[1] == 0, X_raw[1], -X_raw[1]) + 0.5 * Z_raw[1] + e[1]
    for (t in 2:n_time) {
      interaction_effect <- ifelse(Z_raw[t] == 0, X_raw[t], -X_raw[t])
      Y[t] <- phi * Y[t - 1] + interaction_effect + 0.5 * Z_raw[t] + e[t]
    }
  } else {
    # No effect: only AR(1) + noise
    Y[1] <- e[1]
    for (t in 2:n_time) {
      Y[t] <- phi * Y[t - 1] + e[t]
    }
  }
  
  # Center X and Z
  X <- scale(X_raw, center = TRUE, scale = FALSE)
  Z <- scale(Z_raw, center = TRUE, scale = FALSE)
  XZ <- X * Z
  
  data.frame(
    ID = id,
    Time = 1:n_time,
    Y = Y,
    X = as.numeric(X),
    Z = as.numeric(Z),
    XZ = as.numeric(XZ),
    Z_raw = Z_raw,
    has_effect = has_effect
  )
}

# Function to simulate one dataset (51 with main + cross-over, 51 with no effect)
simulate_dataset_list <- function(n_individuals = 102, n_time, phi, error_sd) {
  half <- floor(n_individuals / 2)
  c(
    lapply(1:half, function(i) simulate_individual_main_and_cross(i, n_time, phi, error_sd, has_effect = TRUE)),
    lapply((half + 1):n_individuals, function(i) simulate_individual_main_and_cross(i, n_time, phi, error_sd, has_effect = FALSE))
  )
}

# Simulate 100 datasets
simulation_100_1main_cross <- lapply(1:n_datasets, function(i) {
  simulate_dataset_list(n_individuals, n_time, phi, error_sd)
})

#### Simulated XZ interaction, Z main effect, and X main effect ####
# Function to simulate individuals with 2 main effects + interaction, or no effect
simulate_individual_2main_and_interaction <- function(id, n_time, phi, error_sd, has_effect = TRUE) {
  Z_raw <- rbinom(n_time, 1, 0.5)
  X_raw <- rnorm(n_time)
  e <- rnorm(n_time, mean = 0, sd = error_sd)
  
  Y <- numeric(n_time)
  
  if (has_effect) {
    # Add main effects for X and Z, plus cross-over interaction
    Y[1] <- 0.5 * Z_raw[1] + 0.5 * X_raw[1] + ifelse(Z_raw[1] == 0, X_raw[1], -X_raw[1]) + e[1]
    for (t in 2:n_time) {
      interaction_effect <- ifelse(Z_raw[t] == 0, X_raw[t], -X_raw[t])
      Y[t] <- phi * Y[t - 1] + 0.5 * Z_raw[t] + 0.5 * X_raw[t] + interaction_effect + e[t]
    }
  } else {
    # No effect, just AR(1) + noise
    Y[1] <- e[1]
    for (t in 2:n_time) {
      Y[t] <- phi * Y[t - 1] + e[t]
    }
  }
  
  # Center X and Z for modeling
  X <- scale(X_raw, center = TRUE, scale = FALSE)
  Z <- scale(Z_raw, center = TRUE, scale = FALSE)
  XZ <- X * Z
  
  data.frame(
    ID = id,
    Time = 1:n_time,
    Y = Y,
    X = as.numeric(X),
    Z = as.numeric(Z),
    XZ = as.numeric(XZ),
    X_raw = X_raw,
    Z_raw = Z_raw,
    has_effect = has_effect
  )
}

# Function to simulate a dataset with 51 effect vs 51 no-effect individuals
simulate_dataset_list <- function(n_individuals, n_time, phi, error_sd) {
  half <- floor(n_individuals / 2)
  c(
    lapply(1:half, function(i) simulate_individual_2main_and_interaction(i, n_time, phi, error_sd, has_effect = TRUE)),
    lapply((half + 1):n_individuals, function(i) simulate_individual_2main_and_interaction(i, n_time, phi, error_sd, has_effect = FALSE))
  )
}

# Simulate 100 datasets
simulation_100_2main_cross <- lapply(1:n_datasets, function(i) {
  simulate_dataset_list(n_individuals, n_time, phi, error_sd)
})

# Assuming your list is named simulation_list (length 100)
# and inside each simulation there are 102 individual dataframes

simulation_100_1main_cross <- lapply(simulation_100_1main_cross, function(sim) {
  do.call(rbind, sim)  # Combine all 102 individuals into one dataframe
})

############################################################################
#### Tests of interaction simulation ####
library(ggplot2)
library(dplyr)

# Assuming your list is named simulation_list (length 100)
# and inside each simulation there are 102 individual dataframes

combined_simulations <- lapply(simulation_100_2main_cross, function(sim) {
  do.call(rbind, sim)  # Combine all 102 individuals into one dataframe
})
test <- combined_simulations[[1]]

# Plot outcome over time by condition
# Plot some individuals from each group
ggplot(test %>% filter(ID %in% c(1, 5, 51, 52, 60, 100)), 
       aes(x = Time, y = Y, color = factor(has_effect))) +
  geom_line() +
  facet_wrap(~ID) +
  labs(color = "Has Effect", title = "Outcome Trajectories", y = "Y", x = "Time")

# Plot average Y by Z_raw to inspect main effect of Z
test %>%
  group_by(has_effect, Z_raw) %>%
  summarize(mean_Y = mean(Y), .groups = "drop") %>%
  ggplot(aes(x = factor(Z_raw), y = mean_Y, fill = factor(has_effect))) +
  geom_col(position = "dodge") +
  labs(x = "Z_raw (0 or 1)", y = "Average Y", fill = "Has Effect",
       title = "Mean Y by Z (Main Effect of Z)")

# Plot interaction effect (cross-over)
test %>%
  group_by(has_effect, Z_raw) %>%
  mutate(X_bin = cut(X, breaks = 3)) %>%
  group_by(has_effect, Z_raw, X_bin) %>%
  summarize(mean_Y = mean(Y), .groups = "drop") %>%
  ggplot(aes(x = X_bin, y = mean_Y, color = factor(Z_raw), group = Z_raw)) +
  geom_line() +
  facet_wrap(~has_effect) +
  labs(title = "Interaction Effect: X by Z",
       x = "Binned X", y = "Mean Y", color = "Z_raw")

# Model based check
# For individuals with effects
lm(Y ~ X + Z + XZ, data = test %>% filter(has_effect == TRUE)) %>% summary()

# For individuals without effects
lm(Y ~ X + Z + XZ, data = test %>% filter(has_effect == FALSE)) %>% summary()