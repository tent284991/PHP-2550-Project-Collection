# AIM 1: Design a simulation study using the ADEMP framework from class to evaluate potential study designs.
# AIM 2: Explore relationships between the underlying data generation mechanism parameters and the relative costs c1/c2 and how these impact the optimal study design.

# Aim 1 and 2  ------------------------------------------------------------
# Load required packages
library(lme4)      # For fitting mixed models
library(tidyverse) # For data manipulation and visualization
library(parallel)  # For parallel processing
library(gridExtra) # For arranging multiple plots

# Function to generate data from hierarchical normal model
generate_data <- function(G, R, alpha = 0, beta = 1, gamma = 1, sigma = 1) {
  # Generate cluster-level treatment assignment
  X <- rep(c(0,1), length.out = G)  # Balanced treatment assignment
  
  # Generate cluster-level random effects
  epsilon <- rnorm(G, 0, gamma)
  
  # Generate cluster means
  mu <- alpha + beta * X + epsilon
  
  # Generate observations within clusters
  data <- data.frame(
    cluster = rep(1:G, each = R),
    treatment = rep(X, each = R),
    true_mean = rep(mu, each = R)
  )
  
  # Add observation-level noise
  data$Y <- rnorm(G*R, data$true_mean, sigma)
  
  return(data)
}

# Function to calculate total cost
calculate_cost <- function(G, R, c1, c2) {
  return(G * (c1 + (R-1)*c2))
}

# Function to fit model and extract treatment effect estimate
fit_model <- function(data) {
  model <- lmer(Y ~ treatment + (1|cluster), data = data)
  beta_est <- fixef(model)["treatment"]
  return(beta_est)
}

# Function to evaluate design performance
evaluate_design <- function(G, R, c1, c2, budget, nsim = 100) {
  # Check if design is within budget
  total_cost <- calculate_cost(G, R, c1, c2)
  if(total_cost > budget) return(NULL)
  
  # Run simulations
  results <- replicate(nsim, {
    data <- generate_data(G, R)
    beta_est <- fit_model(data)
    return(beta_est)
  })
  
  # Calculate performance metrics
  mse <- mean((results - 1)^2)  # True beta = 1
  bias <- mean(results) - 1
  power <- mean(abs(results/sd(results)) > qnorm(0.975))
  
  return(data.frame(
    G = G,
    R = R,
    cost_ratio = c1/c2,
    total_cost = total_cost,
    mse = mse,
    bias = bias,
    power = power
  ))
}

# Set simulation parameters
budget <- 2000
cost_ratios <- c(2, 5, 10)
G_values <- seq(10, 50, by = 5)
R_values <- seq(2, 20, by = 2)

# Run simulation for different cost ratios
results_list <- list()

for(ratio in cost_ratios) {
  c1 <- ratio * 10  # Base cost for first sample
  c2 <- 10          # Base cost for additional samples
  
  # Create all possible combinations of G and R
  designs <- expand.grid(G = G_values, R = R_values)
  
  # Evaluate each design (in parallel)
  results <- mcmapply(function(g, r) {
    evaluate_design(g, r, c1, c2, budget)
  }, designs$G, designs$R, mc.cores = parallel::detectCores() - 1, SIMPLIFY = FALSE)
  
  # Combine results
  results_df <- do.call(rbind, results[!sapply(results, is.null)])
  results_df$cost_ratio <- ratio
  
  results_list[[as.character(ratio)]] <- results_df
}

# Combine all results
all_results <- bind_rows(results_list)

# Create visualization of results
plot_power <- ggplot(all_results, aes(x = G, y = R, fill = power)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_viridis_c() +
  labs(title = "Statistical Power by Design and Cost Ratio",
       x = "Number of Clusters (G)",
       y = "Observations per Cluster (R)")

plot_mse <- ggplot(all_results, aes(x = G, y = R, fill = mse)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_viridis_c() +
  labs(title = "MSE by Design and Cost Ratio",
       x = "Number of Clusters (G)",
       y = "Observations per Cluster (R)")

# Find optimal designs for each cost ratio
optimal_designs <- all_results %>%
  group_by(cost_ratio) %>%
  slice_max(power, n = 1)

# Print optimal designs
print(optimal_designs)

# Save plots
grid.arrange(plot_power, plot_mse, ncol = 1)


# AIM 3: -----------------------------------
# Extend your simulation study to the setting in which Y follows a Poisson distribution with mean μi and explore how this impacts the results. The hierarchical model for this setting is given below.

# Simulation Under Poisson Distribution 

# Function to generate data from hierarchical Poisson model
generate_poisson_data <- function(G, R, alpha = 0, beta = 0.5, gamma = 0.3) {
  # Generate cluster-level treatment assignment
  X <- rep(c(0,1), length.out = G)
  
  # Generate log(mu_i) from normal distribution
  log_mu <- rnorm(G, alpha + beta * X, gamma)
  mu <- exp(log_mu)
  
  # Generate aggregated Poisson observations
  Y <- rpois(G, R * mu)
  
  # Create data frame
  data <- data.frame(
    cluster = 1:G,
    treatment = X,
    Y = Y,
    offset = log(R)  # Include offset for number of observations
  )
  
  return(data)
}

# Function to fit Poisson mixed model
fit_poisson_model <- function(data) {
  # Fit generalized linear mixed model with Poisson family
  model <- glmer(Y ~ treatment + offset(offset) + (1|cluster), 
                 family = poisson,
                 data = data)
  
  beta_est <- fixef(model)["treatment"]
  return(beta_est)
}

# Function to evaluate design performance for Poisson case
evaluate_poisson_design <- function(G, R, c1, c2, budget, nsim = 100) {
  # Check if design is within budget
  total_cost <- G * (c1 + (R-1)*c2)
  if(total_cost > budget) return(NULL)
  
  # Run simulations
  results <- replicate(nsim, {
    tryCatch({
      data <- generate_poisson_data(G, R)
      beta_est <- fit_poisson_model(data)
      return(beta_est)
    }, error = function(e) NA)
  })
  
  # Remove any failed simulations
  results <- results[!is.na(results)]
  
  if(length(results) < nsim/2) return(NULL)  # Return NULL if too many failures
  
  # Calculate performance metrics
  true_beta <- 0.5  # True effect size
  mse <- mean((results - true_beta)^2)
  bias <- mean(results) - true_beta
  power <- mean(abs(results/sd(results)) > qnorm(0.975))
  
  return(data.frame(
    G = G,
    R = R,
    cost_ratio = c1/c2,
    total_cost = total_cost,
    mse = mse,
    bias = bias,
    power = power
  ))
}

# Set simulation parameters
budget <- 2000
cost_ratios <- c(2, 5, 10)
G_values <- seq(10, 50, by = 5)
R_values <- seq(2, 20, by = 2)

# Run simulation for different cost ratios
results_list_poisson <- list()

for(ratio in cost_ratios) {
  c1 <- ratio * 10
  c2 <- 10
  
  designs <- expand.grid(G = G_values, R = R_values)
  
  results <- mcmapply(function(g, r) {
    evaluate_poisson_design(g, r, c1, c2, budget)
  }, designs$G, designs$R, mc.cores = parallel::detectCores() - 1, SIMPLIFY = FALSE)
  
  results_df <- do.call(rbind, results[!sapply(results, is.null)])
  results_df$cost_ratio <- ratio
  
  results_list_poisson[[as.character(ratio)]] <- results_df
}

# Combine all results
all_results_poisson <- bind_rows(results_list_poisson)

# Create visualizations
plot_power_poisson <- ggplot(all_results_poisson, aes(x = G, y = R, fill = power)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_viridis_c() +
  labs(title = "Statistical Power by Design and Cost Ratio (Poisson)",
       x = "Number of Clusters (G)",
       y = "Observations per Cluster (R)")

plot_mse_poisson <- ggplot(all_results_poisson, aes(x = G, y = R, fill = mse)) +
  geom_tile() +
  facet_wrap(~cost_ratio) +
  scale_fill_viridis_c() +
  labs(title = "MSE by Design and Cost Ratio (Poisson)",
       x = "Number of Clusters (G)",
       y = "Observations per Cluster (R)")

# Find optimal designs for each cost ratio
optimal_designs_poisson <- all_results_poisson %>%
  group_by(cost_ratio) %>%
  slice_max(power, n = 1)

# Print optimal designs
print(optimal_designs_poisson)

# Compare plots between normal and Poisson cases
grid.arrange(
  plot_power, plot_power_poisson,
  plot_mse, plot_mse_poisson,
  ncol = 2,
  top = "Figure 1. Comparison of Normal vs Poisson"
)











