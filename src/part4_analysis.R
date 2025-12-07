# ==============================================================================
# Part 4: Sequential Testing
# ==============================================================================

###################
#Rough outline in pseudocode:

#get all engine pairs

#for every engine pair:
  # get a list of game results between the two engines, in the order they appear in the .csv
  # number_of_games = length(game list)

  # Initialize the Prior: Delta is normally distributed around mean 0, with variance 2*(200^2)

  #reject_or_accept_H0 = false  
  #k = 1

  #while (k <= number_of_games) or (reject_or_accept_H0 == false):
    # Let r_k be the result of the k-th game.

    # Define the prior of step k to be the posterior after k-1 iterations, or the initialized prior if k=1   

    # Calculate the likelihood of the cumulative results r_1, ..., r_k, given the prior at step k,
    # using the expected score E(Delta) = P(new engine wins | Delta) + P(draw | Delta)
    # (the expected score does not give information about the draw rate, so we model it as a Beta(3,7) distributed variable

    # Update the Posterior (MCMC sampling using Stan)

    # Reject H_0, if P(Delta < E_0 | r_1,...,r_k) < alpha
    # Accept H_0, if P(Delta >= E_0 | r_1,...,r_k) > beta

    # k++

##################
if (basename(getwd()) == "src") {
  setwd("..")
}


library(rstan)
source("src/preprocessing.R")

#----------------------------------------
# Parameters
#----------------------------------------

E_0 <- 10 #improvement difference threshold (H_0: Delta >= E_0)
alpha <- 0.05 #upper bound for rejecting H0 (reject if P(Delta < E_0) < alpha)
beta <- 0.95 #lower bound for accepting H0 (accept if P(Delta >= E_0) > beta)

#----------------------------------------
# Compile Stan Model
#----------------------------------------

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_model_seq <- stan_model("models/sequential_test_model.stan")
cat("Stan model compiled successfully!\n\n")

#----------------------------------------
# Helper functions
#----------------------------------------

expected_score <- function(delta) {
  # Calculate expected score for new engine given Delta
  # where Delta = R_new - R_base
  
  # E_new = 1 / (1 + 10^(-Delta/400))
  return(1 / (1 + 10^(-delta / 400)))
}

update_posterior <- function(prior_mean, prior_var, results, 
                             stan_model_obj = stan_model_seq) {
  # Update posterior using Stan
  
  # Ensure results is a numeric vector (not scalar or data frame column)
  results <- as.numeric(results)
  n <- length(results)
  if (n == 0) {
    return(list(mean = prior_mean, var = prior_var))
  }
  
  # Ensure score is always passed as an array/vector structure
  # Use array() to explicitly maintain dimensions even for length-1 vectors
  # This prevents R from simplifying length-1 vectors to scalars when passing to Stan
  score_vector <- array(as.numeric(results), dim = n)
  
  stan_data <- list(
    prior_mean = prior_mean,
    prior_sd = sqrt(prior_var),
    N = n,
    score = score_vector
  )
  
  # Run MCMC sampling
  # Use fewer iterations for sequential testing to speed up
  # Since we're doing this many times in a loop
  # The model estimates both Delta and draw_rate simultaneously
  fit <- sampling(
    stan_model_obj,
    data = stan_data,
    chains = 2,           # Reduced chains for speed
    iter = 1000,          # Reduced iterations for speed
    warmup = 500,
    refresh = 0,          
    control = list(adapt_delta = 0.8)
  )
  
  # Extract posterior samples
  delta_samples <- extract(fit, pars = "delta")$delta
  
  # Check if sampling was successful
  if (is.null(delta_samples) || length(delta_samples) == 0) {
    stop("Stan sampling failed: no samples extracted. Check Stan model and data.")
  }
  
  # Calculate posterior mean and variance
  posterior_mean <- mean(delta_samples)
  posterior_var <- var(delta_samples)
  
  return(list(mean = posterior_mean, var = posterior_var))
}

calculate_reject_prob <- function(posterior_mean, posterior_var, E_0) {
  # Calculate probability that Delta < E_0 (i.e., reject H0 condition)
  # H_0: Delta >= E_0 (new engine is at least E_0 points better)
  # Reject H_0 if P(Delta < E_0 | data) < alpha
  
  #pnorm: returns the value of the cdf of the normal distribution,
  # given the random variable E_0, the mean and stadard deviation of Delta.
  return(pnorm(E_0, mean = posterior_mean, sd = sqrt(posterior_var)))
}

calculate_accept_prob <- function(posterior_mean, posterior_var, E_0){
  return (1.0-calculate_reject_prob(posterior_mean, posterior_var, E_0))
}


# ==============================================================================
# Main Sequential Testing Function
# ==============================================================================

sequential_test <- function(data, engine1, engine2, E_0, alpha, beta) {
  # Sequential test for engine comparison
  # Delta = R_new - R_base (rating difference)
  # H_0: Delta >= E_0 (null hypothesis: new engine is at least E_0 points better)
  # Reject H_0 if P(Delta < E_0 | data) < alpha
  # Accept H_0 if P(Delta >= E_0 | data) > beta
  
  # Get ordered games between the two engines
  games <- get_ordered_games(data, engine1, engine2)
  
  if (nrow(games) == 0) {
    return(list(
      decision = "no_games",
      games_played = 0,
      final_posterior = NULL
    ))
  }
  
  # Initialize prior: Delta ~ N(0, 2*(200^2))
  prior_mean <- 0
  prior_var <- 2 * (200^2)
  
  # Extract results from new engine's perspective
  results <- as.numeric(games$new_wins)
  
  
  number_of_games <- length(results)
  reject_or_accept_H0 <- FALSE
  decision <- "undecided"
  k <- 1
  
  # Store posterior at each step
  posterior_mean <- prior_mean
  posterior_var <- prior_var
  
  while (k <= number_of_games && !reject_or_accept_H0) {
    # Let r_k be the result of the k-th game
    r_k <- results[k]
    
    # Define the prior of step k to be the posterior after k-1 iterations
    # (or the initialized prior if k=1)
    if (k == 1) {
      # First game: use initialized prior
      current_prior_mean <- prior_mean
      current_prior_var <- prior_var
    } else {
      # Use previous posterior as new prior
      current_prior_mean <- posterior_mean
      current_prior_var <- posterior_var
    }
    
    # Ensure results_so_far is always a vector (not scalar when k=1)
    results_so_far <- as.numeric(results[1:k])
    
    
    # Calculate the likelihood of the cumulative results r_1, ..., r_k
    # given the prior at step k
    # Update the Posterior using Stan MCMC
    # The model estimates both Delta and draw_rate simultaneously
    # draw_rate is estimated as a parameter (not fixed) with a Beta(3, 7) prior
    posterior <- update_posterior(current_prior_mean, current_prior_var, 
                                  results_so_far, stan_model_seq)
    posterior_mean <- posterior$mean
    posterior_var <- posterior$var
    
    # Calculate probabilities
    reject_prob <- calculate_reject_prob(posterior_mean, posterior_var, E_0)
    accept_prob <- 1.0-reject_prob
    
    # Check rejection condition: Reject H_0 if P(Delta < E_0 | data) >= (1 - alpha)
    # H_0: Delta >= E_0, so reject if we're confident Delta < E_0
    # This means: reject if reject_prob >= (1 - alpha) = 0.95
    if (reject_prob >= (1 - alpha)) {
      reject_or_accept_H0 <- TRUE
      decision <- "reject_H0"  # New engine is NOT significantly better (Delta < E_0)
    }
    
    # Check acceptance condition: Accept H_0 if P(Delta >= E_0 | data) > beta
    # H_0: Delta >= E_0, so accept if we're confident Delta >= E_0
    if (!reject_or_accept_H0) {
      if (accept_prob > beta) {
        reject_or_accept_H0 <- TRUE
        decision <- "accept_H0"  # New engine IS significantly better (Delta >= E_0)
      }
    }
    
    k <- k + 1
  }
  
  return(list(
    engine1 = engine1,
    engine2 = engine2,
    new_engine = games$new_engine[1],
    base_engine = games$base_engine[1],
    decision = decision,
    games_played = k - 1,
    total_games_available = number_of_games,
    final_posterior_mean = posterior_mean,
    final_posterior_var = posterior_var,
    final_reject_prob = calculate_reject_prob(posterior_mean, posterior_var, E_0),
    final_accept_prob = 1.0-final_reject_prob
  ))
}

# ==============================================================================
# Execute Sequential Testing for All Pairs
# ==============================================================================

# Read the data
data <- read.csv("data/games.csv", stringsAsFactors = FALSE)

# Get all engine pairs
engines <- unique(c(data$white, data$black))
pairs <- get_engine_pairs(data)

cat("Sequential Testing for All Engine Pairs\n")
cat("========================================\n\n")
cat(sprintf("E_0 = %d, alpha = %.3f, beta = %.3f\n\n", E_0, alpha, beta))

# Store results
results_list <- list()

# Run sequential test for each pair
for (pair in pairs) {
  engine1 <- pair[1]
  engine2 <- pair[2]
  
  cat(sprintf("Testing pair: %s vs %s\n", engine1, engine2))
  
  result <- sequential_test(data, engine1, engine2, E_0, alpha, beta)
  results_list[[length(results_list) + 1]] <- result
  
  cat(sprintf("  Decision: %s\n", result$decision))
  cat(sprintf("  Games played: %d / %d\n", result$games_played, result$total_games_available))
  cat(sprintf("  Final posterior mean (Delta): %.2f\n", result$final_posterior_mean))
  cat(sprintf("  Final reject prob: %.4f\n", result$final_reject_prob))
  cat(sprintf("  Final accept prob: %.4f\n\n", result$final_accept_prob))
}

# Convert results to data frame for easier analysis
results_df <- do.call(rbind, lapply(results_list, function(r) {
  data.frame(
    engine1 = r$engine1,
    engine2 = r$engine2,
    new_engine = r$new_engine,
    base_engine = r$base_engine,
    decision = r$decision,
    games_played = r$games_played,
    total_games = r$total_games_available,
    final_delta_mean = r$final_posterior_mean,
    final_delta_sd = sqrt(r$final_posterior_var),
    stringsAsFactors = FALSE
  )
}))

cat("\nSummary Table:\n")
print(results_df)

