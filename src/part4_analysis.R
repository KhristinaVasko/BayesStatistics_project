# ==============================================================================
# Part 4: Sequential Testing
# ==============================================================================

###################
#Rough outline in pseudocode:

#get all engine pairs

#for every engine pair:
  # get a list of game results between the two engines, in the order they appear in the .csv
  # number_of_games = length(game list)

  # Initialize the Prior:
    #Delta is normally distributed around mean 0, with variance 2*(200^2)
    #draw_rate is initialised as Beta(3,7), which has expecation 0.3

  #reject_or_accept_H0 = false  
  #k = 1

  #while (k <= number_of_games) or (reject_or_accept_H0 == false):
    # Let r_k be the result of the k-th game.

    # Using Stan, calculate the posterior for Delta and draw_rate:
      # Calculate the likelihood of the cumulative results r_1, ..., r_k,
      # using the expected score E(Delta) = P(new engine wins | Delta) + P(draw | Delta)

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
alpha <- 0.05 #reject H0 if P(Delta < E_0) < alpha
beta <- 0.95 #accept H0 if P(Delta >= E_0) > beta

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
                             stan_model_obj = stan_model_seq, chains=4, iterations=1000, warmup=500) {
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
  # The model estimates both Delta and draw_rate simultaneously
  fit <- sampling(
    stan_model_obj,
    data = stan_data,
    chains = chains,          
    iter = iterations,          
    warmup = warmup,
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

calculate_reject_H0_prob <- function(posterior_mean, posterior_var, E_0) {
  # Calculate P(Delta < E_0 | data) (i.e., probability of !H_0)
  # H_0: Delta >= E_0 (new engine is at least E_0 points better)
  
  #pnorm: returns the value of the cdf of the normal distribution,
  # given the random variable E_0, the mean and stadard deviation of Delta.
  return(pnorm(E_0, mean = posterior_mean, sd = sqrt(posterior_var)))
}

# ==============================================================================
# Main Sequential Testing Function
# ==============================================================================

sequential_test <- function(data, engine1, engine2, E_0, alpha, beta) {
  # Sequential test for engine comparison
  # Delta = R_new - R_base (rating difference), where engine1=new, engine2=base
  # H_0: Delta >= E_0 (null hypothesis: new engine is at least E_0 points better)
  # Reject H_0 if P(Delta < E_0 | data) < alpha
  # Accept H_0 if P(Delta >= E_0 | data) > beta
  
  # Get ordered games between the two engines
  games <- get_ordered_games(data, engine1, engine2)
  
  if (is.null(games)) {
    return(list(
      engine1 = engine1,
      engine2 = engine2,
      decision = "no_games",
      games_played = 0,
      total_games_available = 0,
      final_posterior_mean = NA_real_,
      final_posterior_var = NA_real_,
      final_reject_prob = NA_real_,
      final_accept_prob = NA_real_
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
  
  while (k <= number_of_games && !reject_or_accept_H0) {
    
    # get cumulative results as vector
    results_so_far <- as.numeric(results[1:k])
    
    
    # Calculate the likelihood of the results r_1, ..., r_k
    # Update the Posterior using Stan MCMC
    # The model estimates both Delta and draw_rate simultaneously
    # draw_rate is estimated as a parameter (not fixed) with a Beta(3, 7) prior
    posterior <- update_posterior(
      prior_mean,
      prior_var, 
      results_so_far,
      stan_model_seq,
      chains=1,
      iterations=1000,
      warmup=500)
    posterior_mean <- posterior$mean
    posterior_var <- posterior$var
    
    #calculate P(Delta < E_0 | data) (i.e., probability of !H_0)
    reject_prob <- calculate_reject_H0_prob(posterior_mean, posterior_var, E_0)
    
    # Check reject/accept condition
    # (We might consider not accepting/rejecting until a few games have been played)
    if (reject_prob >= (1 - alpha)) {
      reject_or_accept_H0 <- TRUE
      decision <- "reject_H0"  # New engine is NOT significantly better (Delta < E_0)
    }else if(1.0-reject_prob >= beta){
      reject_or_accept_H0 <- TRUE
      decision <- "accept_H0"  # New engine IS significantly better (Delta >= E_0)
    }
    
    k <- k + 1
  }
  
  return(list(
    engine1 = engine1,
    engine2 = engine2,
    decision = decision,
    games_played = k - 1,
    total_games_available = number_of_games,
    final_posterior_mean = posterior_mean,
    final_posterior_var = posterior_var,
    final_reject_prob = reject_prob,
    final_accept_prob = 1.0-reject_prob
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
  
  cat(sprintf("Testing pair: %s (=new engine) vs %s (=base engine)\n", engine1, engine2))
  
  result <- sequential_test(data, engine1, engine2, E_0, alpha, beta)
  
  # Calculate scores after games played and over all games
  games <- get_ordered_games(data, engine1, engine2)
  if (!is.null(games) && nrow(games) > 0) {
    total_score_after_games_played <- sum(games$new_wins[1:result$games_played])
    total_score_all_games <- sum(games$new_wins)
  } else {
    total_score_after_games_played <- NA_real_
    total_score_all_games <- NA_real_
  }
  
  # Add scores to result object
  result$total_score_after_games_played <- total_score_after_games_played
  result$total_score_all_games <- total_score_all_games
  results_list[[length(results_list) + 1]] <- result
  
  cat(sprintf("  Decision: %s\n", result$decision))
  cat(sprintf("  Games played: %d / %d\n", result$games_played, result$total_games_available))
  cat(sprintf("  Total score after games played: %.f:%.f\n", total_score_after_games_played, result$games_played-total_score_after_games_played))
  cat(sprintf("  Total score over all games: %.f:%.f\n", total_score_all_games, result$total_games_available- total_score_all_games))
  cat(sprintf("  Final posterior mean (Delta): %.2f\n", result$final_posterior_mean))
  cat(sprintf("  Final reject prob: %.4f\n", result$final_reject_prob))
  cat(sprintf("  Final accept prob: %.4f\n\n", result$final_accept_prob))
}

# Convert results to data frame for easier analysis
results_df <- do.call(rbind, lapply(results_list, function(r) {
  # Handle NA values for variance (when no games were played)
  delta_sd <- if (!is.na(r$final_posterior_var) && r$final_posterior_var > 0) {
    sqrt(r$final_posterior_var)
  } else {
    NA_real_
  }
  
  data.frame(
    new = r$engine1,
    base = r$engine2,
    decision = r$decision,
    games_played = r$games_played,
    score = stringr::str_interp("${r$total_score_after_games_played}:${r$games_played-r$total_score_after_games_played}"),
    total_games = r$total_games_available,
    total_score = stringr::str_interp("${r$total_score_all_games}:${r$total_games_available-r$total_score_all_games}"),
    posterior_delta_mean = r$final_posterior_mean,
    posterior_delta_sd = delta_sd,
    stringsAsFactors = FALSE
  )
}))

cat("\nSummary Table:\n")
print(results_df)

