# ==============================================================================
# VU 105.173 Bayesian Statistics - Project Task 8: STRICT FULL RUN
# ==============================================================================

rm(list = ls()) # Clean memory
library(Rschach)

# 1. SETUP ENGINES
# ==============================================================================

# Base Engine
e_base <- Engine("Base")

# Tuned Engine (Task 5 Hint parameters)
e_tuned <- Engine("Tuned")
e_tuned$set.params(list(
  NMP_intercept = 3,
  NMP_slope = 0,
  LMR_intercept = 1,
  RFP_intercept = -30,
  RFP_slope = 150,
  LMR_slope = 1.5 
))

# Optimized 1D (Task 6 parameter)
e_opt1d <- Engine("Opt1D")
e_opt1d$set.params(list(
  LMR_slope = 0.245
))

# Optimized 2D (Task 7 parameters)
e_opt2d <- Engine("Opt2D")
e_opt2d$set.params(list(
  RFP_intercept = 100,
  RFP_slope = 0
))

# Create the list of engines
engines <- list(e_base, e_tuned, e_opt1d, e_opt2d)

# 2. TOURNAMENT RUN (STRICT REQUIREMENTS)
# ==============================================================================
print("Starting FULL tournament simulation (10+0.1)...")
print("WARNING: This will take several hours. Do not close R.")

tournament_results <- play.tournament(
  engines, 
  nr_rounds = 50L,      # 50 Rounds (Required)
  tc_base = 10.0,       # 10 seconds base (Required)
  tc_inc = 0.1,         # 0.1 seconds increment (Required)
  resign_count = 5L     # Resign allowed
)
print("Tournament finished.")

# 3. DATA CLEANING
# ==============================================================================
names(tournament_results) <- tolower(names(tournament_results))

# Normalize results
if (is.character(tournament_results$result)) {
  tournament_results$score_num <- NA
  tournament_results$score_num[tournament_results$result == "1-0"] <- 1
  tournament_results$score_num[tournament_results$result == "0-1"] <- 0
  tournament_results$score_num[tournament_results$result == "1/2-1/2"] <- 0.5
  tournament_results$score_num[is.na(tournament_results$score_num)] <- 0.5
} else {
  tournament_results$score_num <- as.numeric(tournament_results$result)
}

# 4. MCMC MODEL EXECUTION
# ==============================================================================
calc_log_posterior <- function(ratings, games) {
  # Prior: Normal(2000, 200)
  log_prior <- sum(dnorm(ratings, mean = 2000, sd = 200, log = TRUE))
  
  # Likelihood
  white_names <- as.character(games$white)
  black_names <- as.character(games$black)
  scores <- games$score_num
  r_w <- ratings[white_names]
  r_b <- ratings[black_names]
  
  if (any(is.na(r_w)) || any(is.na(r_b))) return(-Inf)
  
  expected_w <- 1 / (1 + 10^((r_b - r_w) / 400))
  
  log_lik <- sum(
    ifelse(scores == 1, log(expected_w + 1e-9),
           ifelse(scores == 0, log(1 - expected_w + 1e-9),
                  log(sqrt((expected_w * (1 - expected_w)) + 1e-9)))) 
  )
  return(log_prior + log_lik)
}

run_mcmc <- function(games_data, iterations = 10000, step_size = 20) {
  n_engines <- 4
  current_ratings <- c(base=2000, tuned=2000, opt1d=2000, opt2d=2000)
  chain <- matrix(NA, nrow = iterations, ncol = n_engines)
  colnames(chain) <- c("Base", "Tuned", "Opt1D", "Opt2D") 
  
  current_log_post <- calc_log_posterior(current_ratings, games_data)
  if (is.na(current_log_post) || current_log_post == -Inf) current_log_post <- -100000 
  
  for (i in 1:iterations) {
    proposal <- current_ratings + rnorm(n_engines, mean = 0, sd = step_size)
    proposal_log_post <- calc_log_posterior(proposal, games_data)
    
    if (is.na(proposal_log_post)) proposal_log_post <- -Inf
    
    if (log(runif(1)) < (proposal_log_post - current_log_post)) {
      current_ratings <- proposal
      current_log_post <- proposal_log_post
    }
    chain[i, ] <- current_ratings
  }
  return(chain)
}

# 5. RUN & PLOT
# ==============================================================================
print("Running MCMC Sampler...")
set.seed(42)

mcmc_chain <- run_mcmc(tournament_results, iterations = 20000, step_size = 15)

# Burn-in 
burn_in <- 2000
clean_chain <- mcmc_chain[(burn_in + 1):nrow(mcmc_chain), ]

print("--- Final Estimated Elo Ratings ---")
print(sort(colMeans(clean_chain), decreasing = TRUE))

# Plotting with adjusted margins to avoid errors
par(mfrow=c(2,2), mar=c(3, 3, 2, 1)) 
plot(clean_chain[, "Base"], type="l", main="Base", ylab="Elo", col="black")
plot(clean_chain[, "Tuned"], type="l", main="Tuned", ylab="Elo", col="red")
plot(clean_chain[, "Opt1D"], type="l", main="Opt1D", ylab="Elo", col="blue")
plot(clean_chain[, "Opt2D"], type="l", main="Opt2D", ylab="Elo", col="green")

sink(file = "Task8_Final_Elo_Ratings.txt")

print("--- TASK 8 FINAL RESULTS ---")
print("Tournament Settings: 50 Rounds, Time Control: 10+0.1 (Simulated/Adjusted)")
print("---------------------------------------------------")
print("FINAL ESTIMATED ELO RATINGS (Posterior Means):")
print(sort(colMeans(clean_chain), decreasing = TRUE))
print("---------------------------------------------------")
print("DETAILED SUMMARY STATISTICS:")
print(summary(clean_chain))

sink() 