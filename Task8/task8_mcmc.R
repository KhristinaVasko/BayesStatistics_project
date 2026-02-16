# ==============================================================================
# VU 105.173 Bayesian Statistics - Project Task 8: FINAL FULL RUN
# ==============================================================================
# Settings: 50 Rounds | 10+0.1s Time Control | Plots visible

rm(list = ls()) # Clear memory

# ==============================================================================
# PART 0: SETUP
# ==============================================================================
if (!require("Rcpp")) {
  install.packages("Rcpp")
  library(Rcpp)
}

if (!require("Rschach")) {
  # Check if file exists manually
  rschach_file <- "Rschach_1.0.tar.gz"
  if (file.exists(rschach_file)) {
    print("Installing Rschach from source...")
    install.packages(rschach_file, repos = NULL, type = "source")
    library(Rschach)
  } else {
    stop("ERROR: 'Rschach_1.0.tar.gz' not found. Please put it in the folder.")
  }
}

# ==============================================================================
# PART 1: SETUP ENGINES
# ==============================================================================

# --- ENGINE 1: Base (Defaults) ---
e_base <- Engine("Base")
e_base$set.params(list(
  NMP_intercept = 3, NMP_slope = 0,
  LMR_intercept = 1, LMR_slope = 0,
  RFP_intercept = -30, RFP_slope = 150
))

# --- ENGINE 2: Tuned (Values from your terminal_log.txt) ---
e_tuned <- Engine("Tuned")
e_tuned$set.params(list(
  NMP_minDepth = 10.80193,
  NMP_intercept = 4.198173,
  NMP_slope = 156.8046,
  LMR_intercept = 0,
  LMR_slope = 0.7234632,
  RFP_intercept = 30098.72,
  RFP_slope = 0,
  ageing = 4.094189,
  bigDelta = 950
))

# --- ENGINE 3: Opt1D (Best from Task 6) ---
e_opt1d <- Engine("Opt1D")
e_opt1d$set.params(list(
  NMP_intercept = 3, NMP_slope = 0,
  LMR_intercept = 1,
  RFP_intercept = -30, RFP_slope = 150,
  LMR_slope = 0.405 
))

# --- ENGINE 4: Opt2D (Best from Task 7) ---
e_opt2d <- Engine("Opt2D")
e_opt2d$set.params(list(
  NMP_intercept = 3, NMP_slope = 0,
  LMR_intercept = 1,
  LMR_slope = 0.405,      
  RFP_intercept = 100.00, 
  RFP_slope = 69.00       
))

engines <- list(e_base, e_tuned, e_opt1d, e_opt2d)

# ==============================================================================
# PART 2: TOURNAMENT RUN (FULL 50 ROUNDS)
# ==============================================================================
print("Starting Task 8 Tournament (FULL)...")
print("Settings: 50 Rounds, Time Control 10+0.1s")
print("NOTE: This will take approx. 2-4 hours.")

# File Detection
if (file.exists("8moves_v3.txt")) {
  book_file <- "8moves_v3.txt"
} else if (file.exists("8moves_v3.epd")) {
  book_file <- "8moves_v3.epd"
} else {
  book_file <- NULL
}

if (!is.null(book_file)) {
  openings <- readLines(book_file)
  safe_openings <- sample(openings, 50 * 12, replace = TRUE) 
} else {
  safe_openings <- rep("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 600)
}

# Run Tournament
games_obj <- play.tournament(
  engines, 
  book = safe_openings,
  nr_rounds = 50L,      # STRICT REQUIREMENT: 50 Rounds
  tc_base = 10.0,       # STRICT REQUIREMENT: 10s
  tc_inc = 0.1,         # STRICT REQUIREMENT: 0.1s
  resign_count = 5L,    
  verbose = 1
)
print("Tournament finished.")

# ==============================================================================
# PART 3: DATA PARSING
# ==============================================================================
pgn_file <- "task8_final_full.pgn"
pgn(games_obj, file = pgn_file)

lines <- readLines(pgn_file)
whites  <- gsub('\\[White "|"\\]', "", grep('\\[White ".*"\\]', lines, value = TRUE))
blacks  <- gsub('\\[Black "|"\\]', "", grep('\\[Black ".*"\\]', lines, value = TRUE))
results <- gsub('\\[Result "|"\\]', "", grep('\\[Result ".*"\\]', lines, value = TRUE))

tournament_results <- data.frame(
  white = whites,
  black = blacks,
  result = results,
  stringsAsFactors = FALSE
)

# Convert results
tournament_results$score_num <- sapply(tournament_results$result, function(x) {
  if(x == "1-0") return(1)
  if(x == "1/2-1/2") return(0.5)
  return(0)
})

# ==============================================================================
# PART 4: MCMC SAMPLER
# ==============================================================================

calc_log_posterior <- function(ratings, games) {
  # Prior: Normal(2000, 200)
  log_prior <- sum(dnorm(ratings, mean = 2000, sd = 200, log = TRUE))
  
  names_map <- setNames(seq_along(ratings), names(ratings))
  idx_w <- names_map[games$white]
  idx_b <- names_map[games$black]
  
  r_w <- ratings[idx_w]
  r_b <- ratings[idx_b]
  
  # Likelihood
  expected_w <- 1 / (1 + 10^((r_b - r_w) / 400))
  expected_w <- pmax(pmin(expected_w, 0.999999), 0.000001)
  
  scores <- games$score_num
  log_lik <- sum(scores * log(expected_w) + (1 - scores) * log(1 - expected_w))
  
  return(log_prior + log_lik)
}

run_mcmc <- function(games_data, iterations = 30000, step_size = 15) {
  engine_names <- c("Base", "Tuned", "Opt1D", "Opt2D")
  current_ratings <- setNames(rep(2000, 4), engine_names)
  
  n_engines <- 4
  chain <- matrix(NA, nrow = iterations, ncol = n_engines)
  colnames(chain) <- engine_names
  
  current_log_post <- calc_log_posterior(current_ratings, games_data)
  
  for (i in 1:iterations) {
    proposal <- current_ratings + rnorm(n_engines, mean = 0, sd = step_size)
    proposal_log_post <- calc_log_posterior(proposal, games_data)
    
    if (log(runif(1)) < (proposal_log_post - current_log_post)) {
      current_ratings <- proposal
      current_log_post <- proposal_log_post
    }
    chain[i, ] <- current_ratings
  }
  return(chain)
}

# ==============================================================================
# PART 5: EXECUTION, PLOTTING & OUTPUT
# ==============================================================================
print("Running MCMC Sampler...")
set.seed(42)

mcmc_chain <- run_mcmc(tournament_results, iterations = 30000, step_size = 15)

# Burn-in
burn_in <- 5000
clean_chain <- mcmc_chain[(burn_in + 1):nrow(mcmc_chain), ]

# Final Estimates
final_means <- sort(colMeans(clean_chain), decreasing = TRUE)
prob_opt2d_best <- mean(apply(clean_chain, 1, function(x) names(which.max(x)) == "Opt2D"))

print("--- Final Estimated Elo Ratings ---")
print(final_means)

# --- PLOTTING (SCREEN + FILE) ---

# 1. Plot to Screen (Plots Pane)
par(mfrow=c(2,2), mar=c(3, 3, 2, 1)) 
for(eng in colnames(clean_chain)) {
  plot(clean_chain[, eng], type="l", main=eng, ylab="Elo", col="blue")
}

# 2. Save to File
png("Task8_Final_Plot_Full.png", width=800, height=600)
par(mfrow=c(2,2), mar=c(3, 3, 2, 1)) 
for(eng in colnames(clean_chain)) {
  plot(clean_chain[, eng], type="l", main=eng, ylab="Elo", col="blue")
}
dev.off() # Close the file device

# Save Text Results
sink(file = "Task8_Final_Elo_Ratings_Full.txt")
print("--- TASK 8 FINAL RESULTS (FULL RUN) ---")
print("Tournament Settings: 50 Rounds, Time Control: 10+0.1")
print("---------------------------------------------------")
print("FINAL ESTIMATED ELO RATINGS (Posterior Means):")
print(final_means)
print("---------------------------------------------------")
print(paste("Probability Opt2D is Best:", round(prob_opt2d_best*100, 2), "%"))
print("---------------------------------------------------")
print("SUMMARY STATISTICS:")
print(summary(clean_chain))
sink()

print("DONE! Plot is visible in 'Plots' pane AND saved as 'Task8_Final_Plot_Full.png'.")