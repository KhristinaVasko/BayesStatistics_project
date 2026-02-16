# ==============================================================================
# Part 5: Sequential Testing & Manual Tuning
# ==============================================================================
#if (.Platform$OS.type == "windows") windows(width = 8, height = 10)
library(Rschach)
library(rstan)
library(stringr)

# --- HELPER FUNCTIONS (From Part 4) ---
update_posterior <- function(prior_mean, prior_var, results, 
                             stan_model_obj = stan_model_seq, chains=4, iterations=1000, warmup=500) {
  
  results <- as.numeric(results)
  n <- length(results)
  if (n == 0) {
    return(list(mean = prior_mean, var = prior_var))
  }
  
  score_vector <- array(as.numeric(results), dim = n)
  
  stan_data <- list(
    prior_mean = prior_mean,
    prior_sd = sqrt(prior_var),
    N = n,
    score = score_vector
  )
  
  fit <- sampling(
    stan_model_obj,
    data = stan_data,
    chains = chains,          
    iter = iterations,          
    warmup = warmup,
    refresh = 0,          
    control = list(adapt_delta = 0.95) # INCREASED from 0.8 to 0.95 to stop warnings
  )
  
  delta_samples <- extract(fit, pars = "delta")$delta
  
  if (is.null(delta_samples) || length(delta_samples) == 0) {
    stop("Stan sampling failed: no samples extracted. Check Stan model and data.")
  }
  
  posterior_mean <- mean(delta_samples)
  posterior_var <- var(delta_samples)
  
  return(list(mean = posterior_mean, var = posterior_var))
}

calculate_reject_H0_prob <- function(mu, var, E_0) {
  pnorm(E_0, mean = mu, sd = sqrt(var))
}

# ------------------------------------------------------------------------------

# 1. Setup & Integration
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

cat("Compiling Stan Model for Sequential Testing...\n")
stan_model_seq <- stan_model("models/sequential_test_model.stan")

# 2. Core Testing Logic
# ------------------------------------------------------------------------------
log_to_file <- function(text_line) {
  log_path <- "outputs/part5/terminal_log.txt"
  if (!dir.exists("outputs/part5")) dir.create("outputs/part5", recursive = TRUE)
  
  cat(text_line, file = log_path, append = TRUE, sep = "\n")
}

live_sequential_test <- function(params_base, params_new, book, 
                                 target_p, iteration_n,
                                 E_0 = 5,          
                                 alpha = 0.10,    
                                 beta = 0.90, 
                                 max_games = 50) {
  
  engine_base <- Engine("base", params = params_base)
  engine_new <- Engine("new", params = params_new)
  actual_val <- engine_new$params()[[target_p]]
  cat(sprintf(" [Check] Engine internally loaded %s = %.3f\n", target_p, actual_val))
  results <- numeric(0)
  decision <- "continue"
  k <- 1
  
  cat(sprintf("\nStarting Discovery Test: New vs Base (Hurdle E0=%d)\n", E_0))
  
  while (decision == "continue" && k <= max_games) {
    # FAIRNESS: Paired games
    is_new_white <- (k %% 2 != 0)
    opening_idx <- floor((k-1)/2) %% length(book) + 1
    
    game <- play.game(
      white = if(is_new_white) engine_new else engine_base,
      black = if(is_new_white) engine_base else engine_new,
      startpos = book[opening_idx],
      tc_base = 15,   # 15 seconds
      tc_inc = 0.1,     # 0.1s increment
    )
    
    score <- 0.5
    if (game$Result == "1-0") score <- if(is_new_white) 1 else 0
    if (game$Result == "0-1") score <- if(is_new_white) 0 else 1
    results <- c(results, score)
    
    posterior <- update_posterior(0, 2*(200^2), results, stan_model_seq)
    #dashboard_visualize(posterior, results, E_0, target_p, iteration_n, k)
    
    prob_better <- 1 - calculate_reject_H0_prob(posterior$mean, posterior$var, E_0)
    # ---------------------------------------------------------
    # THE IMPATIENCE RULES
    # ---------------------------------------------------------
    
    # RULE 1
    # after 20 games the probability isn't even at 40%
    if (k == 20 && prob_better < 0.40) {
      decision <- "reject_H0"
      cat("\n!!! [IMPATIENCE] Game 20: Prob too low. Stopping match. !!!\n", prob_better)
    }
    
    # RULE 2
    # If we hit 35 games and haven't crossed 60%, it's not a "Dramatic" gain
    else if (k == 35 && prob_better < 0.60) {
      decision <- "reject_H0"
      cat(sprintf("\n!!! [IMPATIENCE] Game 35: Prob too low. Stopping match. !!!\n", prob_better))    }
    
    # RULE 3: STATISTICAL CERTAINTY
    else if (prob_better > beta) {
      decision <- "accept_H0"
    } else if (prob_better < alpha) {
      decision <- "reject_H0"
    }
    
    # RULE 4: FINAL DECISION
    else if (k == max_games) {
      decision <- if(prob_better > 0.7) "accept_H0" else "reject_H0"
    }
    # ---------------------------------------------------------

    log_line <- sprintf("  G%d: Score %.1f | P(Delta >= %d) = %.3f | %s", 
                        k, score, E_0, prob_better, decision)
    cat(log_line, "\n")
    log_to_file(log_line)

    gc(verbose = FALSE)
    k <- k + 1
  }
  rm(engine_base)
  rm(engine_new)  
  return(list(decision = decision, final_mu = posterior$mean))
}
# 3. The Tuning Loop
# ------------------------------------------------------------------------------

run_manual_tuning <- function(iterations = 15) {
  current_best_params <- Engine("default")$params()
  opening_book <- readLines("data/8moves_v3.epd")
  
  for (i in 1:iterations) {
    all_params <- names(current_best_params)
    target_p <- sample(all_params, 1)
    current_val <- current_best_params[[target_p]]
    
    # --- CONTEXT-AWARE MUTATION RULES ---
    
    if (grepl("LMR", target_p)) {
      # SENSITIVE: Small multipliers (0.7x to 1.3x)
      # LMR controls depth
      multiplier <- runif(1, 0.7, 1.3)
      new_val <- if(current_val == 0) runif(1, 0.5, 1.5) else current_val * multiplier
      
    } else if (grepl("intercept", target_p, ignore.case = TRUE)) {
      # COARSE: Large multipliers (0.3x to 3.0x)
      # Intercepts (like RFP=70k) are often dead zones that need big jumps
      multiplier <- runif(1, 0.3, 3.0)
      new_val <- if(current_val == 0) runif(1, 100, 1000) else current_val * multiplier
      
    } else if (grepl("slope", target_p, ignore.case = TRUE)) {
      # DEPTH-DEPENDENT: Moderate jumps (0.5x to 2.0x)
      # Slopes need to be high enough
      multiplier <- runif(1, 0.5, 2.0)
      new_val <- if(current_val == 0) runif(1, 20, 200) else current_val * multiplier
      
    } else {
      # DEFAULT (Eval/Material): Broad Exploration
      multiplier <- runif(1, 0.4, 2.5)
      new_val <- if(current_val == 0) runif(1, 10, 50) else current_val * multiplier
    }

    candidate_params <- current_best_params
    candidate_params[[target_p]] <- max(0, new_val)

    header_line <- sprintf("\n--- ITERATION %d: Tuning %s (%.2f -> %.2f) ---", 
                       i, target_p, current_val, candidate_params[[target_p]])
    cat(header_line, "\n")
    log_to_file(header_line)  

    # Run the Impatient Test
    test_res <- live_sequential_test(
      params_base = current_best_params, 
      params_new = candidate_params, 
      book = opening_book, 
      target_p = target_p, 
      iteration_n = i,
      E_0 = 5,           
      max_games = 40,
    )
    if (test_res$decision == "accept_H0") {
      cat("SUCCESSFUL!\n")
      current_best_params <- candidate_params
    }
  }
  return(current_best_params)
}

# dashboard_visualize <- function(posterior, results, E_0, param_name, iter, game_n) {
#   par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))
  
#   # --- PLOT 1: Bayesian Posterior ---
#   mu <- posterior$mean
#   sd <- sqrt(posterior$var)
#   x <- seq(mu - 4*sd, mu + 4*sd, length.out = 200)
#   y <- dnorm(x, mu, sd)
  
#   plot(x, y, type = "l", lwd = 3, col = "#2c3e50",
#        main = sprintf("Iteration %d: Tuning %s", iter, param_name),
#        xlab = "Elo Advantage", ylab = "Density", panel.first = grid())
  
#   # Shade the Success Zone
#   abline(v = E_0, col = "#e74c3c", lty = 2, lwd = 2)
#   green_x <- x[x >= E_0]
#   polygon(c(green_x, rev(green_x)), c(dnorm(green_x, mu, sd), rep(0, length(green_x))), 
#           col = rgb(0.18, 0.8, 0.44, 0.4), border = NA)
  
#   # --- PLOT 2: Cumulative Score (The "Trend") ---
#   cum_score <- cumsum(results)
#   plot(1:game_n, cum_score, type = "b", pch = 19, col = "#3498db",
#        main = "Match Progress (Cumulative Points)",
#        xlab = "Game Number", ylab = "Total Points",
#        ylim = c(0, game_n))
#   abline(a = 0, b = 0.5, lty = 3, col = "gray") # The "Draw Line" (50% score)
#   grid()
# }
# 4. Execution
# ------------------------------------------------------------------------------
tuned_params <- run_manual_tuning(iterations = 10)

cat("\nFinal Tuned Parameters found in Part 5:\n")
print(tuned_params)
