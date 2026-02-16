# ==============================================================================
# Task 7: 2D Bayesian Optimization of RFP Parameters
# ==============================================================================
# Jointly optimize RFP_intercept and RFP_slope using 2D Bayesian optimization
# with a separable Gaussian kernel.

if (basename(getwd()) == "src") {
  setwd("..")
} else if (!endsWith(getwd(), "Task7")) {
  setwd("Task7")
}

library(Rschach)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rstan)
library(DiceKriging)
library(gridExtra)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Output filenames
pgn_filename <- "tournament_games.pgn"
csv_filename <- "tournament_results.csv"
params_filename <- "engine_parameters.csv"

# Time control
TC_BASE <- 30   # base time in seconds
TC_INC  <- 0    # increment in seconds

# Number of positions to play (each played twice with colors flipped)
N_POSITIONS <- 15

# Fixed parameters from Task 6 optimization
FIXED_PARAMS <- list(
  NMP_intercept = 3,
  NMP_slope = 0,
  LMR_intercept = 1,
  LMR_slope = 0.405  # Optimal from Task 6
)

# Optimization bounds
BOUNDS <- list(
  intercept = c(-100, 300),
  slope = c(0, 500)
)

# Constraint safety margin
CONSTRAINT_MARGIN <- 1.0

# Load opening book
opening_book <- read.csv("8moves_v3.epd", header = FALSE)[[1]]

# Compile Stan model
cat("Compiling Stan model...\n")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
stanmodel <- stan_model("elo_model.stan")
cat("Stan model compiled successfully!\n\n")

# Load preprocessing functions
source("../src/preprocessing.R")

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Normalize design to [0,1]^2 for numerical stability
normalize_design <- function(design) {
  data.frame(
    x1 = (design$RFP_intercept + 100) / 400,  # [-100, 300] -> [0, 1]
    x2 = design$RFP_slope / 500                # [0, 500] -> [0, 1]
  )
}

# Denormalize point from [0,1]^2 back to original space
denormalize_point <- function(x1, x2) {
  data.frame(
    RFP_intercept = x1 * 400 - 100,
    RFP_slope = x2 * 500
  )
}

# Check if parameters satisfy constraint: RFP_intercept + RFP_slope > margin
is_feasible <- function(intercept, slope, margin = CONSTRAINT_MARGIN) {
  return(intercept + slope > margin)
}

# Parse engine name to extract parameters
# Format: "E_RFP-30_150" -> list(intercept=-30, slope=150)
parse_engine_name <- function(name) {
  pattern <- "E_RFP([-0-9]+)_([0-9]+)"
  matches <- regmatches(name, regexec(pattern, name))[[1]]
  if (length(matches) < 3) {
    return(list(intercept = NA, slope = NA))
  }
  list(
    intercept = as.numeric(matches[2]),
    slope = as.numeric(matches[3])
  )
}

# Run Elo model on tournament results (reused from Task 6)
run_elo_model <- function(results_csv, stan_model) {
  cat("Running Elo rating model...\n")

  games <- read_csv(results_csv, show_col_types = FALSE)
  engine_names <- sort(get_players(results_csv))
  n_engines <- length(engine_names)
  engine_to_idx <- setNames(1:n_engines, engine_names)

  games <- games %>%
    mutate(
      white_score = case_when(
        result == "1-0" ~ 1.0,
        result == "1/2-1/2" ~ 0.5,
        result == "0-1" ~ 0.0
      ),
      white_idx = engine_to_idx[white],
      black_idx = engine_to_idx[black]
    )

  stan_data <- list(
    N = nrow(games),
    K = n_engines,
    white = games$white_idx,
    black = games$black_idx,
    score = games$white_score
  )

  cat("  Running MCMC sampling...\n")
  fit <- sampling(
    stan_model,
    data = stan_data,
    chains = 4,
    iter = 2000,
    warmup = 1000,
    control = list(adapt_delta = 0.95),
    refresh = 0  # Suppress MCMC progress
  )

  # Use rstan::extract to avoid conflict with tidyr
  rating_samples <- rstan::extract(fit, pars = "rating_absolute")$rating_absolute
  rating_summary <- data.frame(
    X = engine_names,
    Y = colMeans(rating_samples),
    epsilon = apply(rating_samples, 2, sd)
  )

  cat("  Elo model complete.\n\n")
  return(rating_summary)
}

# Calculate Expected Improvement (reused from Task 6)
calculate_expected_improvement <- function(pred, y_best) {
  mu <- pred$mean
  sigma <- pred$sd
  sigma <- pmax(sigma, 1e-9)  # Avoid division by zero

  # Calculate Expected Improvement
  Z <- (mu - y_best) / sigma
  ei <- (mu - y_best) * pnorm(Z) + sigma * dnorm(Z)

  return(ei)
}

# Find maximum EI using hybrid approach (grid + local optimization)
find_max_ei <- function(gp_model, y_best, bounds) {
  cat("  Finding maximum Expected Improvement...\n")

  # Step 1: Coarse grid search (51x51 = 2,601 points)
  intercept_grid <- seq(bounds$intercept[1], bounds$intercept[2], length.out = 51)
  slope_grid <- seq(bounds$slope[1], bounds$slope[2], length.out = 51)

  coarse_grid <- expand.grid(
    RFP_intercept = intercept_grid,
    RFP_slope = slope_grid
  ) %>% filter(is_feasible(RFP_intercept, RFP_slope))

  # Predict on coarse grid
  coarse_norm <- normalize_design(coarse_grid)
  pred_coarse <- predict(gp_model, newdata = coarse_norm, type = "UK")
  ei_coarse <- calculate_expected_improvement(pred_coarse, y_best)

  # Find top candidate from grid
  best_grid_idx <- which.max(ei_coarse)
  best_grid <- coarse_grid[best_grid_idx, ]
  best_ei_grid <- ei_coarse[best_grid_idx]

  cat(sprintf("    Grid search best: intercept=%.1f, slope=%.1f, EI=%.4f\n",
              best_grid$RFP_intercept, best_grid$RFP_slope, best_ei_grid))

  # Step 2: Local optimization from best grid point
  neg_ei_fn <- function(x) {
    # x = c(x1, x2) in normalized space [0,1]^2
    if (x[1] < 0 || x[1] > 1 || x[2] < 0 || x[2] > 1) {
      return(1e10)  # Penalty for out of bounds
    }

    # Check feasibility in original space
    original <- denormalize_point(x[1], x[2])
    if (!is_feasible(original$RFP_intercept, original$RFP_slope)) {
      return(1e10)  # Penalty for infeasible points
    }

    # Predict and calculate EI
    pred <- predict(gp_model,
                   newdata = data.frame(x1 = x[1], x2 = x[2]),
                   type = "UK")
    ei <- calculate_expected_improvement(pred, y_best)
    return(-ei)  # Minimize negative EI
  }

  # Starting point from grid (in normalized space)
  x0 <- c(
    (best_grid$RFP_intercept + 100) / 400,
    best_grid$RFP_slope / 500
  )

  # Optimize with box constraints
  opt_result <- optim(
    par = x0,
    fn = neg_ei_fn,
    method = "L-BFGS-B",
    lower = c(0, 0),
    upper = c(1, 1)
  )

  # Return best point in original space
  best_point <- denormalize_point(opt_result$par[1], opt_result$par[2])
  best_ei_opt <- -opt_result$value

  cat(sprintf("    Optimization best: intercept=%.1f, slope=%.1f, EI=%.4f\n",
              best_point$RFP_intercept, best_point$RFP_slope, best_ei_opt))

  # Verify against grid (in case optim failed)
  if (best_ei_opt < best_ei_grid * 0.99) {
    cat("    Warning: optim failed to improve over grid search, using grid result\n")
    return(best_grid)
  }

  return(best_point)
}

# Plot 2D Gaussian Process with 3-panel contour plots
plot_gaussian_process_2d <- function(gp_model, D, bounds, iteration, y_best) {
  cat(sprintf("  Creating visualization for iteration %d...\n", iteration))

  # Create prediction grid (101x101 for smooth contours)
  intercept_seq <- seq(bounds$intercept[1], bounds$intercept[2], length.out = 101)
  slope_seq <- seq(bounds$slope[1], bounds$slope[2], length.out = 101)

  grid_full <- expand.grid(
    RFP_intercept = intercept_seq,
    RFP_slope = slope_seq
  ) %>% filter(is_feasible(RFP_intercept, RFP_slope))

  # Predict
  grid_norm <- normalize_design(grid_full)
  pred <- predict(gp_model, newdata = grid_norm, type = "UK")

  # Calculate EI
  ei_vals <- calculate_expected_improvement(pred, y_best)

  # Prepare data
  plot_data <- grid_full %>%
    mutate(
      mean = pred$mean,
      sd = pred$sd,
      ei = ei_vals
    )

  # Parse observed points from D
  observed_points <- D %>%
    mutate(
      params = lapply(X, parse_engine_name),
      intercept = sapply(params, function(p) p$intercept),
      slope = sapply(params, function(p) p$slope)
    ) %>%
    select(intercept, slope, Y)

  # Create three subplots
  # Plot 1: GP Mean (predicted Elo)
  p1 <- ggplot(plot_data, aes(x = RFP_intercept, y = RFP_slope, z = mean)) +
    geom_contour_filled(bins = 15) +
    geom_point(data = observed_points, aes(x = intercept, y = slope),
               color = "red", size = 3, inherit.aes = FALSE) +
    labs(title = "GP Mean (Predicted Elo)",
         x = "RFP Intercept", y = "RFP Slope") +
    theme_minimal() +
    theme(legend.position = "right")

  # Plot 2: GP Std Dev (uncertainty)
  p2 <- ggplot(plot_data, aes(x = RFP_intercept, y = RFP_slope, z = sd)) +
    geom_contour_filled(bins = 15) +
    geom_point(data = observed_points, aes(x = intercept, y = slope),
               color = "red", size = 3, inherit.aes = FALSE) +
    labs(title = "GP Std Dev (Uncertainty)",
         x = "RFP Intercept", y = "RFP Slope") +
    theme_minimal() +
    theme(legend.position = "right")

  # Plot 3: Expected Improvement
  p3 <- ggplot(plot_data, aes(x = RFP_intercept, y = RFP_slope, z = ei)) +
    geom_contour_filled(bins = 15) +
    geom_point(data = observed_points, aes(x = intercept, y = slope),
               color = "red", size = 3, inherit.aes = FALSE) +
    labs(title = "Expected Improvement",
         x = "RFP Intercept", y = "RFP Slope") +
    theme_minimal() +
    theme(legend.position = "right")

  # Combine
  combined_plot <- grid.arrange(p1, p2, p3, ncol = 3,
                                top = paste0("Iteration ", iteration))

  # Save
  filename <- sprintf("iteration_%d_2d.png", iteration)
  ggsave(filename = filename,
         plot = combined_plot, width = 18, height = 6, dpi = 300)

  cat(sprintf("    Saved visualization to %s\n", filename))
}

# ==============================================================================
# STEP 1: INITIAL DESIGN
# ==============================================================================

cat("========================================\n")
cat("TASK 7: 2D BAYESIAN OPTIMIZATION\n")
cat("========================================\n\n")

cat("Step 1: Creating initial design (4x5 grid)...\n")

# Generate initial grid
intercept_vals <- seq(-100, 300, length.out = 5)  # [-100, 0, 100, 200, 300]
slope_vals <- seq(0, 500, length.out = 4)         # [0, 166.7, 333.3, 500]

initial_design <- expand.grid(
  RFP_intercept = intercept_vals,
  RFP_slope = slope_vals
) %>% filter(is_feasible(RFP_intercept, RFP_slope))

cat(sprintf("  Created %d feasible initial points\n", nrow(initial_design)))
cat("  Initial design:\n")
print(initial_design)
cat("\n")

# Save initial design for reference
write_csv(initial_design, "initial_design.csv")

# ==============================================================================
# STEP 2: CREATE ENGINE INSTANCES
# ==============================================================================

cat("Step 2: Creating engine instances...\n")

engines <- vector("list", nrow(initial_design))
engine_metadata <- data.frame(
  engine_name = character(nrow(initial_design)),
  RFP_intercept = numeric(nrow(initial_design)),
  RFP_slope = numeric(nrow(initial_design)),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(initial_design)) {
  current_intercept <- initial_design$RFP_intercept[i]
  current_slope <- initial_design$RFP_slope[i]

  # Create engine name encoding both parameters
  engine_name <- sprintf("E_RFP%d_%d",
                        round(current_intercept),
                        round(current_slope))

  current_params <- c(
    FIXED_PARAMS,
    list(
      RFP_intercept = current_intercept,
      RFP_slope = current_slope
    )
  )

  # Create engine
  e <- Engine(engine_name)
  e$set.params(current_params)
  engines[[i]] <- e

  # metadata
  engine_metadata$engine_name[i] <- engine_name
  engine_metadata$RFP_intercept[i] <- current_intercept
  engine_metadata$RFP_slope[i] <- current_slope

  cat(sprintf("  Created engine %d/%d: %s\n", i, nrow(initial_design), engine_name))
}

cat("\n")

# Save engine metadata
write_csv(engine_metadata, params_filename)

# ==============================================================================
# STEP 3: INITIAL TOURNAMENT
# ==============================================================================

cat("Step 3: Running initial round-robin tournament...\n")
cat(sprintf("  %d engines, %d positions, time control %d+%d\n",
            length(engines), N_POSITIONS, TC_BASE, TC_INC))
cat("  This will take approximately 1.5-2 hours...\n\n")

# Generate random opening positions
tournament_openings <- opening_book[sample(length(opening_book), N_POSITIONS)]

# Play tournament
games <- play.tournament(
  engines,
  book = tournament_openings,
  nr_rounds = N_POSITIONS,
  repeated = TRUE,
  tc_base = TC_BASE *  1000000,  # microseconds
  tc_inc = TC_INC *  1000000,
  verbose = 1
)

# Save to PGN
pgn(games, file = pgn_filename)
cat(sprintf("\nSaved games to %s\n", pgn_filename))

# Parse PGN and create CSV
lines <- readLines(pgn_filename)
whites <- gsub('\\[White "|"\\]', "", grep('\\[White ".*"\\]', lines, value = TRUE))
blacks <- gsub('\\[Black "|"\\]', "", grep('\\[Black ".*"\\]', lines, value = TRUE))
results <- gsub('\\[Result "|"\\]', "", grep('\\[Result ".*"\\]', lines, value = TRUE))
tc_val <- paste0(TC_BASE, "+", TC_INC)
time_controls <- rep(tc_val, length(results))

results_df <- data.frame(
  white = whites,
  black = blacks,
  result = results,
  timecontrol = time_controls,
  stringsAsFactors = FALSE
)
write.csv(results_df, csv_filename, row.names = FALSE, quote = FALSE)
cat(sprintf("Saved results to %s\n\n", csv_filename))

# ==============================================================================
# STEP 4: INITIAL ELO MODEL AND GP FIT
# ==============================================================================

cat("Step 4: Fitting initial Gaussian Process...\n")

# Run Elo model
D <- run_elo_model(csv_filename, stanmodel)

# Extract parameters from engine names
param_values <- D %>%
  mutate(
    params = lapply(X, parse_engine_name),
    RFP_intercept = sapply(params, function(p) p$intercept),
    RFP_slope = sapply(params, function(p) p$slope)
  )

design_matrix <- param_values %>%
  select(RFP_intercept, RFP_slope)

cat("  Ratings summary:\n")
print(data.frame(
  Engine = param_values$X,
  Intercept = param_values$RFP_intercept,
  Slope = param_values$RFP_slope,
  Rating = round(param_values$Y, 1),
  SD = round(param_values$epsilon, 1)
))
cat("\n")

# Fit 2D Gaussian Process
cat("  Fitting 2D GP with anisotropic Gaussian kernel...\n")
gp_model <- km(
  design = normalize_design(design_matrix),
  response = D$Y,
  covtype = "gauss",
  noise.var = pmax(D$epsilon^2, 1e-6),  # Add noise floor
  upper = c(0.3, 0.3)  # Anisotropic length-scale bounds
)

cat("  GP model summary:\n")
print(gp_model)
cat("\n")

# Create initial visualization
y_best <- max(D$Y)
plot_gaussian_process_2d(gp_model, D, BOUNDS, iteration = 0, y_best)

cat("\nInitial setup complete!\n")
cat(sprintf("Current best rating: %.2f Elo\n", y_best))
best_idx <- which.max(D$Y)
cat(sprintf("Best parameters: intercept=%.1f, slope=%.1f\n\n",
            param_values$RFP_intercept[best_idx],
            param_values$RFP_slope[best_idx]))

# ==============================================================================
# STEP 5: OPTIMIZATION LOOP
# ==============================================================================

cat("========================================\n")
cat("STARTING OPTIMIZATION LOOP\n")
cat("========================================\n\n")

# Set maximum iterations for automatic execution
MAX_ITERATIONS <- 3  # Change this to 2 if you want faster execution

iteration <- 1

while(TRUE) {
  cat(sprintf("\n=== Iteration %d ===\n", iteration))

  # Find current best
  y_best <- max(D$Y)
  best_idx <- which.max(D$Y)
  best_params <- list(
    intercept = param_values$RFP_intercept[best_idx],
    slope = param_values$RFP_slope[best_idx],
    rating = D$Y[best_idx]
  )

  cat(sprintf("Current best: intercept=%.1f, slope=%.1f, rating=%.2f\n",
              best_params$intercept, best_params$slope, best_params$rating))

  # Find next candidate (maximize EI)
  candidate <- find_max_ei(gp_model, y_best, BOUNDS)

  # Calculate EI and PI for the candidate
  candidate_norm <- normalize_design(candidate)
  pred_candidate <- predict(gp_model, newdata = candidate_norm, type = "UK")
  ei <- calculate_expected_improvement(pred_candidate, y_best)
  sigma <- max(pred_candidate$sd, 1e-9)
  Z <- (pred_candidate$mean - y_best) / sigma
  pi <- pnorm(Z)

  cat(sprintf("\nProposed candidate: intercept=%.1f, slope=%.1f\n",
              candidate$RFP_intercept, candidate$RFP_slope))
  cat(sprintf("Expected Improvement: %.4f\n", ei))
  cat(sprintf("Probability of Improvement: %.2f%%\n", pi * 100))

  # Check stopping criteria
  if (pi < 0.01) {
    cat("\nLow probability of improvement (<1%) - stopping optimization!\n")
    break
  }

  # Check iteration limit
  if (iteration > MAX_ITERATIONS) {
    cat(sprintf("\nReached maximum iterations (%d) - stopping optimization!\n", MAX_ITERATIONS))
    break
  }

  cat("\nProceeding automatically...\n")

  # Check if candidate already exists (within tolerance)
  if (any(abs(param_values$RFP_intercept - candidate$RFP_intercept) < 1 &
          abs(param_values$RFP_slope - candidate$RFP_slope) < 1)) {
    cat("Candidate already explored - stopping optimization!\n")
    break
  }

  # Select diverse opponents (best, 2nd best, median, worst)
  D_sorted <- D[order(D$Y, decreasing = TRUE), ]
  param_values_sorted <- param_values[order(param_values$Y, decreasing = TRUE), ]
  n_engines <- nrow(D_sorted)

  opponent_indices <- c(
    1,                           # Best
    2,                           # 2nd best
    ceiling(n_engines / 2),      # Median
    n_engines                    # Worst
  )

  # Using nr_gauntlet with equal rounds per opponent
  rounds_per_opponent <- 6  # Total: 6×4 = 24 rounds (similar to old 10+7+4+2 = 23)

  cat(sprintf("\nSelected opponents (gauntlet format):\n"))
  for (i in seq_along(opponent_indices)) {
    idx <- opponent_indices[i]
    cat(sprintf("  %d. %s (rating=%.1f)\n",
                i, D_sorted$X[idx], D_sorted$Y[idx]))
  }
  cat(sprintf("  → %d rounds per opponent (%d total games)\n",
              rounds_per_opponent, rounds_per_opponent * length(opponent_indices) * 2))

  # Initialize new engine
  new_engine_name <- sprintf("E_RFP%d_%d",
                             round(candidate$RFP_intercept),
                             round(candidate$RFP_slope))
  new_params <- c(
    FIXED_PARAMS,
    list(
      RFP_intercept = candidate$RFP_intercept,
      RFP_slope = candidate$RFP_slope
    )
  )
  new_engine <- Engine(new_engine_name)
  new_engine$set.params(new_params)

  cat(sprintf("\nCreated new engine: %s\n", new_engine_name))

  # Create all opponent engines
  cat("Initializing opponent engines...\n")
  opponent_engines <- vector("list", length(opponent_indices))
  for (i in seq_along(opponent_indices)) {
    opponent_idx <- opponent_indices[i]
    opponent_params <- param_values_sorted[opponent_idx, ]

    opponent_engines[[i]] <- Engine(D_sorted$X[opponent_idx])
    opponent_engines[[i]]$set.params(c(
      FIXED_PARAMS,
      list(
        RFP_intercept = opponent_params$RFP_intercept,
        RFP_slope = opponent_params$RFP_slope
      )
    ))
  }

  # Generate random opening positions
  current_openings <- opening_book[sample(length(opening_book), rounds_per_opponent)]

  # Play gauntlet tournament (new_engine vs all opponents at once)
  cat(sprintf("Playing gauntlet tournament (%d rounds)...\n", rounds_per_opponent))
  match_games <- play.tournament(
    c(list(new_engine), opponent_engines),
    book = current_openings,
    nr_rounds = rounds_per_opponent,
    nr_gauntlet = 1L,  # Only first engine (new_engine) plays vs all others
    repeated = TRUE,
    tc_base = TC_BASE * 1000000,
    tc_inc = TC_INC * 1000000,
    verbose = 0  # Suppress output
  )

  # Append games to PGN
  temp_pgn <- "temp_match_games.pgn"
  pgn(match_games, file = temp_pgn)
  file.append(pgn_filename, temp_pgn)
  unlink(temp_pgn)

  cat("  Gauntlet complete!\n")

  # Update CSV
  cat("\nUpdating results CSV...\n")
  lines <- readLines(pgn_filename)
  whites <- gsub('\\[White "|"\\]', "", grep('\\[White ".*"\\]', lines, value = TRUE))
  blacks <- gsub('\\[Black "|"\\]', "", grep('\\[Black ".*"\\]', lines, value = TRUE))
  results <- gsub('\\[Result "|"\\]', "", grep('\\[Result ".*"\\]', lines, value = TRUE))
  tc_val <- paste0(TC_BASE, "+", TC_INC)
  time_controls <- rep(tc_val, length(results))

  results_df <- data.frame(
    white = whites,
    black = blacks,
    result = results,
    timecontrol = time_controls,
    stringsAsFactors = FALSE
  )
  write.csv(results_df, csv_filename, row.names = FALSE, quote = FALSE)

  # Re-run Elo Model
  D <- run_elo_model(csv_filename, stanmodel)

  # Update parameter values
  param_values <- D %>%
    mutate(
      params = lapply(X, parse_engine_name),
      RFP_intercept = sapply(params, function(p) p$intercept),
      RFP_slope = sapply(params, function(p) p$slope)
    )

  design_matrix <- param_values %>%
    select(RFP_intercept, RFP_slope)

  # Update Gaussian Process
  cat("Updating Gaussian Process...\n")
  gp_model <- km(
    design = normalize_design(design_matrix),
    response = D$Y,
    covtype = "gauss",
    noise.var = pmax(D$epsilon^2, 1e-6),
    upper = c(0.3, 0.3)
  )

  # Visualize
  y_best <- max(D$Y)
  plot_gaussian_process_2d(gp_model, D, BOUNDS, iteration, y_best)

  iteration <- iteration + 1
}

# ==============================================================================
# FINAL ANALYSIS
# ==============================================================================

cat("\n========================================\n")
cat("OPTIMIZATION COMPLETE\n")
cat("========================================\n\n")

# Find optimal parameters
best_idx <- which.max(D$Y)
optimal_params <- list(
  intercept = param_values$RFP_intercept[best_idx],
  slope = param_values$RFP_slope[best_idx],
  rating = D$Y[best_idx],
  sd = D$epsilon[best_idx]
)

cat("Optimal Parameters:\n")
cat(sprintf("  RFP_intercept: %.2f\n", optimal_params$intercept))
cat(sprintf("  RFP_slope: %.2f\n", optimal_params$slope))
cat(sprintf("  Estimated Elo: %.1f ± %.1f\n", optimal_params$rating, optimal_params$sd))
cat("\n")

cat("Complete Parameter Set for Task 8:\n")
cat("  NMP_intercept = 3\n")
cat("  NMP_slope = 0\n")
cat("  LMR_intercept = 1\n")
cat(sprintf("  LMR_slope = %.3f  (from Task 6)\n", FIXED_PARAMS$LMR_slope))
cat(sprintf("  RFP_intercept = %.2f  (optimized)\n", optimal_params$intercept))
cat(sprintf("  RFP_slope = %.2f  (optimized)\n", optimal_params$slope))
cat("\n")

# Save final results
final_results <- list(
  optimal_params = optimal_params,
  all_ratings = param_values,
  n_iterations = iteration - 1,
  fixed_params = FIXED_PARAMS
)
save(final_results, file = "final_results.RData")
cat("Saved final results to final_results.RData\n")
