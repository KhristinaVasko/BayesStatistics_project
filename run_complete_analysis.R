# ==============================================================================
# Part 1: Complete Bayesian Elo Rating Analysis
# ==============================================================================

library(tidyverse)
library(rstan)
library(bayesplot)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

cat("========================================\n")
cat("PART 1: TOURNAMENT RANKING ANALYSIS\n")
cat("========================================\n\n")

# ==============================================================================
# STEP 1: LOAD AND PREPROCESS DATA
# ==============================================================================

cat("Step 1: Loading data...\n")
games <- read_csv("games.csv", show_col_types = FALSE)

cat("  Games loaded:", nrow(games), "\n")
cat("  Columns:", paste(names(games), collapse = ", "), "\n\n")

# Get unique engines
all_engines <- unique(c(games$white, games$black))
engine_names <- sort(all_engines)
n_engines <- length(engine_names)
engine_to_idx <- setNames(1:n_engines, engine_names)

cat("  Unique engines:", n_engines, "\n")
print(engine_names)

# Convert results to scores
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

# Prepare Stan data
stan_data <- list(
  N = nrow(games),
  K = n_engines,
  white = games$white_idx,
  black = games$black_idx,
  score = games$white_score
)

cat("\n  Stan data prepared successfully!\n\n")

# ==============================================================================
# STEP 2: COMPILE AND RUN STAN MODEL
# ==============================================================================

cat("Step 2: Compiling Stan model...\n")

model <- stan_model("elo_model.stan")

cat("Step 3: Running MCMC sampling...\n")
cat("  4 chains x 2000 iterations\n")

fit <- sampling(
  model,
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 42,
  control = list(adapt_delta = 0.95)
)

# ==============================================================================
# STEP 4: CHECK DIAGNOSTICS
# ==============================================================================

cat("\n========================================\n")
cat("DIAGNOSTICS\n")
cat("========================================\n\n")

# Check R-hat
rhats <- rhat(fit, pars = "rating_absolute")
cat("R-hat values:\n")
cat("  Max R-hat:", max(rhats, na.rm = TRUE), "\n")
if (max(rhats, na.rm = TRUE) < 1.1) {
  cat("  ✓ All R-hat < 1.1 (Good convergence!)\n\n")
} else {
  cat("  ✗ Some R-hat > 1.1 (Convergence issues!)\n\n")
}

# Check effective sample size
neff <- neff_ratio(fit, pars = "rating_absolute")
cat("Effective sample size:\n")
cat("  Min n_eff ratio:", min(neff, na.rm = TRUE), "\n")
if (min(neff, na.rm = TRUE) > 0.1) {
  cat("  ✓ All n_eff ratios > 0.1 (Good!)\n\n")
} else {
  cat("  Some n_eff ratios < 0.1 (Consider increasing iterations)\n\n")
}

# ==============================================================================
# STEP 5: EXTRACT RESULTS
# ==============================================================================

cat("========================================\n")
cat("RESULTS\n")
cat("========================================\n\n")

# Extract ratings
rating_samples <- extract(fit, pars = "rating_absolute")$rating_absolute

# Summary statistics
rating_summary <- data.frame(
  engine = engine_names,
  mean_rating = colMeans(rating_samples),
  sd_rating = apply(rating_samples, 2, sd),
  q025 = apply(rating_samples, 2, quantile, 0.025),
  q975 = apply(rating_samples, 2, quantile, 0.975)
) %>%
  arrange(desc(mean_rating)) %>%
  mutate(rank = row_number())

# Display tier list
cat("TIER LIST:\n\n")
print(rating_summary %>% select(rank, engine, mean_rating, sd_rating, q025, q975), 
      row.names = FALSE)

# ==============================================================================
# STEP 6: SUPERIORITY ANALYSIS
# ==============================================================================

cat("\n========================================\n")
cat("SUPERIORITY ANALYSIS\n")
cat("========================================\n\n")

# Calculate pairwise superiority
superiority_matrix <- matrix(0, n_engines, n_engines)
rownames(superiority_matrix) <- engine_names
colnames(superiority_matrix) <- engine_names

for (i in 1:n_engines) {
  for (j in 1:n_engines) {
    if (i != j) {
      superiority_matrix[i, j] <- mean(rating_samples[, i] > rating_samples[, j])
    }
  }
}

cat("Pairwise Superiority Probabilities:\n")
cat("(Probability that row engine > column engine)\n\n")
print(round(superiority_matrix, 3))

# Find clear superiorities (P > 0.95)
cat("\n\nClear Superiorities (P > 0.95):\n")
found_any <- FALSE
for (i in 1:(n_engines - 1)) {
  for (j in (i + 1):n_engines) {
    engine_i <- engine_names[rating_summary$rank == i]
    engine_j <- engine_names[rating_summary$rank == j]
    prob <- superiority_matrix[engine_i, engine_j]
    if (prob > 0.95) {
      cat(sprintf("  %s > %s (P = %.3f)\n", engine_i, engine_j, prob))
      found_any <- TRUE
    }
  }
}
if (!found_any) {
  cat("  (None with P > 0.95)\n")
}

# ==============================================================================
# STEP 7: VISUALIZATIONS
# ==============================================================================

cat("\n========================================\n")
cat("GENERATING VISUALIZATIONS\n")
cat("========================================\n\n")

# Plot 1: Ratings with error bars
p1 <- ggplot(rating_summary, aes(x = reorder(engine, mean_rating), y = mean_rating)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.2) +
  coord_flip() +
  labs(
    title = "Engine Ratings with 95% Credible Intervals",
    x = "Engine",
    y = "Rating"
  ) +
  theme_minimal()

ggsave("rating_plot.png", p1, width = 8, height = 6)
cat("  ✓ Saved: rating_plot.png\n")

# Plot 2: Posterior densities
rating_df <- as.data.frame(rating_samples)
colnames(rating_df) <- engine_names

rating_long <- rating_df %>%
  pivot_longer(everything(), names_to = "engine", values_to = "rating")

p2 <- ggplot(rating_long, aes(x = rating, fill = engine)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Posterior Rating Distributions",
    x = "Rating",
    y = "Density"
  ) +
  theme_minimal()

ggsave("rating_densities.png", p2, width = 10, height = 6)
cat("  ✓ Saved: rating_densities.png\n")

# Plot 3: Superiority heatmap
sup_df <- as.data.frame(superiority_matrix) %>%
  rownames_to_column("engine1") %>%
  pivot_longer(-engine1, names_to = "engine2", values_to = "probability")

p3 <- ggplot(sup_df, aes(x = engine2, y = engine1, fill = probability)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", probability)), size = 3) +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "blue",
    midpoint = 0.5, limits = c(0, 1)
  ) +
  labs(
    title = "Pairwise Superiority Probabilities",
    subtitle = "P(Row Engine > Column Engine)",
    x = "Engine", y = "Engine"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("superiority_heatmap.png", p3, width = 8, height = 6)
cat("  ✓ Saved: superiority_heatmap.png\n")

# ==============================================================================
# SAVE RESULTS
# ==============================================================================

save(rating_summary, superiority_matrix, games, stan_data,
     file = "part1_results.RData")
saveRDS(fit, "part1_fit.rds")