# ==============================================================================
# Part 2: Time Control Dependent Elo Ratings - EXTENDING PART 1
# ==============================================================================

if (basename(getwd()) == "src") {
  setwd("..")
}

library(tidyverse)
library(rstan)
library(bayesplot) 

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ==============================================================================
# STEP 1: LOAD DATA AND CALCULATE TIME CONTROLS
# ==============================================================================

games <- read_csv("data/games.csv", show_col_types = FALSE)

# parse time control and calculate tc = time + 40*increment
time_values <- as.numeric(str_extract(games$timecontrol, "^[0-9]+"))
increment_values <- as.numeric(str_replace(games$timecontrol, "^[0-9]+\\+", ""))
increment_values[is.na(increment_values)] <- 0
tc_values <- time_values + 40 * increment_values

# calculate mean time, std and create centered values
tc_mean <- mean(tc_values, na.rm = TRUE)
tc_sd <- sd(tc_values, na.rm = TRUE)
time_centered_vector <- (tc_values - tc_mean) / tc_sd

cat("Time control - Mean:", round(tc_mean, 1), "SD:", round(tc_sd, 1), "seconds\n")

cat("Time centered vector:\n")
cat("  Length:", length(time_centered_vector), "\n")
cat("  Range:", round(min(time_centered_vector), 1), "to", round(max(time_centered_vector), 1), "seconds\n")
cat("  Sample values:", paste(head(time_centered_vector), collapse = ", "), "\n\n")

# get unique engines
source("src/preprocessing.R")
engine_names <- sort(get_players("data/games.csv"))
n_engines <- length(engine_names)
engine_to_idx <- setNames(1:n_engines, engine_names)

# ==============================================================================
# STEP 2: EXTEND PART 1 MODEL WITH TIME CONTROL
# ==============================================================================

# Load Part 1 results
list_of_names <- load("outputs/part1/part1_results.RData")
#print(list_of_names)
# Prepare Stan data
stan_data <- list(
  N = nrow(games),
  K = n_engines,
  white = games$white_idx,
  black = games$black_idx,
  score = games$white_score,
  time_centered = as.numeric(time_centered_vector),
  mean_time = tc_mean,
  tc_sd = tc_sd
)

# ==============================================================================
# STEP 3: STAN MODEL THAT EXTENDS PART 1
# ==============================================================================

cat("Compiling model...\n")
model <- stan_model("models/elo_time_model.stan")

cat("Running MCMC sampling...\n")
fit_extended <- sampling(
  model,
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 25,
  control = list(adapt_delta = 0.95)
)

cat("Model completed successfully!\n\n")

## ==============================================================================
## STEP 3.1: DIAGNOSTICS AND CONVERGENCE CHECK 
## ==============================================================================

cat("\n========================================\n")
cat("DIAGNOSTICS\n")
cat("========================================\n\n")

# Check R-hat
rhats <- rhat(fit_extended, pars = c("base_rating", "time_slope"))
cat("R-hat values:\n")
cat(" Max R-hat:", max(rhats, na.rm = TRUE), "\n")
if (max(rhats, na.rm = TRUE) < 1.05) {
  cat("  ✓ All R-hat < 1.1 (Good convergence!)\n\n")
} else {
  cat("  ✗ Some R-hat > 1.1 (Convergence issues!)\n\n")
}

# Check effective sample size
neff_ratio_min <- min(neff_ratio(fit_extended, pars = c("base_rating", "time_slope")), na.rm = TRUE)
cat("Effective sample size ratio:\n")
cat(" Min n_eff ratio:", neff_ratio_min, "\n")
if (neff_ratio_min > 0.1) {
  cat("  ✓ All n_eff ratios > 0.1 (Good!)\n\n")
} else {
  cat("  Some n_eff ratios < 0.1 (Consider increasing iterations)\n\n")
}


# ==============================================================================
# STEP 4: COMPARE WITH PART 1 RESULTS
# ==============================================================================

cat("Comparing with Part 1 results:\n")

samples_base <- extract(fit_extended, pars = "base_rating")$base_rating
samples_slope <- extract(fit_extended, pars = "time_slope")$time_slope

base_summary <- data.frame(
  engine = engine_names,
  mean_rating = colMeans(samples_base),
  sd_rating = apply(samples_base, 2, sd)
)

comparison <- left_join(
  base_summary %>% rename(part2_rating = mean_rating, part2_sd = sd_rating),
  rating_summary %>% select(engine, part1_rating = mean_rating, part1_sd = sd_rating),
  by = "engine"
) %>%
  mutate(
    difference = part2_rating - part1_rating,
    correlation = cor(part2_rating, part1_rating)
  )

print(comparison)
# ==============================================================================
# STEP 5: VISUALIZATIONS AND TIER LISTS
# ==============================================================================

cat("Generating visualizations...\n")

required_tc_values <- c(
  "15+0" = 15 + 40*0,
  "60+0" = 60 + 40*0,
  "180+2" = 180 + 40*2,
  "600+0" = 600 + 40*0
)

# create time control plot
tc_range <- seq(30, 1000, length.out = 100)
tc_range_centered <- (tc_range - tc_mean) / tc_sd


plot_data <- data.frame()

for (engine_idx in 1:n_engines) {
  engine_ratings <- matrix(0, nrow = nrow(samples_base), ncol = length(tc_range))
  for (i in 1:length(tc_range)) {
    engine_ratings[, i] <- samples_base[, engine_idx] + samples_slope[, engine_idx] * tc_range_centered[i]
  }

  engine_data <- data.frame(
    engine = engine_names[engine_idx],
    time_control = tc_range,
    mean_rating = colMeans(engine_ratings),
    sd_rating = apply(engine_ratings, 2, sd),
    q025 = apply(engine_ratings, 2, quantile, 0.025),
    q975 = apply(engine_ratings, 2, quantile, 0.975)
  )
  plot_data <- bind_rows(plot_data, engine_data)
}


p1 <- ggplot(plot_data, aes(x = time_control, y = mean_rating)) +
  geom_line(size = 1.2, aes(color = engine)) +
  geom_ribbon(aes(ymin = q025, ymax = q975, fill = engine), alpha = 0.3) +
  facet_wrap(~ engine, ncol = 2) +
  labs(
    title = "Engine Ratings vs Time Control",
    subtitle = "With 95% Credible Intervals - Individual Panels",
    x = "Expected Game Duration (seconds)",
    y = "Elo Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("outputs/part2/time_control_ratings.png", p1, width = 12, height = 8, dpi = 300)
cat("✓ Saved: outputs/part2/time_control_ratings.png\n")


cat("Creating tier lists for four time controls...\n")

tier_lists <- list()

for (tc_name in names(required_tc_values)) {
  tc_val <- required_tc_values[tc_name]
  tc_val_std <- (tc_val - tc_mean) / tc_sd
  
  # calculate ratings for this time control
  ratings_tc <- matrix(0, nrow = nrow(samples_base), ncol = n_engines)
  
  for (i in 1:n_engines) {
    ratings_tc[, i] <- samples_base[, i] + samples_slope[, i] * tc_val_std
  }

  # create tier list summary
  tier_summary <- data.frame(
    engine = engine_names,
    mean_rating = colMeans(ratings_tc),
    sd_rating = apply(ratings_tc, 2, sd),
    q025 = apply(ratings_tc, 2, quantile, 0.025),
    q975 = apply(ratings_tc, 2, quantile, 0.975)
  ) %>%
    arrange(desc(mean_rating)) %>%
    mutate(rank = row_number())
  
  tier_lists[[tc_name]] <- tier_summary
  
  cat("\n", strrep("=", 50), "\n")
  cat("TIER LIST:", tc_name, "(", tc_val, "seconds)\n")
  cat(strrep("=", 50), "\n")
  print(tier_summary %>% select(rank, engine, mean_rating, sd_rating))
  
  # create tier list visualization
  p_tier <- ggplot(tier_summary, aes(x = reorder(engine, -mean_rating), y = mean_rating)) +
    #geom_col(alpha = 0.8, width = 0.7) +
    geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.2, alpha = 0.7) +
    geom_text(aes(label = round(mean_rating)), vjust = -0.5, size = 3.5, fontface = "bold") +
    labs(
      title = paste("Tier List:", tc_name),
      subtitle = paste("Expected duration:", tc_val, "seconds"),
      x = "Engine",
      y = "Elo Rating"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "none"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(
      ylim = c(1500, max(tier_summary$mean_rating + tier_summary$sd_rating) * 1.05)
    )
  
  filename <- paste0("outputs/part2/tier_list_", gsub("+", "_", tc_name, fixed = TRUE), ".png")
  ggsave(filename, p_tier, width = 8, height = 6, dpi = 300)
  cat("✓ Saved:", filename, "\n")
}

cat("Generating discrete time control comparison plot...\n")

# combine the four tier list summaries into a single data frame
comparison_data <- bind_rows(tier_lists, .id = "time_name")

comparison_data <- comparison_data %>%
  mutate(
    tc_val = required_tc_values[time_name], 
    time_label = factor(time_name, levels = c("15+0", "60+0", "180+2", "600+0"))
  )

p_discrete_change <- ggplot(comparison_data, aes(x = time_label, y = mean_rating, group = engine)) +
  
  # lines to show the trend of Elo change for each engine
  geom_line(aes(color = engine), size = 1.2, alpha = 0.8) +
  
  # points for each specific time control
  geom_point(aes(color = engine, shape = time_label), size = 3) +
  
  labs(
    title = "Engine Elo Change Across Discrete Time Controls",
    subtitle = "Elo ratings predicted at four specific time controls (Mean)",
    x = "Time Control (Time + Increment)",
    y = "Elo Rating",
    color = "Engine",
    shape = "Time Control"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, size = 11, face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("outputs/part2/discrete_tc_comparison.png", p_discrete_change, width = 10, height = 7, dpi = 300)
cat("✓ Saved: outputs/part2/discrete_tc_comparison.png\n")

# ==============================================================================
# SAVE RESULTS
# ==============================================================================
save(comparison, tier_lists, stan_data, 
     file = "outputs/part2/part2_results.RData") 
saveRDS(fit_extended, "outputs/part2/part2_fit.rds")

cat("\n========================================\n")
cat("PART 2 COMPLETE!\n")
cat("========================================\n\n")