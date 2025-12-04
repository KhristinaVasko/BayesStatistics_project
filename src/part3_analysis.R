# PART 3

if (basename(getwd()) == "src") {
  setwd("..")
}

library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

cat("PART 3\n")

# 1. LOAD MODEL, DATA, ENGINE NAMES

fit_extended <- readRDS("outputs/part2/part2_fit.rds")
load("outputs/part2/part2_results.RData")  # loads stan_data etc.

tc_mean <- stan_data$mean_time
tc_sd   <- stan_data$tc_sd

source("src/preprocessing.R")
engine_names <- sort(get_players("data/games.csv"))
K <- length(engine_names)
cat("Loaded", K, "engines.\n")

# 2. COMPUTE RATINGS AT 300+3 

tc_target   <- 300 + 40 * 3 
tstar       <- (tc_target - tc_mean) / tc_sd

samples_base  <- rstan::extract(fit_extended, pars = "base_rating")$base_rating
samples_slope <- rstan::extract(fit_extended, pars = "time_slope")$time_slope
S <- nrow(samples_base)

ratings_tc <- samples_base + samples_slope * tstar  # S x K
colnames(ratings_tc) <- engine_names

# 3. TOURNAMENT STATE (A–E, SM) 

tournament_labels <- c("A", "B", "C", "D", "E", "SM")
tournament_to_engine <- c(A = "A", B = "B", C = "C",
                          D = "D", E = "E", SM = "SM")

current_scores_mat <- matrix(c(
  NA, 3,   1.5, 4,   2,   2,    # A
  1,  NA,  2,   3.5, 0,   0.5,  # B
  2.5,2,   NA,  4,   0.5, 1,    # C
  0,  0.5, 0,   NA,  0,   0,    # D
  2,  4,   3.5, 4,   NA,  2,    # E
  2,  3.5, 3,   4,   2,   NA    # SM
), nrow = 6, byrow = TRUE,
dimnames = list(tournament_labels, tournament_labels))

current_total_points <- rowSums(current_scores_mat, na.rm = TRUE)

# 4. ESTIMATE DRAW RATE 
games <- read_csv("data/games.csv", show_col_types = FALSE)
draw_rate <- mean(games$result == "1/2-1/2", na.rm = TRUE)
cat("Draw rate:", round(draw_rate, 3))

# 5. ELO EXPECTED SCORE + SIMULATION FUNCTION 

elo_expected_score <- function(Ri, Rj) {
  1 / (1 + 10 ^ ((Rj - Ri) / 400))
}

simulate_one_tournament <- function(ratings_all,
                                    tournament_labels,
                                    tournament_to_engine,
                                    current_points,
                                    draw_rate,
                                    n_games_remaining_pair = 6) {
  ratings_tournament <- ratings_all[tournament_to_engine]
  names(ratings_tournament) <- tournament_labels
  
  points <- current_points
  
  for (i in 1:(length(tournament_labels) - 1)) {
    for (j in (i + 1):length(tournament_labels)) {
      ei <- tournament_labels[i]
      ej <- tournament_labels[j]
      
      Ri <- ratings_tournament[ei]
      Rj <- ratings_tournament[ej]
      
      E_ij <- elo_expected_score(Ri, Rj)
      
      p_draw <- draw_rate
      p_win  <- max(min(E_ij - 0.5 * p_draw, 1 - p_draw), 0)
      p_loss <- 1 - p_draw - p_win
      
      for (g in 1:n_games_remaining_pair) {
        u <- runif(1)
        if (u < p_win) {
          points[ei] <- points[ei] + 1
        } else if (u < p_win + p_draw) {
          points[ei] <- points[ei] + 0.5
          points[ej] <- points[ej] + 0.5
        } else {
          points[ej] <- points[ej] + 1
        }
      }
    }
  }
  
  ord  <- order(points, decreasing = TRUE)
  rank <- integer(length(points))
  rank[ord] <- 1:length(points)
  names(rank) <- names(points)
  
  list(points = points, rank = rank)
}

# 6. MONTE CARLO SIMULATIONS
set.seed(123)

n_sims <- 5000
sm_top2_count <- 0

rank_counts <- matrix(0,
                      nrow = length(tournament_labels),
                      ncol = length(tournament_labels),
                      dimnames = list(tournament_labels,
                                      paste0("pos", 1:length(tournament_labels))))

for (s in 1:n_sims) {
  idx <- sample(1:S, 1)
  ratings_vec <- ratings_tc[idx, ]
  
  res <- simulate_one_tournament(
    ratings_all            = ratings_vec,
    tournament_labels      = tournament_labels,
    tournament_to_engine   = tournament_to_engine,
    current_points         = current_total_points,
    draw_rate              = draw_rate,
    n_games_remaining_pair = 6
  )
  
  if (res$rank["SM"] <= 2) {
    sm_top2_count <- sm_top2_count + 1
  }
  
  for (lab in tournament_labels) {
    pos <- res$rank[lab]
    rank_counts[lab, paste0("pos", pos)] <-
      rank_counts[lab, paste0("pos", pos)] + 1
  }
}

rank_probs    <- rank_counts / n_sims
prob_sm_top2  <- sm_top2_count / n_sims

cat("P(SM in top 2) ≈", round(prob_sm_top2, 3), "\n")
cat("Rank probabilities:\n")
print(round(rank_probs, 3))

# 7. EXPECTED VALUE OF THE BET

EV <- 100 * (2 * prob_sm_top2 - 1)
cat("Expected value of bet:", round(EV, 2), "\n")

if (EV > 0) {
  cat("take the bet.\n")
} else {
  cat("do not take the bet.\n")
}

# 8. SAVE RESULTS

if (!dir.exists("outputs/part3")) {
  dir.create("outputs/part3", recursive = TRUE)
}

sm_summary <- list(
  prob_sm_top2 = prob_sm_top2,
  EV_euros     = EV,
  rank_probs   = rank_probs,
  n_sims       = n_sims
)

save(sm_summary, file = "outputs/part3/part3_results.RData")

write_csv(
  as.data.frame(rank_probs) %>%
    rownames_to_column("engine"),
  "outputs/part3/part3_rank_probabilities.csv"
)

cat("PART 3 COMPLETE\n")


