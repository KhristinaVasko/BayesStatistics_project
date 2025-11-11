// Bayesian Elo Rating Model
data {
  int<lower=1> N;                    // Number of games
  int<lower=1> K;                    // Number of engines
  array[N] int<lower=1,upper=K> white;  // White player indices
  array[N] int<lower=1,upper=K> black;  // Black player indices
  array[N] real<lower=0,upper=1> score; // Score from white's perspective
}

parameters {
  vector[K] rating;                  // Engine ratings (centered around 0)
}

transformed parameters {
  vector[N] expected_score;
  
  // Calculate expected scores using Elo formula
  // E_white = 1 / (1 + 10^((R_black - R_white) / 400))
  for (n in 1:N) {
    real rating_diff = rating[black[n]] - rating[white[n]];
    expected_score[n] = inv_logit(rating_diff * log(10) / 400);
  }
}

model {
  // Prior: Ratings centered around 0 with SD 200
  rating ~ normal(0, 200);
  
  // Likelihood: Model actual scores
  for (n in 1:N) {
    score[n] ~ normal(expected_score[n], 0.3);
  }
}

generated quantities {
  // Generate absolute ratings (centered at 2000)
  vector[K] rating_absolute = rating + 2000;
  
  // Log-likelihood for model comparison
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(score[n] | expected_score[n], 0.3);
  }
}