data {
  int<lower=1> N;                               // Number of games
  int<lower=1> K;                               // Number of engines
  array[N] int<lower=1, upper=K> white;         // White player indices
  array[N] int<lower=1, upper=K> black;         // Black player indices
  array[N] real<lower=0, upper=1> score;        // Score from white's perspective
  vector[N] time_centered;                      // Standardized time control (t*)
  real mean_time;                               // Mean of original time (for prediction)
  real tc_sd;                                   // SD of original time (for prediction)
}

parameters {
  vector[K] rating_raw;                         // Raw engine ratings (mean 0, SD 1)
  vector[K] time_slope_raw;                     // Raw time slope (mean 0, SD 1)
  real<lower=0> sigma;                          // Observation noise/error
}

transformed parameters {
  // Base rating is the rating at mean time control (centered at 2000, SD 200)
  vector[K] base_rating = 2000 + rating_raw * 200;
  // Time slope magnitude (change in Elo per 1 SD of time)
  vector[K] time_slope = time_slope_raw * 20;
  
  vector[N] expected_score;
  
  for (n in 1:N) {
  // Time-dependent ratings: R(t*) = base_rating + time_slope * t*
    real rating_white = base_rating[white[n]] + time_slope[white[n]] * time_centered[n];
    real rating_black = base_rating[black[n]] + time_slope[black[n]] * time_centered[n];
    
    real rating_diff = rating_black - rating_white;
    expected_score[n] = 1.0 / (1.0 + pow(10.0, rating_diff / 400.0));
  }
}

model {
  // Priors
  rating_raw ~ normal(0, 1);
  time_slope_raw ~ normal(0, 1);
  sigma ~ normal(0, 0.1);

  // Likelihood
  score ~ normal(expected_score, sigma);
}

generated quantities {
  vector[K] rating_absolute = base_rating;
  
  // Log-likelihood for model comparison
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(score[n] | expected_score[n], sigma);
  }
}
