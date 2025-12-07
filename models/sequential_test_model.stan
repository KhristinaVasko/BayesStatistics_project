// Sequential Test Model for Rating Difference (=Delta)
// Delta = R_new - R_base
// This model samples from the posterior distribution of Delta and draw_rate (we model the draw rate as Beta(3,7))

data {
  real prior_mean;              // Prior mean for Delta
  real<lower=0> prior_sd;       // Prior standard deviation for Delta
  int<lower=0> N;               // Number of games
  array[N] real score;          // Game results from new engine's perspective (1=win, 0.5=draw, 0=loss)
}

parameters {
  real delta;                   // Rating difference: R_new - R_base
  real<lower=0,upper=1> draw_rate;  // Draw rate (estimated as parameter)
}

transformed parameters {
  // Expected score for new engine given Delta is 1 / (1 + 10^(-Delta/400))
  // inv_logit(x) = 1/(1+exp(-x))
  real expected_score = inv_logit(delta * log(10) / 400);
  
  // Model probabilities: E = P(win) + 0.5*P(draw)
  real p_win_raw = expected_score - 0.5 * draw_rate;
  real p_draw = draw_rate;
  real p_loss_raw = 1 - expected_score - 0.5 * draw_rate;
  
  // Ensure probabilities are non-negative and normalize
  real p_win = fmax(0.001, p_win_raw);  // Ensure minimum probability
  real p_loss = fmax(0.001, p_loss_raw);
  real total_prob = p_win + p_draw + p_loss;
  
  // Normalized probabilities (these will be used in model block)
  real p_win_norm = p_win / total_prob;
  real p_draw_norm = p_draw / total_prob;
  real p_loss_norm = p_loss / total_prob;
}

model {
  // Prior
  delta ~ normal(prior_mean, prior_sd);
  
  // Prior for draw rate: Beta distribution centered around 0.3
  draw_rate ~ beta(3, 7);
  
  // Likelihood: Model each game result
  // use log probabilities
  for (n in 1:N) {
    if (score[n] == 1.0) {
      // New engine wins
      target += log(p_win_norm);
    } else if (score[n] == 0.5) {
      // Draw
      target += log(p_draw_norm);
    } else {
      // New engine loses (score[n] == 0.0)
      target += log(p_loss_norm);
    }
  }
}

