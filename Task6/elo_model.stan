// Bayesian Elo Rating Model, adapted for Task 6
data {
  int<lower=1> N;                    // Number of games
  int<lower=1> K;                    // Number of engines
  array[N] int<lower=1,upper=K> white;  // White player indices
  array[N] int<lower=1,upper=K> black;  // Black player indices
  array[N] real<lower=0,upper=1> score; // Score from white's perspective, 1=win, 0.5=draw, 0=loss
}

parameters {
  vector[K] rating;                  // Engine ratings (centered around 0)
  real<lower=0, upper=1> draw_rate; //instead of fixed draw rate 0.3, model it as random variable
}

model {
  // Ratings centered around 0 with SD 200
  rating ~ normal(0, 200);

  // model draw rate as beta distributed, with mean 0.3
  draw_rate ~ beta(3, 7);

  // --- Likelihood ---
  for (n in 1:N) {
    // Calculate White's Expected Score: 1 / (1 + 10^((Rb - Rw)/400))
    real delta_rating = rating[white[n]] - rating[black[n]];
    real expected_score = inv_logit(delta_rating * log(10) / 400);

    // Derive probabilities for Win/Loss/Draw
    // Logic: E = P(win) + 0.5 * P(draw)
    // Therefore: P(win) approx E - 0.5 * draw_rate
    real p_win_raw = expected_score - 0.5 * draw_rate;
    real p_loss_raw = 1.0 - expected_score - 0.5 * draw_rate;

    // Safety: ensure non-negative (fmax from reference model)
    real p_win = fmax(0.001, p_win_raw);
    real p_loss = fmax(0.001, p_loss_raw);
    
    // Normalize probabilities so they sum to 1
    real total_prob = p_win + draw_rate + p_loss;
    real p_win_norm = p_win / total_prob;
    real p_draw_norm = draw_rate / total_prob;
    real p_loss_norm = p_loss / total_prob;

    // Update log-probability based on result
    if (score[n] == 1.0) {
       target += log(p_win_norm);
    } else if (score[n] == 0.5) {
       target += log(p_draw_norm);
    } else {
       target += log(p_loss_norm);
    }
  }
}

generated quantities {
  // Generate absolute ratings (centered at 2000)
  vector[K] rating_absolute = rating + 2000;

}
