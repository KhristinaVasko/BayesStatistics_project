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

  //add additional constriction to make sure the mean rating is centered around 0
  sum(rating) ~ normal(0, 1);	
	
  // model draw rate as beta distributed, with mean 0.3
  draw_rate ~ beta(3, 7);
  
  vector[N] delta = rating[white] - rating[black];
  vector[N] E = inv_logit(delta * log(10) / 400);

  // Likelihood
  for (n in 1:N) {
    real p_win_raw = E[n] - 0.5 * draw_rate;
    real p_loss_raw = 1.0 - E[n] - 0.5 * draw_rate;

    real p_win = fmax(0.001, p_win_raw);
    real p_loss = fmax(0.001, p_loss_raw);
    real total = p_win + draw_rate + p_loss;

    if (score[n] == 1.0)      target += log(p_win / total);
    else if (score[n] == 0.5) target += log(draw_rate / total);
    else                      target += log(p_loss / total);
  }
}

generated quantities {
  // Generate absolute ratings (centered at 2000)
  vector[K] rating_absolute = rating + 2000;

}
