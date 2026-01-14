# Bayesian Chess Engine Optimization (Task 8)

This folder contains the code and results for the final task of the *Bayesian Statistics* course project.

## Files
* `task8_mcmc.R`: The R script that runs the tournament simulation and implements the Metropolis-Hastings MCMC sampler.
* `Rplot.png`: Traceplots showing the convergence of the MCMC chains for the estimated Elo ratings.
* `Task8_Final_Elo_Ratings.txt`: The final posterior mean Elo ratings and summary statistics.

## Key Results
The MCMC analysis (based on 50 rounds) produced the following Elo estimates:
1.  **Base Engine:** ~2069 Elo (Highest Performance)
2.  **Opt1D:** ~1993 Elo
3.  **Opt2D:** ~1975 Elo
4.  **Tuned:** ~1965 Elo
