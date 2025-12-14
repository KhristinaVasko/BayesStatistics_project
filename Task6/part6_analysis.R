library(Rschach)

# Output filenames
pgn_filename <- "tournament_games.pgn"
csv_filename <- "tournament_results.csv"

# Time control
TC_BASE <- 30   # base time in milliseconds
TC_INC  <- 0 # increment in milliseconds
  #according to the documentation, tc_base and tc_inc are specified in seconds,
  #but after initial run I suspect it must be microseconds --> *1000

# we always play N_POSITIONS different positions, each position 2x (flip colors)
N_POSITIONS <- 25 

#load opening book
opening_book <- read.csv("8moves_v3.epd", header = FALSE)[[1]]

# -----------------------------------
# Step 1: Initialization
# -----------------------------------

# We initialize five engines with LMR_slope parameters {0, 0.5, 1.0, 1.5, 2.0}
# and run a big tournament, where each pair plays N_POSITIONS*2 games

# Values for LMR_slope
param_values <- c(0, 0.5, 1.0, 1.5, 2.0)

# Create engine instances
engines <- vector("list", length(param_values))

for (i in seq_along(param_values)) {
  #note: I originally tried to initalize the engines using a separate function,
  #but weird stuff happened to the engine-objects that were returned...
  #somehow, returning the Engine() objects made some pointers do weird stuff.
  #since I didn't find out what the problem was, I decided to do it "manually"
  current_LMRslope <- param_values[i]
  engine_name   <- paste0("E_", current_LMRslope)
  
  # Define the parameters for this specific engine
  current_params <- list(NMP_intercept = 3,
                         NMP_slope = 0,
                         LMR_intercept = 1,
                         RFP_intercept = -30,
                         RFP_slope = 150,
                         LMR_slope = current_LMRslope)
  e <- Engine(engine_name)
  e$set.params(current_params)
  
  engines[[i]] <- e
}

# Assign names to the list for easy access (e.g., engines$E_1.5)
names(engines) <- paste0("E_", param_values)

#generate N_POSITIONS random starting positions from the opening book
tournament_openings <- opening_book[sample(length(opening_book), N_POSITIONS)]

games <- play.tournament(engines,
                         book = tournament_openings,
                         nr_rounds = 25,
                         repeated = TRUE,
                         tc_base = TC_BASE*1000,
                         tc_inc = TC_INC*1000,
                         verbose = 1)

################
# Create pgn and save results to .csv file
################
pgn(games, file = pgn_filename)

lines <- readLines(pgn_filename)
# Extract fields
whites  <- gsub('\\[White "|"\\]', "", grep('\\[White ".*"\\]', lines, value = TRUE))
blacks  <- gsub('\\[Black "|"\\]', "", grep('\\[Black ".*"\\]', lines, value = TRUE))
results <- gsub('\\[Result "|"\\]', "", grep('\\[Result ".*"\\]', lines, value = TRUE))
tc_val <- paste0(TC_BASE, "+", TC_INC)
time_controls <- rep(tc_val, length(results))
# Create dataframe with columns: white, black, result, timecontrol
results_df <- data.frame(
  white = whites, 
  black = blacks, 
  result = results, 
  timecontrol = time_controls, 
  stringsAsFactors = FALSE
)
write.csv(results_df, csv_filename, row.names = FALSE, quote = FALSE)

#tournament results (table generated with chessbase)
#     1	       2	           3	           4	           5	
# 1	E_0.5	27.0 - 23.0   31.0 - 19.0   38.5 - 11.5   39.0 - 11.0 **					135.5/200
# 2	E_0	  23.0 - 27.0   30.5 - 19.5   35.0 - 15.0   40.0 - 10.0	  **				128.5/200
# 3	E_1	  19.0 - 31.0   19.5 - 30.5   26.0 - 24.0   37.0 - 13.0		  **			101.5/200
# 4	E_1.5	11.5 - 38.5   15.0 - 35.0   24.0 - 26.0   28.0 - 22.0			  **		78.5/200
# 5	E_2	  11.0 - 39.0   10.0 - 40.0   13.0 - 37.0   22.0 - 28.0				  **	56.0/200


# -----------------------------------
# Step 2: Initial Modelling (Bayesian Elo + Gaussian Process)
# -----------------------------------

#To-do:

# Step 2: Initial Modelling
  #run the Rating Model (Task 1) on the game results, with smaller variance
  #extract mean rating \mu and standard deviation \sigma for each engine.
  
  #Fit the Gaussian Process:
    #X: LMR_slope values.
    #Y: Mean ratings.
    #Noise: normally distributed around 0, with standard deviation \sigma

  #plot the GP+data points

# Step3: Optimization - repeat this step for either a fixed amount of repeats,
    #or until the probability of improvement drops below a certain value
    #after every iteration, print out the expected improvement and ask the user if he wants to continue

  #Calculate the Expected Improvement (see lecture 8) 
  #find x that maximizes the EI-function and initialize new engine with x LMRslope
  #pick a diverse subset of the previous engines (e.g. the two best engines till now,
    #the median engine and the worst engine) and let the new engine play games, eg.
    # 15 rounds each against the best two engines, and 5 rounds against the median & worst
    # (this would take approx. 80mins)
  #add the games to the .pgn, and the scores to the .csv

  # run the Rating Model (Task 1) on all game results
  
  # calculate and plot the new GP

  # calculate the probability of improvement
  