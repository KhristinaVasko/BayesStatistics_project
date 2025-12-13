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

pgn(games, file = pgn_filename)