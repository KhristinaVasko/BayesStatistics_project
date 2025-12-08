count_results <- function(file_path, P1, P2) {
  #returns a list containing the number of P1-wins, draws, and P2-wins
  
  # Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Filter for games where Player 1 and Player 2 are involved
  games_of_interest <- subset(data, 
                              (white == P1 & black == P2) | 
                                (white == P2 & black == P1))

  #if there are no results, return 0,0,0
  if(nrow(games_of_interest)==0){
    return(list(P1_wins = 0, draws = 0, P2_wins = 0))
  }
  
  # Initialize counters
  P1_wins <- 0
  P2_wins <- 0
  draws <- 0
  
  # Loop through the games and count the results
  for (i in 1:nrow(games_of_interest)) {
    result <- games_of_interest$result[i]
    
    if (result == "1-0" && games_of_interest[i, 1] == P1) {
      # P1 won
      P1_wins <- P1_wins + 1
    } else if (result == "1-0" && games_of_interest[i, 1] == P2) {
      # P2 won
      P2_wins <- P2_wins + 1
    } else if (result == "0-1" && games_of_interest[i, 1] == P1) {
      # P2 won
      P2_wins <- P2_wins + 1
    } else if (result == "0-1" && games_of_interest[i, 1] == P2) {
      # P1 won
      P1_wins <- P1_wins + 1
    } else if (result == "1/2-1/2") {
      # Draw
      draws <- draws + 1
    }
  }
  
  # Return the results as a list
  return(list(P1_wins = P1_wins, draws = draws, P2_wins = P2_wins))
}

get_players <- function(file_path) {
  # Returns all unique players from the chess games
  
  # Read the CSV file
  chess_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Combine the 'white' and 'black' columns, then extract unique player names
  players <- unique(c(chess_data$white, chess_data$black))
  return(players)
}

get_all_pair_results <- function(file_path) {
  # get results for all possible player pairs
  
  # Get the list of all players
  players <- get_players(file_path)
  
  # Initialize an empty list to store the results for each pair
  pair_results <- list()
  
  # Loop over all pairs of players
  for (i in 1:(length(players) - 1)) {
    for (j in (i + 1):length(players)) {
      P1 <- players[i]
      P2 <- players[j]
      
      # Get the results for the current pair
      result <- count_results(file_path, P1, P2)
      
      # Store the result in the list, using a combination of player names as the key
      pair_results[[paste(P1, P2, sep = "_vs_")]] <- result
    }
  }
  
  # Return the list of results for all pairs
  return(pair_results)
}

get_engine_pairs <- function(data) {
  # Get all unique engine pairs (both directions: A vs B and B vs A)
  engines <- sort(unique(c(data$white, data$black)))
  pairs <- list()
  
  # Create ordered pairs: for each pair (A, B), test both A vs B and B vs A
  for (i in 1:(length(engines) - 1)) {
    for (j in (i + 1):length(engines)) {
      pairs[[length(pairs) + 1]] <- c(engines[i], engines[j])  # A vs B
      pairs[[length(pairs) + 1]] <- c(engines[j], engines[i])  # B vs A
    }
  }
  
  return(pairs)
}

get_ordered_games <- function(data, engine1, engine2) {
  # Get games between two engines in order they appear in CSV
  # engine1 is treated as "new", engine2 as "base"
  
  # Add row number to preserve original order
  data$row_num <- 1:nrow(data)
  
  # Filter games where the two engines play each other
  filter_condition <- (data$white == engine1 & data$black == engine2) | 
    (data$white == engine2 & data$black == engine1)
  games <- data[filter_condition, ]
  
  #return Null, if no games found
  if (nrow(games) == 0) {
    return(NULL)
  }
  
  # Preserve order by sorting by row_num
  games <- games[order(games$row_num), ]
  
  # Add new columns
  games$new_engine <- engine1
  games$base_engine <- engine2
  
  # Determine result from "new" engine's perspective (engine1)
  games$new_wins <- ifelse(
    games$white == engine1 & games$result == "1-0", 1,      # new wins as white
    ifelse(
      games$black == engine1 & games$result == "0-1", 1,      # new wins as black
      ifelse(
        games$result == "1/2-1/2", 0.5,                      # draw
        ifelse(
          games$white == engine1 & games$result == "0-1", 0,  # new loses as white
          ifelse(
            games$black == engine1 & games$result == "1-0", 0, # new loses as black
            NA  # default fallback (shouldn't happen)
          )
        )
      )
    )
  )
  
  return(games)
}