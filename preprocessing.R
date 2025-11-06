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
    result <- games_of_interest[i, 3]
    
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


results <- get_all_pair_results("games.csv")
print(results)
