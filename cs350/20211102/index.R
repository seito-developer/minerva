simulate_raquetball = function(number_of_games, prob_win_serve,
                               prob_win_noserve, points_to_win) 
{    
  # variable to track the number of wins
  wins = 0
  
  # variable to track the number of simulated games
  total_games = 1
  
  # list where we will store the number of volleys per game
  volleylist = c()
  
  # running the game simulation loop
  while (total_games <= number_of_games) 
  {
    # variable to count number of volleys in one game
    volleys = 0
    
    # variable to count my points in one game
    my_points = 0
    
    # variable to count opponent's points in one game
    opp_points = 0
    
    # variable to keep track of who is serving 
    # (TRUE -> me, FALSE -> opponent)
    serving = TRUE
    
    # simulating a single game
    while (my_points < points_to_win && opp_points < points_to_win) 
    {
      # simulating the outcome of a single serve
      x = runif(1)
      
      # updating the game results if it was my serve
      if (serving == TRUE) 
      {
        # if I won  point, update  points in game
        if (x < prob_win_serve) 
          my_points = my_points + 1
        
        # otherwise, pass next serve to the opponent
        else 
          serving = FALSE
      } 
      
      # updating game results if it was opponent's serve
      else 
      {
        # if I won point, next serve is set to be mine
        if (x < prob_win_noserve)
          serving = TRUE
        
        # otherwise, update the opponent's earned points
        else 
          opp_points = opp_points + 1
      }
      
      # in either case, update the number of served volleys
      volleys = volleys + 1
    }
    
    # save  number of serves that were made in the last game
    volleylist[total_games] = volleys
    
    # update the number of games played
    total_games = total_games + 1
    
    # if I won the last game, update the number of wins
    if (my_points == points_to_win) 
      wins = wins + 1
  }
  
  # calculate the win probability
  win_probability = wins/total_games
  
  # calculate the expected number of volleys
  expected_num_volleys = my_points/(my_points + opp_points)
  
  print(paste('After', number_of_games, 'simulated games win probability is', win_probability))
  print(paste('Average number of volleys per match was', expected_num_volleys))
  print(paste('my_points: ', my_points))
  print(paste('opp_points: ', opp_points))
}

simulate_raquetball(number_of_games = 1000,
                    prob_win_serve = 0.6,
                    prob_win_noserve = 0.5,
                    points_to_win = 21)