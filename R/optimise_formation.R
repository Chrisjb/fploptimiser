#' Optimise Formation
#'
#' Solves the optimisation problem for a FPL team given a budget constraint, amount of players, and objective function.
#'  also choses the best formation automatically
#'
#' @param objective takes value 'points', 'ppg' or 'vapm', indicating whether we want to maximise the total points obtained over the course of the dataset (season), the points per game played, or value added per million.
#' @param bank takes a numeric value indicating the budget available for our full team (team + bench). Enter the value in four digits so 100.0m would be 1000 - the default for the season start.
#' @param bench_value before running the optimiser you'll want to have chosen your bench already (there is little point in optimising players on your bench as they will not earn you points a lot of the time). Enter the value of your chosen bench in four digits as per the bank parameter.
#' @param gameweek_range numeric vector of the gameweeks to include in the data. This is not available pre-season and defaults to FALSE including all historic data for the past season.
#' @param min_games the minimum number of games a player must have played to be considered for selection. This should be set when the objective function is 'ppg' so that players with high points per game but with relatively few games played are avoided.
#' @param custom_df defaults to FALSE. If you want to make adjustments to the player data before optimising (eg. you want to exclude certain players from consideration) you can use the fetch_player_data() function and update the resulting dataset yourself. This dataset can be passed as the custom_df parameter.
#' @param expected_points_adjust defaults to FALSE. Should we adjust points and points per game for the xA / xG? Data comes from understat.
#' @return a data.frame of optimised solution.
#'
#' @import dplyr
#' @import lpSolve
#'
#' @examples
#' df <- fetch_player_data()
#'
#' # RUN OPTIMISATION  ON POINTS PER GAME FOR A 4-3-3 formation, with a bench value of 175 and minimum games of 13
#' result <- optimise_formation(objective = 'ppg', custom_df = FALSE, bench_value = 175, gk = 1, def = 4, mid = 3, fwd = 3, min_games = 13)
#' sum(result$points_per_game)
#'
#'
#' # RUN OPTIMISATION  ON TOTAL POINTS FOR A 3-4-3 formation, with a bench value of 170
#' result <- optimise_formation(objective = 'points', custom_df = FALSE, bench_value = 170)
#' sum(result$total_points)
#'
#' # USING CUSTOM DATA TO FILTER OUT UNWANTED RESULTS
#' library(dplyr)
#' df <- fetch_player_data() %>%
#'   # must have played more than 13 games
#'   filter(games >= 13) %>%
#'   # I don't want to consider Milivojevic or Alonso
#'   filter(! id %in% c(134, 103))

#' # RUN OPTIMISATION WITH CUSTOM DATA (using custom_df = df option)
#' result <- optimise_formation(objective = 'ppg', custom_df = df, bench_value = 175, min_games = 14)
#' sum(result$points_per_game)
#'
#'
#'
#'
#'
#' @export

optimise_formation <- function(objective = 'ppg', custom_df = F, bank = 1000, bench_value = 175, min_games = 2, expected_points_adjust = F, ...) {
  result1 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank,  bench_value = bench_value, gk = 1, def = 5, mid = 4, fwd = 1, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result2 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank,  bench_value = bench_value, gk = 1, def = 4, mid = 5, fwd = 1, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result3 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 5, mid = 3, fwd = 2, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result4 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 4, mid = 4, fwd = 2, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result5 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 3, mid = 5, fwd = 2, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result6 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 4, mid = 3, fwd = 3, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result7 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 3, mid = 4, fwd = 3, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)

  if(objective == 'ppg'){
    results <-  data.frame(
      result1 = result1$points_per_game %>% sum(),
      result2 = result2$points_per_game %>% sum(),
      result3 = result3$points_per_game %>% sum(),
      result4 = result4$points_per_game %>% sum(),
      result5 = result5$points_per_game %>% sum(),
      result6 = result6$points_per_game %>% sum(),
      result7 = result7$points_per_game %>% sum()
    )
  } else {
    results <-  data.frame(
      result1 = result1$total_points %>% sum(),
      result2 = result2$total_points %>% sum(),
      result3 = result3$total_points %>% sum(),
      result4 = result4$total_points %>% sum(),
      result5 = result5$total_points %>% sum(),
      result6 = result6$total_points %>% sum(),
      result7 = result7$total_points %>% sum()
    )
  }


  return(get(names(which.max(results))))
}
