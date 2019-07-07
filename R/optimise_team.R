#' Fetch Player Data From The FPL API
#'
#' Solves the optimisation problem for a FPL team given a budget constraint, amount of players, and objective function.
#'
#' @param objective takes value 'points', 'ppg' or 'vapm', indicating whether we want to maximise the total points obtained over the course of the dataset (season), the points per game played, or value added per million.
#' @param bank takes a numeric value indicating the budget available for our full team (team + bench). Enter the value in four digits so 100.0m would be 1000 - the default for the season start.
#' @param bench_value before running the optimiser you'll want to have chosen your bench already (there is little point in optimising players on your bench as they will not earn you points a lot of the time). Enter the value of your chosen bench in four digits as per the bank parameter.
#' @param gk number of goalkeepers we want to pick - most likely one. If rotating keepers, it can be useful to pick your goalkeepers and add both to your bench value, then set gk = 0. This prevents the goalkeeper being optimised as if it were in goal as set and forget.
#' @param def number of defenders to pick in our optimal solution
#' @param mid number of midfielders to pick in our optimal solution
#' @param fwd number of forwards to pick in our optimal solution
#' @param gameweek_range numeric vector of the gameweeks to include in the data. This is not available pre-season and defaults to FALSE including all historic data for the past season.
#' @param min_games the minimum number of games a player must have played to be considered for selection. This should be set when the objective function is 'ppg' so that players with high points per game but with relatively few games played are avoided.
#' @param custom_df defaults to FALSE. If you want to make adjustments to the player data before optimising (eg. you want to exclude certain players from consideration) you can use the fetch_player_data() function and update the resulting dataset yourself. This dataset can be passed as the custom_df parameter.
#' @param expected_points_adjust defaults to FALSE. Should we adjust points and points per game for the xA / xG? Data from understat.
#' @return a data.frame of optimised solution.
#'
#' @import dplyr
#' @import lpSolve
#'
#' @examples
#' df <- fetch_player_data()
#'
#' # RUN OPTIMISATION  ON POINTS PER GAME FOR A 4-3-3 formation, with a bench value of 175 and minimum games of 13
#' result <- optimise_team(objective = 'ppg', custom_df = F, bench_value = 175, gk = 1, def = 4, mid = 3, fwd = 3, min_games = 13)
#' sum(result$points_per_game)
#'
#'
#' # RUN OPTIMISATION  ON TOTAL POINTS FOR A 3-4-3 formation, with a bench value of 170
#' result <- optimise_team(objective = 'points', custom_df = F, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3)
#' sum(result$total_points)
#'
#' # USING CUSTOM DATA TO FILTER OUT UNWANTED RESULTS
#' df <- fetch_player_data() %>%
#'   # must have played more than 13 games
#'   filter(games >= 13) %>%
#'   # I don't want to consider Milivojevic or Alonso
#'   filter(! id %in% c(134, 103))
#'
#' # adjust zaha's points as he's now midfield (1 extra pt for goal and point for cs)
#' df[df$id == 133,]$total_points <- df[df$id == 133,]$total_points + df[df$id == 133,]$goals_scored + df[df$id == 133,]$clean_sheets
#' df[df$id == 133,]$points_per_game <- df[df$id == 133,]$total_points / df[df$id == 133,]$games
#'
#' # same for perez
#' df[df$id == 265,]$total_points <- df[df$id == 265,]$total_points + df[df$id == 265,]$goals_scored + df[df$id == 265,]$clean_sheets
#' df[df$id == 265,]$points_per_game <- df[df$id == 265,]$total_points / df[df$id == 265,]$games
#'
#' # downgrade jota and delofeou's stats
#' df[df$id == 363,]$total_points <- df[df$id == 363,]$total_points - df[df$id == 363,]$goals_scored - df[df$id == 363,]$clean_sheets
#' df[df$id == 363,]$points_per_game <- df[df$id == 363,]$total_points / df[df$id == 363,]$games
#'
#' df[df$id == 410,]$total_points <- df[df$id == 410,]$total_points - df[df$id == 410,]$goals_scored - df[df$id == 410,]$clean_sheets
#' df[df$id == 410,]$points_per_game <- df[df$id == 410,]$total_points / df[df$id == 410,]$games
#'
#' # RUN OPTIMISATION WITH CUSTOM DATA (using custom_df = df option)
#' result <- optimise_team(objective = 'ppg', custom_df = df, bench_value = 175, gk = 1, def = 4, mid = 4, fwd = 2, min_games = 14)
#' sum(result$points_per_game)
#'
#'
#' # RUN WITH expected goals / assists
#' result <- optimise_team(objective = 'ppg', custom_df = df, bench_value = 175, gk = 1, def = 4, mid = 4, fwd = 2, min_games = 14, expected_points_adjust = T)
#' sum(result$points_per_game)
#'
#'
#'
#'
#' @export

optimise_team <- function(objective = 'points', bank = 1000, bench_value = 170, gk =1, def = 3, mid = 4, fwd = 3, gameweek_range = F, min_games = 1, custom_df = F, expected_points_adjust = F) {

  # checks
  if(!objective %in% c('points', 'ppg', 'vapm')){
    stop('objective must be one of: "points", "ppg" or "vapm"')
  } else if(gameweek_range != F) {
    warning('setting gameweek_range currently has no effect as gameweek history is unavailable for this season.')
  } else if(bank < 800) {
    warning('Are you sure that you have entered the bank parameter correctly? For 100.0m you should enter bank = 1000')
  } else if(!gk %in% c(0:2)){
    warning('Check that you have entered the gk parameter correctly. It should be 0, 1 or 2')
  } else if(!def %in% c(0:5)){
    warning('Check that you have entered the def parameter correctly. It should be between 0 and 5')
  } else if(!mid %in% c(0:5)){
    warning('Check that you have entered the mid parameter correctly.  It should be between 0 and 5')
  } else if(!fwd %in% c(0:3)){
    warning('Check that you have entered the fwd parameter correctly. It should between 0 and 3')
  }

  if(objective == 'ppg' & min_games %in% c(0,1)){
    warning('When using objective = "ppg" you may want to set the min_games parameter.')
  }

  if(is.logical(custom_df)) {
    df <- fetch_player_data(gameweek_range)


  } else{
    if(!is.data.frame(df)){
      stop('df should be a data.frame if specified. Else set to df = FALSE.')
    }
    df <- custom_df
  }

  if(expected_points_adjust == T) {
    message('adjusting for xA / xG...')
    df <-  fetch_xg_data() %>%
      filter(!is.na(xG))
    if(!is.logical(custom_df)){
      df<- df %>%
        filter(id %in% custom_df$id)
    }

  }

  df <- df %>%
    filter(games >= min_games)


  # create the constraints
  n_goalkeepers <- gk
  n_defenders <- def
  n_midfield <- mid
  n_forwards <- fwd

  max_cost <- bank - bench_value

  # create vectors to constrain by position
  df$Goalkeeper = ifelse(df$singular_name == "Goalkeeper", 1, 0)
  df$Defender = ifelse(df$singular_name == "Defender", 1, 0)
  df$Midfielder = ifelse(df$singular_name == "Midfielder", 1, 0)
  df$Forward = ifelse(df$singular_name == "Forward", 1, 0)


  # Create vector to constrain by max number of players allowed per team
  team_constraint = unlist(lapply(unique(df$team_code), function(x, df){
    ifelse(df$team_code==x, 1, 0)
  }, df=df))

  # next we need the constraint directions
  const_dir <- c("=", "=", "=", "=", rep("<=", length(unique(df$team_code)) + 1))

  if(objective == 'points') {
    obj_fun <- df$total_points
  } else if(objective == 'ppg'){
    obj_fun <- df$points_per_game
  } else if(objective == 'vapm') {
    obj_fun <- df$vapm
  }


  # Put the complete matrix together
  const_mat = matrix(c(df$Goalkeeper, df$Defender, df$Midfielder, df$Forward,
                       df$now_cost, team_constraint),
                     nrow=(5 + length(unique(df$team_code))),
                     byrow=TRUE)

  const_rhs = c(n_goalkeepers, n_defenders, n_midfield, n_forwards, max_cost, rep(3, length(unique(df$team_code))))

  # And solve the linear system
  x = lpSolve::lp("max", obj_fun, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)

  result <- arrange(df[which(x$solution==1),], desc(Goalkeeper), desc(Defender), desc(Midfielder), desc(Forward))

  return(result)


}
