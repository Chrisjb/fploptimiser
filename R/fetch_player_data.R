#' Fetch Player Data From The FPL API
#'
#' Fetches the historical data for each FPL player, including the current prices of those players.
#'
#' @import jsonlite
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#'
#' @param reduce If TRUE, returns only select columns from the API for nicer display. If FALSE, will return all columns from the FPL API.
#'
#' @return a data.frame of the full player data and history.
#'
#' @examples
#' df <- fetch_player_data()
#'

fetch_player_data <- function(reduce=TRUE) {
    fpl_api <-  jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
    player_details <- fpl_api$elements
    position_names <- fpl_api$element_types
    teams <- fpl_api$teams %>%
      select(-code, -form) %>%
      arrange(name) %>%
      mutate(team_id = 1:20)

    df <- player_details %>% left_join(position_names, by = c('element_type' = 'id'))  %>%
      left_join(teams, by = c('team' = 'id')) %>%
      mutate(points_per_game = as.numeric(points_per_game),
             vapm = (points_per_game - 2) / now_cost,
             vapm = if_else(vapm < 0, 0, vapm),
             total_points = as.numeric(total_points),
             games = round(total_points / points_per_game,0),
             games = if_else(is.na(games), 0, games)) %>%
      arrange(name)

    if(reduce == TRUE){
      df <- df %>%
        select(web_name,singular_name, team_name=name, now_cost, total_points, games, minutes, goals_scored, assists, clean_sheets, goals_conceded) %>%
        tibble()
    }
    return(df)

  }
