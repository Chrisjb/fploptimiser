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
#' @param gameweek_range numeric vector of the gameweeks to include in the data. This is not available pre-season and defaults to FALSE including all historic data for the past season.
#'
#' @return a data.frame of the full player data and history.
#'
#' @examples
#' df <- fetch_player_data()
#'

fetch_player_data <- local({
  memory <- list()
  function(gameweek_range = F) {
    valueName <- as.character(gameweek_range)
    if(!is.null(memory[[valueName]])){
      message('Retrieving player data already stored in memory...')
      return(memory[[valueName]])
    }
    fpl_api <-  jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static")
    player_details <- fpl_api$elements
    position_names <- fpl_api$element_types

    df <- player_details %>% left_join(position_names, by = c('element_type' = 'id'))  %>%
      mutate(points_per_game = as.numeric(points_per_game),
             total_points = as.numeric(total_points),
             games = round(total_points / points_per_game,0))

    memory[[valueName]] <<- df
    return(df)
  }
})
