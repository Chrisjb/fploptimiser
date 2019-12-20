#' utility function to fetch data for players of a given team from understat

fetch_understat_data <- function(team = 'Arsenal', year = '2019') {
  understat <- xml2::read_html(glue::glue('https://understat.com/team/{team}/{year}'))


  understat_dat <- understat %>% rvest::html_nodes('script') %>%
    as.character() %>%
    stringr::str_subset('playersData') %>%
    stringi::stri_unescape_unicode()  %>%
    stringr::str_extract('\\[.+\\]') %>%
    jsonlite::fromJSON(flatten = T) %>%
    select(player_name, understat_games = games, understat_minutes = time,
           understat_goals = goals, xG, xA, understat_assists = assists, understat_shots = shots,
           understat_key_passes = key_passes, understat_yellow = yellow_cards, understat_red = red_cards,
           npg, npxG)




  return(understat_dat)
}
