#' Fetch Expected Clean Sheet data
#'
#' Fetches the expected clean sheet data for each prem team. Uses shot data scraped from Understat.
#'
#' @import jsonlite
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#'
#' @param year Season we want to download the xCS data for. For 2019/20 enter 2019.
#'
#' @return a data.frame of the expected vs actual clean sheets, split by home/away.
#'
#' @examples
#' df <- fetch_player_data()
#'

fetch_xCS <- function(year = 2019, ...){
  # get IDs of each game played
  understat_matches <- xml2::read_html(paste0('https://understat.com/league/EPL/',year))

  match_ids <- understat_matches %>%
    rvest::html_nodes('script') %>%
    as.character() %>%
    stringr::str_subset('datesData')%>%
    stringi::stri_unescape_unicode() %>%
    stringr::str_extract('\\[.+\\]') %>%
    jsonlite::fromJSON(flatten = T) %>%
    filter(isResult == 'TRUE') %>%
    pull(id)


  # for each played game, scrape understat xCS
  understat_xCS <- lapply(match_ids, function(x) calc_xcs(x, ...))

  do.call(rbind, understat_xCS) %>%
    group_by(defending_team, home_away) %>%
    summarise(xCS = sum(xCS), matches = n()) %>%
    pivot_wider(names_from = home_away, values_from = c(xCS, matches)) %>%
    mutate(xCS_per_game_a = xCS_a / matches_a, xCS_per_game_h = xCS_h / matches_h) %>%
    mutate(total_xCS = xCS_a + xCS_h)


}