#' Fetch Expected Clean Sheet data
#'
#' Fetches the expected clean sheet data for each prem team. Uses shot data scraped from Understat.
#'
#'
#' @param year Season we want to download the xCS data for. For 2020/21 enter 2021.
#' @param match_id Fetch xCS data for a single match, if you know the match ID. By default it is set to 'all'.
#' @param ungroup Set this to TRUE if you want raw data for each game.
#' @return a data.frame of the expected clean sheets, split by home/away.
#'
#' @import jsonlite
#' @import progress
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#'
#' @examples
#' df <- fetch_xCS()
#'
#' @export

fetch_xCS <- function(year = 2023, match_id = 'all', ungroup = F, ...){
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

  if(match_id != 'all') {
    return(calc_xcs(match_id))
  }

  pb <- progress::progress_bar$new(
    format = "  downloading :what [:bar] :percent eta: :eta",
    clear = FALSE, total = length(match_ids), width = 60)

  # for each played game, scrape understat xCS
  xcs_df <- data.frame()
  for(i in match_ids){
    tmp <- calc_xcs(i)
    xcs_df <- rbind(xcs_df, tmp)
    if(pb$finished != TRUE) {
      pb$tick(tokens = list(what= glue::glue('match {i}')))
    }
  }

  pb$terminate()
  if(ungroup == T){
    raw <- xcs_df %>%
      lapply(., function(x){
        cbind(x, attacking_team = rev(x$defending_team))
      }) %>%
      do.call(rbind, .)

    return(raw)
  }

  xcs_df %>%
    group_by(defending_team, home_away) %>%
    summarise(xCS = sum(xCS), matches = n()) %>%
    pivot_wider(names_from = home_away, values_from = c(xCS, matches)) %>%
    mutate(xCS_per_game_a = xCS_a / matches_a, xCS_per_game_h = xCS_h / matches_h) %>%
    mutate(total_xCS = xCS_a + xCS_h)


}
