#' utility function to fetch xCS data for teams

# function to calculate xCS for each match
calc_xcs<- function(match_id, verbose = T) {
  if(verbose==T) message('fetching clean sheet data for match ', match_id)

  understat <- xml2::read_html(paste0('https://understat.com/match/',match_id))

  understat_dat <- understat %>% rvest::html_nodes('script') %>%
    as.character() %>%
    stringr::str_subset('shotsData') %>%
    stringi::stri_unescape_unicode()  %>%
    stringr::str_extract('\\{.+\\}') %>%
    jsonlite::fromJSON(flatten = T) %>%
    do.call(rbind, .) %>%
    mutate(defending_team = if_else(h_a == 'h', a_team, h_team),
           defending_team = stringr::str_replace_all(defending_team, ' ','_'),
           xGA = as.numeric(xG)) %>%
    select(minute, xGA, result, defending_team)

  # work out xCS
  understat_dat %>%
    group_by(defending_team) %>%
    summarise(xCS = prod((1-xGA)))
}


fetch_understat_xCS <- function(...){
  # get IDs of each game played
  understat_matches <- xml2::read_html('https://understat.com/league/EPL/2019')

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
    group_by(defending_team) %>%
    summarise(xCS = sum(xCS), matches = n())


}

