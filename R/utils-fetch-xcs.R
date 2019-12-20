#' utility function to fetch xCS data for teams

# function to calculate xCS
calc_xcs<- function(match_id, verbose = TRUE, details = FALSE) {
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
           home_away = if_else(h_a == 'h', 'a', 'h'),
           xGA = as.numeric(xG),
           GA = if_else(h_a == 'h', h_goals, a_goals)) %>%
    select(home_away, minute, xGA, result, defending_team, player, shotType, player_assisted, lastAction, GA)

  # check if one team had no shots at all
  if(length(unique(understat_dat$defending_team)) == 1){
    understat_check <- understat %>% rvest::html_nodes('script') %>%
      as.character() %>%
      stringr::str_subset('shotsData') %>%
      stringi::stri_unescape_unicode()  %>%
      stringr::str_extract('\\{.+\\}') %>%
      jsonlite::fromJSON(flatten = T) %>%
      do.call(rbind, .) %>%
      mutate(defending_team = if_else(h_a == 'h', a_team, h_team),
             defending_team = stringr::str_replace_all(defending_team, ' ','_'),
             home_away = if_else(h_a == 'h', 'a', 'h'),
             xGA = as.numeric(xG),
             GA = if_else(h_a == 'h', h_goals, a_goals))

    teams <- c(understat_check$h_team[1],understat_check$a_team[1])
    team_no_shots_against <- teams[! teams == unique(understat_dat$defending_team)]
    team_no_shots_against <- str_replace_all(team_no_shots_against, ' ', '_')
    additional_row <- understat_dat[1,]  %>%
      mutate(home_away = if_else(home_away == 'h', 'a', 'h'),
             minute = '0',
             xGA = 0,
             result = NA_character_,
             defending_team = team_no_shots_against,
             player = NA_character_,
             shotType = NA_character_,
             player_assisted = NA_character_,
             lastAction = 'None',
             GA = '0'
             )

    if(details != TRUE){
      additional_row <- additional_row %>%
        group_by(defending_team, home_away) %>%
        summarise(xCS = prod((1-xGA)),
                  CS = if_else(last(GA) == 0, 1, 0))
    }

  } else {
    additional_row <- data.frame()
  }


  if(details ==TRUE) {
    return(understat_dat %>%
             bind_rows(additional_row))
  }

  # work out xCS
  understat_dat %>%
    group_by(defending_team, home_away) %>%
    summarise(xCS = prod((1-xGA)),
              CS = if_else(last(GA) == 0, 1, 0)) %>%
    bind_rows(additional_row)
}


# function to scrape xcs data for each match
fetch_understat_xCS <- function(year = 2019, ...){
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
    group_by(defending_team) %>%
    summarise(xCS = sum(xCS), matches = n())


}




