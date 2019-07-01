library(rvest)
library(V8)
library(stringr)
library(jsonlite)

understat <- read_html('https://understat.com/team/Arsenal/2018')


understat_dat <- understat %>% html_nodes('script') %>%
  as.character() %>%
  stringr::str_subset('playersData') %>%
  stringi::stri_unescape_unicode()  %>%
  stringr::str_extract('\\[.+\\]') %>%
  jsonlite::fromJSON(flatten = T) %>%
  select(player_name, understat_games = games, understat_minutes = time,
         understat_goals = goals, xG, xA, understat_assists = assists, understat_shots = shots,
         understat_key_passes = key_passes, understat_yellow = yellow_cards, understat_red = red_cards,
         npg, npxG)


fpl_dat <- FPLOptimiseR::fetch_player_data()


fpl_dat2 <- fpl_dat %>% filter(name == 'Arsenal') %>%
  mutate(full_name = paste0(first_name, ' ', second_name))

fpl_dat2 %>%
  full_join(understat_dat, by = c('full_name' = 'player_name')) %>% View
