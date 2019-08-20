#' Fetch xG and xA data from understat
#'
#' Scrapes understat data for the 2018 season from understat for the 17 teams that were in the PL in both 2018 and 2019 seasons
#'
#' @import jsonlite
#' @importFrom magrittr %>%
#' @export
#'
#' @return a data.frame of the full player xg data for the season.
#'
#' @examples
#' df <- fetch_understat_data()
#'
#'

fetch_xg_data <- function(){

teams <- data.frame(name = c('Arsenal', 'Aston_Villa', 'Bournemouth', 'Brighton', 'Burnley', "Chelsea", 'Crystal_Palace', 'Everton', 'Leicester', 'Liverpool', 'Manchester_City',
           'Manchester_United', 'Newcastle_United', 'Norwich','Sheffield_United', 'Southampton', 'Tottenham', 'Watford', 'West_Ham', 'Wolverhampton_Wanderers'),
           number = c(1:20))

message('fetching data from understat...')
understat <- tibble()

for(i in 1:nrow(teams)) {
  tmp <- fetch_understat_data(team = teams$name[i]) %>% mutate(team_name = teams$name[i], team_code = teams$number[i])
  understat <- bind_rows(understat, tmp)
}


fpl_dat <- fetch_player_data()

fpl_dat2 <- fpl_dat %>%
     mutate(full_name = paste0(first_name, ' ', second_name)) %>%
  left_join(understat, by = c('full_name' = 'player_name', 'team' = 'team_code')) %>%
  select(id, full_name,  first_name, second_name, web_name, form, now_cost, team, team_code, element_type, singular_name,  minutes, goals_scored, assists, clean_sheets, goals_conceded, own_goals, penalties_saved, penalties_missed, yellow_cards, red_cards, saves, bonus, bps, influence, creativity, threat, ict_index, vapm, understat_games, understat_minutes, understat_goals, understat_assists, understat_shots, understat_key_passes, xG, xA, npg, npxG,  points_per_game, total_points)


unmatched <- fpl_dat2 %>% filter(is.na(xG))

found1 <- unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG) %>%
  left_join(understat, by=c('web_name' = 'player_name', 'team' = 'team_code'))  %>%
  filter(!is.na(xG))

fpl_dat3 <- fpl_dat2 %>%
  filter(!web_name %in% found1$web_name) %>%
  bind_rows(found1)
  # remove teams that aren't in understat

fpl_dat3_unmatched  <- fpl_dat3 %>%
  filter(is.na(xG))

# now be less fussy about team as player may have moved
found2 <- fpl_dat3_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG) %>%
  left_join(understat, by=c('full_name' = 'player_name'))  %>%
  filter(!is.na(xG))

fpl_dat4 <- fpl_dat3 %>%
  filter(!full_name %in% found2$full_name) %>%
  bind_rows(found2) %>%
  mutate(xA = as.numeric(xA),
         xG = as.numeric(xG),
         understat_games = as.numeric(understat_games))


fpl_dat4_unmatched  <- fpl_dat4 %>%
  filter(is.na(xG))

understat_surnames <- understat %>%
  mutate(understat_surname = unlist(lapply(stringr::str_split(understat$player_name, ' ',simplify = F), function(x) last(x))),
         understat_surname = stringi::stri_trans_general(understat_surname,"Latin-ASCII"))


# focus just on surname and team matching
found3 <- fpl_dat4_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG) %>%
  mutate(fpl_surname = unlist(lapply(stringr::str_split(fpl_dat4_unmatched$second_name, ' ',simplify = F), function(x) last(x))),
         fpl_surname = stringi::stri_trans_general(fpl_surname,"Latin-ASCII")) %>%
  left_join(understat_surnames, by=c('fpl_surname' = 'understat_surname', 'team' = 'team_code'))  %>%
  filter(!is.na(xG))

found4 <- fpl_dat4_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG) %>%
  mutate(fpl_surname = unlist(lapply(stringr::str_split(fpl_dat4_unmatched$second_name, ' ',simplify = F), function(x) first(x))),
         fpl_surname = stringi::stri_trans_general(fpl_surname,"Latin-ASCII")) %>%
  left_join(understat_surnames, by=c('fpl_surname' = 'understat_surname', 'team' = 'team_code'))  %>%
  filter(!is.na(xG))

found4 <- rbind(found3, found4) %>%
  filter(!duplicated(id)) %>%
  mutate(understat_games = as.numeric(understat_games),
         xA = as.numeric(xA),
         xG = as.numeric(xG)) %>%
  select(-fpl_surname)


fpl_dat5 <- fpl_dat4 %>%
  filter(!full_name %in% found4$full_name) %>%
  bind_rows(found4) %>%
  mutate(xA = as.numeric(xA),
         xG = as.numeric(xG),
         understat_games = as.numeric(understat_games))

# unmatched args should only be players that haven't played this season (and thus not in understat data)
fpl_dat5_unmatched  <- fpl_dat5 %>%
  filter(is.na(xG))

# adjust points, points per game and vapm for xA/xg
expected_pts <- fpl_dat5 %>%
  mutate(goal_pts = case_when(element_type == 1 ~ 6,
                              element_type == 2 ~ 6,
                              element_type == 3 ~ 5,
                              element_type == 4 ~ 4)) %>%
  mutate(adjust_assists = (xA - assists) * 3,
         adjust_goals = (xG - goals_scored) * goal_pts) %>%
  mutate(total_points = total_points + adjust_assists + adjust_goals,
         points_per_game = total_points/ understat_games,
         vapm = (points_per_game - 2) / now_cost) %>%
  rename(games = understat_games)



return(expected_pts)

}




