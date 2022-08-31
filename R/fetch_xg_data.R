#' Fetch xG and xA data from understat
#'
#' Scrapes xA and xG data for a given season from understat
#'
#' @param year the season for which we want to scrape data from understat. Currently covers only teams who were in the premier league in either 2018/19 or 2019/20
#'
#' @import jsonlite
#' @import tidyr
#' @import glue
#' @importFrom magrittr %>%
#' @import rvest
#' @export
#'
#' @return a data.frame of the full player xg data for the season.
#'
#' @examples
#' df <- fetch_xg_data(year = 2020)
#'
#'

fetch_xg_data <- function(year = 2022, check_data=FALSE){

suppressWarnings(
  epl_teams_raw <- xml2::read_html(glue::glue('https://understat.com/league/EPL/{year}')) %>%
    rvest::html_nodes('script')  %>%
    stringr::str_subset('teamsData') %>%
    stringi::stri_unescape_unicode()
)


  # official fpl player data/team data
  fpl_dat <- fetch_player_data()




  team_names <- {epl_teams_raw %>%
      stringr::str_extract("(?<=JSON.parse\\(').+(?='\\);)") %>%
      jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>%
      lapply(.,function(x) x$title) %>%
      do.call(rbind,.)}[,1] %>%
    stringr::str_replace_all(' ','_')

  teams <- data.frame(name = team_names) %>%
    arrange(name) %>%
    mutate(number=1:20)



message('fetching data from understat...')
understat <- tibble()

for(i in 1:nrow(teams)) {
  tmp <- fetch_understat_data(team = teams$name[i], year = year) %>%
    mutate(team_name = teams$name[i], team_code = teams$number[i],
           understat_games = as.numeric(understat_games),
           xG = as.numeric(xG),
           xA = as.numeric(xA))
  understat <- bind_rows(understat, tmp) %>%
    mutate(player_name = stringi::stri_trans_general(str = player_name, id = "Latin-ASCII"))
}


# team IDs may not match if comparing two different seasons so match on name instead... this list may need updating in later seasons
team_names_common <- data.frame(fpl_name =unique(fpl_dat$name)) %>%
  mutate(understat_name = stringr::str_replace_all(fpl_name, ' ','_'),
         understat_name = stringr::str_replace(understat_name, 'Man_','Manchester_'),
         understat_name = stringr::str_replace(understat_name, '_Utd','_United'),
         understat_name =stringr::str_replace(understat_name, '(Newcastle|Sheffield)','\\1_United'),
         understat_name =dplyr::if_else(understat_name== 'Spurs','Tottenham',understat_name),
         understat_name =dplyr::if_else(understat_name== 'Wolves','Wolverhampton_Wanderers',understat_name)
         )



# match understat data to fpl data by player name
fpl_dat2 <- fpl_dat %>%
  left_join(team_names_common, by = c('name'= 'fpl_name')) %>%
     mutate(full_name = paste0(first_name, ' ', second_name),
            full_name = stringi::stri_trans_general(str = full_name, id = "Latin-ASCII")) %>%
  left_join(understat, by = c('full_name' = 'player_name', 'understat_name' = 'team_name')) %>%
  select(id, full_name,  first_name, second_name, web_name, form, now_cost, team= understat_name, team_id, element_type, singular_name,  minutes, goals_scored, assists, clean_sheets, goals_conceded, own_goals, penalties_saved, penalties_missed, yellow_cards, red_cards, saves, bonus, bps, influence, creativity, threat, ict_index, vapm, understat_games, understat_minutes, understat_goals, understat_assists, understat_shots, understat_key_passes, xG, xA, npg, npxG,  points_per_game, total_points)


# match unmatched understat data to fpl data by web name
unmatched <- fpl_dat2 %>% filter(is.na(xG))

found1 <- unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG) %>%
  left_join(understat, by=c('web_name' = 'player_name', 'team' = 'team_name'))  %>%
  filter(!is.na(xG))

fpl_dat3 <- fpl_dat2 %>%
  filter(!web_name %in% found1$web_name) %>%
  bind_rows(found1)

# match unmatched understat data to fpl data by last name (removing special characters)
fpl_dat3_unmatched  <- fpl_dat3 %>%
  filter(is.na(xG))

understat_surnames <- understat %>%
  mutate(understat_surname = unlist(lapply(stringr::str_split(understat$player_name, ' ',simplify = F), function(x) nth(x,2))),
         understat_surname = if_else(is.na(understat_surname), player_name, understat_surname),
         understat_surname = stringr::str_replace_all(understat_surname,'&#039;',"'" ),
         understat_surname = stringi::stri_trans_general(understat_surname,"Latin-ASCII"),
         understat_surname = stringr::str_to_upper(understat_surname)) %>%
  mutate(understat_firstname = unlist(lapply(stringr::str_split(understat$player_name, ' ',simplify = F), function(x) nth(x,1))),
         understat_firstname = if_else(is.na(understat_firstname), player_name, understat_firstname),
         understat_firstname = stringr::str_replace_all(understat_firstname,'&#039;',"'" ),
         understat_firstname = stringi::stri_trans_general(understat_firstname,"Latin-ASCII"),
         understat_firstname = stringr::str_to_upper(understat_firstname))

found2 <- fpl_dat3_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -understat_yellow,-understat_red,-team_code) %>%
  mutate(fpl_surname = unlist(lapply(stringr::str_split(fpl_dat3_unmatched$web_name, ' |-',simplify = F), function(x) last(x))),
         fpl_surname = stringi::stri_trans_general(fpl_surname,"Latin-ASCII"),
         fpl_surname = stringr::str_to_upper(fpl_surname)) %>%
  left_join(understat_surnames, by=c('fpl_surname' = 'understat_surname', 'team' = 'team_name'))  %>%
  filter(!is.na(xG))

fpl_dat4 <- fpl_dat3 %>%
  filter(!web_name %in% found2$web_name) %>%
  bind_rows(select(found2,-player_name, -fpl_surname, -understat_firstname))

fpl_dat4_unmatched <- fpl_dat4 %>%
  filter(is.na(xG)) %>%
  filter(minutes >0)


# based on FPL webname matching understat firstname
found3 <- fpl_dat4_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -understat_yellow,-understat_red,,-team_code) %>%
  mutate(fpl_surname = unlist(lapply(stringr::str_split(fpl_dat4_unmatched$web_name, ' |-',simplify = F), function(x) last(x))),
         fpl_surname = stringi::stri_trans_general(fpl_surname,"Latin-ASCII"),
         fpl_surname = stringr::str_to_upper(fpl_surname)) %>%
  left_join(understat_surnames, by=c('fpl_surname' = 'understat_firstname', 'team' = 'team_name'))  %>%
  filter(!is.na(xG))

fpl_dat5 <- fpl_dat4 %>%
  filter(!web_name %in% found3$web_name) %>%
  bind_rows(select(found3,-player_name, -fpl_surname, -understat_surname))


fpl_dat5_unmatched  <- fpl_dat5 %>%
  filter(is.na(xG)) %>%
  filter(minutes >0)


# based on FPL firstname matching understat surname
found4 <- fpl_dat5_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -understat_yellow,-understat_red,-team_code) %>%
  mutate(fpl_firstname = stringr::str_to_upper(first_name)) %>%
  left_join(understat_surnames, by=c('fpl_firstname' = 'understat_surname', 'team' = 'team_name'))  %>%
  filter(!is.na(xG))

fpl_dat6 <- fpl_dat5 %>%
  filter(!web_name %in% found4$web_name) %>%
  bind_rows(select(found4,-player_name, -fpl_firstname,-understat_firstname))

fpl_dat6_unmatched  <- fpl_dat6 %>%
  filter(is.na(xG)) %>%
  filter(minutes >0)


# based on first part of FPL surname and understat surname
found5 <- fpl_dat6_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -understat_yellow,-understat_red, -team_code) %>%
  mutate(fpl_surname_1 = unlist(lapply(stringr::str_split(fpl_dat6_unmatched$second_name, ' |-',simplify = F), function(x) first(x))),
         fpl_surname_1 = stringr::str_to_upper(fpl_surname_1)) %>%
  left_join(understat_surnames, by=c('fpl_surname_1' = 'understat_surname', 'team' = 'team_name'))  %>%
  filter(!is.na(xG))

found5_1 <- fpl_dat6_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -understat_yellow,-understat_red, -team_code) %>%
  mutate(second_name2 = stringr::str_to_upper(second_name)) %>%
  left_join(understat_surnames, by=c('second_name2' = 'understat_surname', 'team' = 'team_name'))  %>%
  select(-second_name2) %>%
  filter(!is.na(xG))

fpl_dat7 <- fpl_dat6 %>%
  filter(!web_name %in% found5$web_name) %>%
  bind_rows(select(found5,-player_name,-understat_firstname,-fpl_surname_1)) %>%
  filter(!web_name %in% found5_1$web_name) %>%
  bind_rows(select(found5_1,-player_name,-understat_firstname))


# unmatched args should only be players that haven't played this season (and thus not in understat data)
fpl_dat7_unmatched  <- fpl_dat7 %>%
  filter(is.na(xG)) %>%
  filter(minutes >0)


found6 <- fpl_dat7_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -team_code) %>%
  left_join(mutate(understat, player_name= stringr::str_replace_all(player_name, '-',' ')), by=c('full_name' = 'player_name', 'team' = 'team_name'))  %>%
  filter(!is.na(xG))



fpl_dat8 <- fpl_dat7 %>%
  filter(!web_name %in% found6$web_name) %>%
  bind_rows(found6)


fpl_dat8_unmatched  <- fpl_dat8 %>%
  filter(is.na(xG)) %>%
  filter(minutes >0)


# unmatched players may have a different club now...

found7 <- fpl_dat8_unmatched %>%
  select(-understat_games, -understat_minutes, -understat_goals, -xG, -xA, -understat_assists, -understat_shots, -understat_key_passes, -npg, -npxG, -team_code) %>%
  left_join(understat, by=c('full_name' = 'player_name')) %>%
  filter(!is.na(xG))

fpl_dat9 <- fpl_dat8 %>%
  filter(!web_name %in% found7$web_name) %>%
  bind_rows(found7)


# unmatched args should only be players that haven't played this season (and thus not in understat data)
fpl_dat9_unmatched  <- fpl_dat9 %>%
  filter(is.na(xG)) %>%
  filter(minutes >0)



if(check_data==TRUE){
  if(nrow(fpl_dat9_unmatched) >0){
    message(glue::glue('{nrow(fpl_dat9_unmatched)} unmatched players: [{glue::glue_collapse(fpl_dat9_unmatched$web_name,", ")}]. Please raise an issue on Github.'))
  } else {
    message('0 unmatched players.')
  }
}





# FETCH XCS DATA FROM UNDERSTAT
understat_xCS <- fetch_understat_xCS(year=year)

understat_xCS <- understat_xCS %>%
  mutate(xCS_per_game = xCS / matches)


# adjust points, points per game and vapm for xA/xg
expected_pts <- fpl_dat9 %>%
  select(-team_name, -matches(c('\\.x','\\.y'))) %>%
  left_join(understat_xCS, by = c('team' = 'defending_team')) %>%
  mutate(goal_pts = case_when(element_type == 1 ~ 6,
                              element_type == 2 ~ 6,
                              element_type == 3 ~ 5,
                              element_type == 4 ~ 4)) %>%
  mutate(cs_pts = case_when(element_type == 1 ~ 4,
                              element_type == 2 ~ 4,
                              element_type == 3 ~ 1,
                              element_type == 4 ~ 0)) %>%
  mutate(adjust_assists = (xA - assists) * 3,
         adjust_goals = (xG - goals_scored) * goal_pts,
         adjust_cs = (xCS_per_game * understat_games - clean_sheets) * cs_pts) %>%
  mutate(total_points = total_points + adjust_assists + adjust_goals + adjust_cs,
         points_per_game = total_points/ understat_games,
         vapm = (points_per_game - 2) / now_cost) %>%
  rename(games = understat_games) %>%
  select(id, full_name, web_name, form, now_cost, team_id, team_name=team,singular_name,minutes, goals=goals_scored, assists, clean_sheets,
         goals_conceded, own_goals, penalties_saved, penalties_missed, yellow_cards, red_cards, saves,bonus,bps, influence,
         creativity, threat, ict_index, vapm, games, understat_minutes, understat_goals, understat_assists, understat_shots,
         understat_key_passes, xG, xA, xCS, xCS_per_game, npg, npxG, points_per_game, total_points, matches)



return(expected_pts)

}




