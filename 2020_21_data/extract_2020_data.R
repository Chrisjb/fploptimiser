opt_formation <- function(objective = 'ppg', custom_df = F, bank = 1000, bench_value = 175, min_games = 2, expected_points_adjust = F, ...) {
  result1 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank,  bench_value = bench_value, gk = 1, def = 5, mid = 4, fwd = 1, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result2 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank,  bench_value = bench_value, gk = 1, def = 4, mid = 5, fwd = 1, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result3 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 5, mid = 3, fwd = 2, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result4 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 4, mid = 4, fwd = 2, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result5 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 3, mid = 5, fwd = 2, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result6 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 4, mid = 3, fwd = 3, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)
  result7 <- optimise_team(objective = objective, custom_df = custom_df, bank = bank, bench_value = bench_value, gk = 1, def = 3, mid = 4, fwd = 3, min_games = min_games, expected_points_adjust = expected_points_adjust, ...)

  if(objective == 'ppg'){
    results <-  data.frame(
      result1 = result1$points_per_game %>% sum(),
      result2 = result2$points_per_game %>% sum(),
      result3 = result3$points_per_game %>% sum(),
      result4 = result4$points_per_game %>% sum(),
      result5 = result5$points_per_game %>% sum(),
      result6 = result6$points_per_game %>% sum(),
      result7 = result7$points_per_game %>% sum()
    )
  } else {
    results <-  data.frame(
      result1 = result1$total_points %>% sum(),
      result2 = result2$total_points %>% sum(),
      result3 = result3$total_points %>% sum(),
      result4 = result4$total_points %>% sum(),
      result5 = result5$total_points %>% sum(),
      result6 = result6$total_points %>% sum(),
      result7 = result7$total_points %>% sum()
    )
  }


  return(get(names(which.max(results))))
}

fpl_api <-  jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")


fpl_api$elements %>% View


library(pdftools)
txt <- pdftools::pdf_text('https://resources.premierleague.com/premierleague/document/2020/08/15/9f20a42f-6905-4ad9-8878-c49b44f1f882/2020-21-FPL-Player-prices-by-club-150820.pdf')

price_text <- str_c(txt[[1]],txt[[2]],txt[[3]],
                       txt[[4]],txt[[5]],txt[[6]],
                       txt[[7]],txt[[8]],txt[[9]])
price_text

x1 <- {price_text %>%
  str_split('  ')}[[1]]

x1 <- x1[x1 != '']

x1 <- x1[6:(length(x1)-1)]

prices_2021 <- matrix(x1,ncol=5,byrow = TRUE) %>%
  as.data.frame() %>%
  setNames(c('name','position','team','points','price')) %>%
  mutate(name = str_trim(name),
         position = str_trim(position),
         team = str_trim(team),
         points= as.numeric(points),
         price = as.numeric(str_extract(price,'[0-9\\.]+'))) %>%
  filter(position != 'Player')


# write_csv(prices_2021, '2020_21_data/prices_2020.csv')

prices_2021 <- read_csv('2020_21_data/prices_2020.csv') %>%
  select(name, price_2020=price)

## get last years xg data
xg_20_prices <- read_csv('2019_data/xg_data_2019.csv') %>%
  select(-X1) %>%
  # left_join(prices_2021, by = c('web_name'='name')) %>%
  # mutate(now_cost = price_2020*10) %>%
  filter(!duplicated(id))  %>%
  filter(!id %in% c(437,388, 232, 226, 458, 461, 462,394))


pts_20_prices <- read_csv('2019_data/player_data_2019.csv') %>%
  select(-X1) %>%
  # left_join(prices_2021, by = c('web_name'='name')) %>%
  # mutate(now_cost = price_2020*10) %>%
  filter(!duplicated(id))  %>%
  filter(!id %in% c(355, 437))



optimise_team(objective = 'ppg', bank = 955,
              bench_value = 195, gk = 0, def = 3, mid = 5, fwd = 2, min_games = 15, custom_df = pts_20_prices) %>%
  select(id,element_type, now_cost, web_name, games, goals_scored, assists, clean_sheets, points_per_game, total_points)


optimise_team(objective = 'ppg', bank = 955,
                           bench_value = 195, gk = 0, def = 3, mid = 4, fwd = 3, min_games = 15, custom_df = xg_20_prices) %>%
  select(id,element_type, now_cost, web_name, games, goals_scored, assists, clean_sheets, points_per_game, total_points)



opt_formation(objective = 'ppg', bench_value = 195, custom_df = xg_20_prices, bank = 1000, min_games = 15) %>%
  select(id, element_type, now_cost, web_name, full_name, games, goals_scored, assists, clean_sheets, team_xcs, xG, xA, npxG, points_per_game, total_points)


