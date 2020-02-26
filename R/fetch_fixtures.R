root <- jsonlite::fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

fix <- jsonlite::fromJSON('https://fantasy.premierleague.com/api/fixtures')

fixtures_df <- data.frame(
  time = as.Date(fix$kickoff_time),
  gw = fix$event,
  home = fix$team_h,
  away = fix$team_a,
  difficulty_home = fix$team_h_difficulty,
  difficulty_away = fix$team_a_difficulty,
  home_opponent = fix$team_a,
  away_opponent = fix$team_h,
  played = fix$finished,
  stringsAsFactors = FALSE
)

team_names <- root$teams[c('id','name')]


fixture_difficulty <- fixtures_df %>%
  tidyr::pivot_longer(cols=c('home','away'), names_to = 'ha',values_to = 'id') %>%
  dplyr::left_join(team_names) %>%
  mutate(difficulty = if_else(ha == 'home', difficulty_home, difficulty_away),
         difficulty = as.factor(difficulty),
          opponent_id = if_else(ha == 'home', home_opponent, away_opponent)) %>%
  dplyr::left_join(team_names, by = c('opponent_id'='id'), suffix = c('','_opponent')) %>%
  filter(played != T) %>%
  select(time, gw, team_id= id, team_name = name, ha, played, opponent = name_opponent, difficulty)

# deal with double fixtures
fd_summary <- fixture_difficulty %>%
  mutate(ha = if_else(ha =='home','h','a')) %>%
  group_by(gw, team_name) %>%
  arrange(difficulty) %>%
  filter(!duplicated(team_name)) %>%
  ungroup()

cols <-c('#851f46','#fc115d','#ebebe4','#2afd8b')
p <- ggplot(fd_summary, aes(gw, forcats::fct_rev(team_name))) +
    geom_tile(aes(fill = difficulty),
              colour = "white") +
    geom_text(aes(label = ha), size= 3, col = 'grey')+
    scale_fill_manual(values = rev(cols)) +
    theme_fplr()+
    labs(x = 'Gameweek', y= '')

p

# just next_n
n <- 5
next_n<- fd_summary %>%
  filter(gw %in% seq(min(gw,na.rm=T), min(gw, na.rm=T)+n-1))

next_n %>%
  group_by(team_name) %>%
  summarise(mean_difficulty = mean(as.numeric(as.character(difficulty))),
            median_difficulty = median(as.numeric(as.character(difficulty)))) %>%
  arrange(mean_difficulty)


ggplot(next_n, aes(gw, forcats::fct_rev(team_name))) +
  geom_tile(aes(fill = difficulty),
            colour = "white") +
  scale_fill_manual(values = rev(cols)) +
  theme_fplr()+
  labs(x = 'Gameweek', y= '')
