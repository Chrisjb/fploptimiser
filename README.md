# FPLOptimiseR

## Installation
You can install FPLOptimiseR from github with:
``` r
# install.packages("devtools")
devtools::install_github('Chrisjb/fploptimiser')
```


# Usage

Get the historic player data from the FPL API for your own analysis
``` r
library(FPLOptimiseR)

df <- fetch_player_data()
```

Optimise your team for a given formation, team value, and assumed bench value. You'll want to have chosen your bench already (there is little point in optimising players on your bench as they will not earn you points a lot of the time).

You can optimise your team based on maximising total points from the historic dataset (full season by default) or points per game ('points' or 'ppg' respectively). If using 'ppg' you will want to set a `min_games` value  so that players with high points per game but with relatively few games played are avoided.

``` r
library(FPLOptimiseR)

result <- optimise_team(objective = 'ppg', bank = 1000, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3, min_games = 13)
```


# Examples

``` r
df <- fetch_player_data()

# RUN OPTIMISATION  ON POINTS PER GAME FOR A 4-3-3 formation, with a bench value of 175 and minimum games of 13
result <- optimise_team(objective = 'ppg', custom_df = F, bench_value = 175, gk = 1, def = 4, mid = 3, fwd = 3, min_games = 13)
sum(result$points_per_game)


# RUN OPTIMISATION  ON TOTAL POINTS FOR A 3-4-3 formation, with a bench value of 170
result <- optimise_team(objective = 'points', custom_df = F, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3)
sum(result$total_points)

# USING CUSTOM DATA TO FILTER OUT UNWANTED RESULTS
df <- fetch_player_data() %>%
  # must have played more than 13 games
  filter(games >= 13) %>%
  # I don't want to consider Milivojevic or Alonso
  filter(! id %in% c(134, 103))

# adjust zaha's points as he's now midfield (1 extra pt for goal and point for cs)
df[df$id == 133,]$total_points <- df[df$id == 133,]$total_points + df[df$id == 133,]$goals_scored + df[df$id == 133,]$clean_sheets
df[df$id == 133,]$points_per_game <- df[df$id == 133,]$total_points / df[df$id == 133,]$games

# same for perez
df[df$id == 265,]$total_points <- df[df$id == 265,]$total_points + df[df$id == 265,]$goals_scored + df[df$id == 265,]$clean_sheets
df[df$id == 265,]$points_per_game <- df[df$id == 265,]$total_points / df[df$id == 265,]$games

# downgrade jota and delofeou's stats
df[df$id == 363,]$total_points <- df[df$id == 363,]$total_points - df[df$id == 363,]$goals_scored - df[df$id == 363,]$clean_sheets
df[df$id == 363,]$points_per_game <- df[df$id == 363,]$total_points / df[df$id == 363,]$games

df[df$id == 410,]$total_points <- df[df$id == 410,]$total_points - df[df$id == 410,]$goals_scored - df[df$id == 410,]$clean_sheets
df[df$id == 410,]$points_per_game <- df[df$id == 410,]$total_points / df[df$id == 410,]$games

# RUN OPTIMISATION WITH CUSTOM DATA (using custom_df = df option)
result <- optimise_team(objective = 'ppg', custom_df = df, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3)
sum(result$points_per_game)
```
