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

Get expected points (adjusting for xG, xA and xCS) data from understat
``` r
df_xp <- fetch_xg_data()
```


Optimise your team for a given formation, team value, and assumed bench value. You'll want to have chosen your bench already (there is little point in optimising players on your bench as they will not earn you points a lot of the time).

You can optimise your team based on maximising total points from the historic dataset (full season by default) or points per game ('points' or 'ppg' respectively). If using 'ppg' you will want to set a `min_games` value  so that players with high points per game but with relatively few games played are avoided.

``` r
result <- optimise_team(objective = 'ppg', bank = 1000, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3, min_games = 3)
```

Optimise team based on expected points (adjusting for xG, xA and xCS). We set `expected_points_adjust = T`. THIS IS NOT CURRENTLY RECOMMENDED (see note below)
``` r
result_xp <- optimise_team(objective = 'ppg', bank = 1000, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3, min_games = 3, expected_points_adjust = TRUE)
```

NOTE: due to the fact that understat data is not currently memoized (on the to-do list), it is best to download the understat data first and then use it in the `custom_df` parameter to avoid downloading again each time you run the function:

```r
df_xp <- fetch_xg_data()

result_xp <- optimise_team(objective = 'ppg', bank = 1000, bench_value = 170, gk = 1, def = 3, mid = 4, fwd = 3, min_games = 3, custom_df = df_xp)
```
