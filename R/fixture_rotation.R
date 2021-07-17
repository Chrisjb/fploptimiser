#' Fixture rotation of fpl_fixtures
#'
#' Calculates correlations between all team's upcoming fixtures to give best rotation possibilities
#'
#'
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @param x An object of class \code{fpl_fixtures}
#' @return An object of class \code{fpl_fixtures}
#' @export
#' @examples
#' fixtures <- fetch_fixtures(n = 5)
#'
#' # visualise fixtures
#' plot(fixtures)
#'
#' # find teams with low correlation fixures (good for rotation)
#' fixture_rotation(fixtures)
fixture_rotation <- function(x, ...) {
  UseMethod("fixture_rotation")
}

#' @describeIn fixture_rotation Default method not implemented yet.
#' @export
fixture_rotation.default <- function(x, ...) {
  stop("fixture_rotation method is only available for objects of class fpl_fixtures.")
}

#' @describeIn fixture_rotation Lowest correlation fixtures for the next n games which should be good for sub rotation.
#' @export
fixture_rotation.fpl_fixtures <- function(x, next_n=10) {


  cor_df <- data.frame()
  teams <- unique(x$team_name)
  for(i in teams) {
    for(j in teams) {
      if(i !=j){
        x1 <- x %>% filter(team_name == i)
        x2 <- x %>% filter(team_name == j)

        fix_len <- min(nrow(x1), nrow(x2))

        x1 <- as.numeric(as.character(x1[1:fix_len,][['difficulty']]))
        x2 <- as.numeric(as.character(x2[1:fix_len,][['difficulty']]))

        c <- data.frame(team1 = i, team2 = j, cor = cor(x1,x2))

        cor_df <- rbind(cor_df, c)
      }
    }
  }

  # remove duplicates
  cor_df %>%
    mutate(team_comb = paste0(pmin(team1,team2),',', pmax(team1,team2))) %>%
    filter(!duplicated(team_comb)) %>%
    select(-team_comb) %>%
    arrange(cor) %>%
    as_tibble()
}


