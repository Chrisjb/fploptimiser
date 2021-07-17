#' Visualize Fixture Difficulty
#'
#' GGplot visualization of an object of class \code{fpl_fixtures}.
#'
#' @import ggplot2
#' @method plot fpl_fixtures
#' @param x An object of class \code{fpl_fixtures}.
#' @return An object of class \code{ggplot2}.
#' @export
#' @examples
#' fixtures <- fetch_fixtures(n = 5)
#'
#' # visualise fixtures
#' plot(fixtures)
#' @seealso \code{\link{fetch_fixtures}}.

plot.fpl_fixtures <- function(x) {

  cols <-c('#851f46','#fc115d','#ebebe4','#2afd8b')


  ggplot2::ggplot(x, aes(gw, forcats::fct_reorder(team_name, -mean_difficulty,.fun = min))) +
    geom_tile(aes(fill = difficulty),
              colour = "white") +
    geom_text(aes(label = ha), size= 3, col = 'grey')+
    scale_fill_manual(values = rev(cols)) +
    theme_fplr()+
    theme(axis.text.x = element_blank()) +
    labs(x = 'Gameweek', y= '')

}
