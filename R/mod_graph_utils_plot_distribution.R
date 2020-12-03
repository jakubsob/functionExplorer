#' Plot Distribution
#'
#' @param g IGraph
#'
#' @return ggplot
#'
#' @import ggplot2
#' 
#' @noRd
plotDistribution <- function(g, mode = "all") {
  if (is.null(g) | length(igraph::V(g)) < 2) {
    return(NULL)
  }
  tibble::tibble(
    Density = igraph::degree_distribution(g, mode = mode),
    Degree = 1:length(Density) - 1
  ) %>%
    ggplot(aes(x = Degree, y = Density)) +
    geom_col() +
    theme_light() +
    labs(title = sprintf("Node %s degree distribution", mode)) +
    theme(rect = element_rect(fill = "transparent"))
}