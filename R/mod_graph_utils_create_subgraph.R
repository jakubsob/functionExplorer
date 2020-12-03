#' Create Subgraph
#'
#' @param g Igraph
#' @param node Character
#' @param order Integer
#' @param mode Character
#'
#' @return Igraph
#'
createSubgraph <- function(g, node, order, mode = c("all", "in", "out")) {
  if (is.null(node)) {
    return(g)
  }
  g_ego <- igraph::ego(g, order = order, nodes = node, mode = mode[1])
  igraph::induced_subgraph(g, unlist(g_ego))
}