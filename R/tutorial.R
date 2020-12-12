#' Tutorial for Load Data module
#'
#' @param ns Namespace
#'
#' @return Data.frame
#' @noRd
tutorial_mod_load_data <- function(ns) {
  data.frame(
    element = paste0("#", ns(c("github_input_div", "toggle", "parse", "reset"))),
    intro = c(
      "Click to download repository for exploration",
      "If this toggle is on then only user defined functions will be parsed",
      "Click this button to run parser",
      "Click this button to delete uploaded files"
    )
  )
}

#' Tutorial for Graph module
#'
#' @param ns Namespace
#'
#' @return Data.frame
#' @noRd
tutorial_mod_graph <- function(ns) {
  data.frame(
    element = paste0("#", ns(c("select_node", "select_order", "select_mode", "reset_button"))),
    intro = c(
      "Use this selection box to select which node to show.
              You can click on node on graph as well.",
      "Number in this field defines how distant nodes to the selected
              one will appear on the graph.",
      "Select mode of edges. \"In\" means that only nodes to which
              selected node points will be shown.",
      "Click this button to reset settings."
    
  ))
}