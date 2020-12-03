#' Create Node Card
#'
#' @param g Igraph
#' @param node Character
#' @param functions Data.frame
#'
#' @return A card
#'
#' @noRd
createNodeCard <- function(g, node, functions) {
  if (is.null(g) | is.null(node) | is.null(functions)) {
    return(NULL)
  }
  div(
    style = "position: relative; top: 5px;",
    shiny.semantic::card(
      style = "width: 100%",
      div(
        style = "width: 100%;",
        class = "content",
        div(
          style = "width: 100%;",
          class = "header", 
          node
        ),
        div(
          style = "width: 100%;",
          class = "meta",
          "Node"
        ),
        div(
          style = "width: 100%; word-break: break-all; word-wrap: break-all",
          class = "description",
          shiny.semantic::list_container(
            list(
              list(
                header = "In degree",
                icon = "arrow right",
                description = igraph::degree(g, node, mode = "in")
              ), 
              list(
                header = "Out degree",
                icon = "arrow left",
                description = igraph::degree(g, node, mode = "out")
              ),
              list(
                header = "Total degree",
                icon = "exchange",
                description = igraph::degree(g, node, mode = "all")
              ),
              list(
                header = "Source file",
                icon = "file",
                description = functions %>% 
                  dplyr::filter(Function == node) %>% 
                  dplyr::pull(Source)
              ),
              list(
                header = "Path",
                icon = "folder outline",
                description = functions %>% 
                  dplyr::filter(Function == node) %>% 
                  dplyr::pull(Path) %>% 
                  tolower()
              )
            ),
            is_divided = FALSE
          )
        )
      )
    )
  )
}