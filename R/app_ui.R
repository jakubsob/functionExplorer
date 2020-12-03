#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    shiny.semantic::semanticPage(
      div(
        class = "ui raised segment",
        style = "
          height: 50px; 
          width: 100%; 
          display: flex;
          justify-content: space-between;
        ",
        div(
          style = "display: inline-block; text-align: left; align-self: center;",
          h2(
            "Function Explorer"
          )
        ),
        tags$span(
          style = "display: inline-block; text-align: right; align-self: center;",
          div(
            shiny.semantic::actionButton(
              "help",
              icon("question mark"),
              class = "align-rights;"
            )
          )
        )
      ),
      shiny.semantic::tabset(
        tabs = list(
          list(
            menu = "Load data",
            id = "load_tab",
            content = mod_load_data_ui("load_data_ui_1")
          ),
          list(
            menu = "Plot", 
            id = "plot_tab",
            content = mod_graph_ui("graph_ui_1")
          )
        ),
        active = "load_tab",
        id = "tabs"
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'functionExplorer'
    ),
    shinyjs::useShinyjs(),
    rintrojs::introjsUI()
  )
}

