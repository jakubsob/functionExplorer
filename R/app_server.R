#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  session$userData$help <- reactive(input$help)
  session$userData$active_tab <- reactive(input$tabs_tab)
  data <- FunctionData$new()
  
  # data$download_github("jakubsob/functiondepends", "master")
  # data$find_functions()
  # data$find_dependencies()
  # data$make_graph()
  # data$reset()
  
  callModule(mod_load_data_server, "load_data_ui_1", data)
  callModule(mod_graph_server, "graph_ui_1", data)
}
