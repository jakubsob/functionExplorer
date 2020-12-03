#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  help <- reactive(input$help)
  curr_tab <- reactive(input$tabs_tab)
  
  loaded_data <- callModule(mod_load_data_server, "load_data_ui_1", help, curr_tab)
  callModule(mod_graph_server, "graph_ui_1", help, curr_tab, loaded_data)
}
