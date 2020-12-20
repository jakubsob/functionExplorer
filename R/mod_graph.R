#' graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom magrittr %>% 
mod_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        width = 3,
        div(
          class = "ui raised segment",
          div(class = "ui horizontal divider", "Node name", style = "font-size: 12px;"),
          shiny.semantic::dropdown_input(
            ns("select_node"),
            choices = "",
            default_text = "You can select node here or by clicking on plot"
          ),
          div(class = "ui horizontal divider", "Neighbourhood degree", style = "font-size: 12px;"),
          shiny.semantic::numericInput(
            ns("select_order"), 
            label = "",
            value = 1,
            min = 1,
            max = 10
          ),
          div(class = "ui horizontal divider", "Edge mode", style = "font-size: 12px;"),
          shiny.semantic::selectInput(
            ns("select_mode"),
            label = "",
            choices = c("In" = "in", "Out" = "out", "All" = "all"),
            selected = "all"
          ),
          div(class = "ui horizontal divider", "Reset settings", style = "font-size: 12px;"),
          shiny.semantic::actionButton(
            ns("reset_button"), 
            label = "",
            icon = icon("remove"),
            class = "basic",
            width = "100%"
          )
        ),
        uiOutput(ns("selected_node_data"))
      ),
      main_panel = shiny.semantic::main_panel(
        width = 10,
        div(
          div(
            class = "ui raised segment",
            visNetwork::visNetworkOutput(
              ns("plot"),
              width = "100%",
              height = "400px"
            ) %>%
              shinycssloaders::withSpinner()
          ),
          div(
            class = "ui raised segment",
            plotOutput(
              ns("distribution_plot"),
              width = "100%",
              height = "200px"
            ) %>% 
              shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}
    
#' graph Server Function
#'
#' @noRd 
#' @importFrom magrittr %>% 
#' @importFrom rlang %||%
mod_graph_server <- function(input, output, session, data){
  ns <- session$ns
  
  # Session data:
  #   - active_tab: name of activetab
  #   - help: reactive, triggered by clicking help button
  active_tab <- session$userData$active_tab
  help <- session$userData$help
  
  # Reactive values from reacting with graph 
  graph_data <- reactiveValues(selected_node = NULL)
  
  
  observeEvent(help(), {
    # Trigger tutorial for this tab only when this tab is selected
    # Without this check, tutorial steps from tabs are messed up
    if (active_tab() != "plot_tab") return()
    rintrojs::introjs(
      session, 
      options = list(steps = reactive(tutorial_mod_graph(ns))())
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  observeEvent(input$node_id, {
    graph_data$selected_node <- input$node_id
  })
  
  
  observeEvent(active_tab(), {
    validate(need(data$get_graph(), ""))
    shiny.semantic::update_dropdown_input(
      session = session,
      input_id = "select_node",
      choices = c("", sort(names(igraph::V(data$get_graph())))),
      value = ""
    )
  }, ignoreInit = TRUE)

  
  observeEvent(graph_data$selected_node, {
    validate(need(data$get_graph(), ""))
    shiny.semantic::update_dropdown_input(
      session = session, 
      input_id = "select_node",
      choices = c("", sort(names(igraph::V(data$get_graph())))),
      value = graph_data$selected_node
    )
  })

  
  observeEvent(input$reset_button, {
    graph_data$selected_node <- NULL
    graph_data$subgraph <- data$get_graph()
    nodes <- if (is.null(data$get_graph())) {
      NULL
    } else {
      c("", sort(names(igraph::V(data$get_graph()))))
    }
    shiny.semantic::update_dropdown_input(
      session = session,
      input_id = "select_node",
      choices = nodes,
      value = ""
    )
    shiny.semantic::update_numeric_input(
      session = session,
      input_id = "select_order",
      value = 1
    )
    shiny.semantic::updateSelectInput(
      session = session,
      inputId = "select_mode",
      label = "",
      selected = "all"
    )
  })
  
  
  observeEvent(input$select_node, {
    req(input$select_node)
    graph_data$selected_node <- input$select_node
  })
  
  
  output$plot <- visNetwork::renderVisNetwork({
    if (!data$is_initialized()) return(NULL)
    data$plot(
      node = graph_data$selected_node,
      mode = input$select_mode,
      order = input$select_order
    ) %>% 
      visNetwork::visEvents(select = sprintf(
        "function(nodes) {
          Shiny.setInputValue('%s', nodes.nodes);
        ;}", ns("node_id")
      ))
  })
  
  
  output$selected_node_data <- renderUI({
    if (!data$is_initialized()) return(NULL)
    createNodeCard(
      data$get_graph(
        node = graph_data$selected_node,
        mode = input$select_mode,
        order = input$select_order
      ), 
      graph_data$selected_node,
      data$get_functions()
    )
  })
  
  
  output$distribution_plot <- renderPlot({
    if (!data$is_initialized()) return(NULL)
    data$plot_distribution(
      node = graph_data$selected_node,
      mode = input$select_mode,
      order = input$select_order
    )
  })
}
    
## To be copied in the UI
# mod_graph_ui("graph_ui_1")
    
## To be copied in the server
# callModule(mod_graph_server, "graph_ui_1")
 
