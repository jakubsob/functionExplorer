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
            networkD3::forceNetworkOutput(
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
mod_graph_server <- function(input, output, session, help, tab, loaded_data){
  ns <- session$ns
  
  intro <- reactive(
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
      )
  ))

  observeEvent(help(), {
    # Trigger tutorial for this tab only when this tab is selected
    # Without this check, tutorial steps from tabs are messed up
    if (tab() != "plot_tab") return()
    rintrojs::introjs(session, options = list(steps = intro()))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  graph_data <- reactiveValues(
    selected_node = NULL,
    graph = NULL,
    subgraph = NULL
  )
  
  observeEvent(loaded_data$dependencies(), {
    dependencies <- loaded_data$dependencies()
    if (is.null(dependencies)) return()
    vertices <- unique(c(dependencies$Source, dependencies$Target))
    vertices <- vertices[!is.na(vertices)]
    graph_data$graph <- igraph::graph_from_data_frame(
      dependencies %>% 
        dplyr::select(Source, Target) %>% 
        dplyr::filter(!is.na(.)),
      vertices = vertices,
      directed = TRUE
    )
  })
  
  
  observeEvent(input$id, {
    graph_data$selected_node <- input$id
  })
  
  
  observeEvent(graph_data$graph, {
    validate(need(graph_data$graph, ""))
    shiny.semantic::update_dropdown_input(
      session = session,
      input_id = "select_node",
      choices = c("", sort(names(igraph::V(graph_data$graph)))),
      value = ""
    )
  })

  
  observeEvent(graph_data$selected_node, {
    validate(need(graph_data$graph, ""))
    shiny.semantic::update_dropdown_input(
      session = session, 
      input_id = "select_node",
      choices = c("", sort(names(igraph::V(graph_data$graph)))),
      value = graph_data$selected_node
    )
  })

  
  observeEvent(input$reset_button, {
    graph_data$selected_node <- NULL
    graph_data$subgraph <- graph_data$graph
    shiny.semantic::update_dropdown_input(
      session = session,
      input_id = "select_node",
      choices = c("", sort(names(igraph::V(graph_data$graph)))),
      value = ""
    )
  })
  
  
  observeEvent(input$select_node, {
    req(input$select_node)
    graph_data$selected_node <- input$select_node
  })
  
  
  output$plot <- networkD3::renderForceNetwork({
    if (is.null(graph_data$graph)) return(NULL)
    
    graph_data$subgraph <- createSubgraph(
      graph_data$graph,
      graph_data$selected_node,
      input$select_order,
      input$select_mode
    )
    
    clickJS <- 'Shiny.setInputValue("graph_ui_1-id", d.name, {priority: "event"});'
    g_d3 <- networkD3::igraph_to_networkD3(graph_data$subgraph)
    g_d3$nodes$nodesize <- igraph::degree(graph_data$subgraph, mode = "all")
    if (!is.null(graph_data$selected_node)) {
      g_d3$nodes$group <- ifelse(g_d3$nodes$name == graph_data$selected_node, 1, 2)
    } else {
      g_d3$nodes$group <- 1
    }
    
    if (nrow(g_d3$links) > 0) {
      g_d3$links$value <- 1
    } else if (nrow(g_d3$links) == 0) {
      return()
    }
    
    networkD3::forceNetwork(
      Links = g_d3$links,
      Nodes = g_d3$nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      Nodesize = "nodesize",
      NodeID = "name",
      Group = "group",
      bounded = FALSE,
      zoom = TRUE,
      opacity = 0.8,
      arrows = TRUE,
      legend = FALSE,
      clickAction = clickJS
    )
  })
  
  
  output$selected_node_data <- renderUI({
    validate(need(graph_data$selected_node, "Select node to show details"))
    createNodeCard(graph_data$graph, graph_data$selected_node, loaded_data$functions())
  })
  
  
  output$distribution_plot <- renderPlot({
    if (is.null(graph_data$subgraph)) return(NULL)
    plotDistribution(graph_data$subgraph, mode = input$select_mode)
  })
}
    
## To be copied in the UI
# mod_graph_ui("graph_ui_1")
    
## To be copied in the server
# callModule(mod_graph_server, "graph_ui_1")
 
