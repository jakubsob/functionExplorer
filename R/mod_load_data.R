#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom magrittr %>% 
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        width = 3,
        div(
          class = "ui raised segment",
          style = "text-align: center;",
          div(class = "ui horizontal divider", "Download", style = "font-size: 12px;"),
          div(
            id = ns("github_input_div"),
            div(
              class = "ui vertical animated basic fade button",
              id = ns("github_input"),
              style = "width: 100%",
              div(
                class = "visible content",
                icon("github")
              ),
              div(
                class = "hidden content",
                "GitHub"
              )
            )
          ),
          div(class = "ui horizontal divider", "User defined functions", style = "font-size: 12px;"),
          div(
            id = ns("toggle"),
            style = "display: inline-block;",
            shiny.semantic::toggle(ns("user_defined"), "", TRUE)
          ),
          div(class = "ui horizontal divider", "Parse data", style = "font-size: 12px;"),
          shiny.semantic::action_button(
            ns("parse"),
            label = "",
            width = "100%",
            icon = icon("laptop code"),
            class = "basic"
          ),
          div(class = "ui horizontal divider", "Remove data", style = "font-size: 12px;"),
          shiny.semantic::actionButton(
            ns("reset"),
            label = "",
            width = "100%",
            icon = icon("window-close"),
            class = "basic"
          )
        )
      ),
      main_panel = shiny.semantic::main_panel(
        width = 10,
        uiOutput(ns("tables"))
      )
    )
  )
}
    
#' load_data Server Function
#'
#' @noRd 
mod_load_data_server <- function(input, output, session, data){
  ns <- session$ns
  
  # Session data:
  #   - active_tab: name of activetab
  #   - help: reactive, triggered by clicking help button
  active_tab <- session$userData$active_tab
  help <- session$userData$help
  
  gargoyle::init(ns("parsed"), ns("resetted"))
  
  observeEvent(help(), {
    # Trigger tutorial for this tab only when this tab is selected
    # Without this check, tutorial steps from tabs are messed up
    if (active_tab() != "load_tab") return()
    rintrojs::introjs(
      session, 
      options = list(steps = reactive(tutorial_mod_load_data(ns))())
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  
  create_download_modal(ns)  

  
  observeEvent(input$github_input, {
    shiny.semantic::show_modal(ns("github_modal"), session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  observeEvent(input$repository_download, {
    shiny.semantic::showNotification(
      "Downloading repository...", 
      id = ns("downloading_notif"),
      duration = 0, 
      type = "message",
      closeButton = FALSE,
      session = session
    )
    on.exit(shiny.semantic::removeNotification(ns("downloading_notif"), session))
    
    repo <- input$repository_name
    branch <- input$repository_branch
    result <- data$download_github(repo, branch)
    
    if (!result) {
      shiny.semantic::showNotification(
        sprintf("Error downloading from %s branch %s. Check if repository exists.", repo, branch),
        type = "error",
        session = session
      )
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  
  observeEvent(input$parse, {
    if (is.null(data$get_path())) {
      shiny.semantic::showNotification(
        "No downloaded data",
        type = "warning",
        duration = 5
      )
      return()
    }
    
    shiny.semantic::showNotification(
      "Parsing data",
      duration = 0,
      closeButton = FALSE,
      type = "message",
      id = ns("parse_notif"),
      session = session
    )
    
    data$find_functions()
    data$find_dependencies(input$user_defined)
    data$make_graph()
    
    gargoyle::trigger(ns("parsed"))
    
    shiny.semantic::removeNotification(
      ns("parse_notif"),
      session = session
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  observeEvent(input$reset, {
    data$reset()
    gargoyle::trigger(ns("resetted"))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  output$tables <- renderUI({
    gargoyle::watch(ns("parsed"))
    gargoyle::watch(ns("resetted"))
    
    if (is.null(data$get_functions())) {
      tagList(
        div(
          class = "ui placeholder segment",
          style = "height: 320px",
          div(
            class = "ui icon header",
            div(
              class = "ui icon header",
              icon("table")
            ),
            "No data uploaded"
          )
        )
      )
    } else {
      tagList(
        DT::DTOutput(
          ns("functions_table"),
          width = "100%"
        ) %>% 
          shinycssloaders::withSpinner(),
        DT::DTOutput(
          ns("dependencies_table"),
          width = "100%"
        ) %>% 
          shinycssloaders::withSpinner()
      )
    }
  })
  
  
  output$functions_table <- DT::renderDT({
    DT::datatable(
      data$get_functions(),
      options = list(scrollX = TRUE)
    )
  })
  
  
  output$dependencies_table <- DT::renderDT({
    DT::datatable(
      data$get_dependencies(),
      options = list(scrollX = TRUE)
    )
  })
}

#' Create download modal
#' 
#' @param ns Namespace 
#'
#' @noRd
create_download_modal <- function(ns) {
  shiny.semantic::create_modal(
    show = FALSE,
    shiny.semantic::modal(
      id = ns("github_modal"),
      class = "mini",
      header = div(
        style = "text-align: center;",
        h2("Download from GitHub")
      ),
      content = div(
        div(
          style = "text-align: center;",
          div(class = "ui horizontal divider", "Repository name", style = "font-size: 12px;"),
          shiny.semantic::textInput(
            ns("repository_name"),
            label = "",
            placeholder = "jakubsob/functionExplorer",
            width = "100%"
          )
        ),
        div(
          style = "text-align: center;",
          div(class = "ui horizontal divider", "Branch name", style = "font-size: 12px;"),
          shiny.semantic::textInput(
            ns("repository_branch"),
            label = "",
            placeholder = "master",
            width = "100%"
          )
        ),
        div(class = "ui horizontal divider", "Download", style = "font-size: 12px;"),
        div(
          style = "text-align: center;",
          shiny.semantic::action_button(
            ns("repository_download"),
            label = "",
            icon = icon("download"),
            width = "100%"
          )
        )
      )
    )
  )
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
