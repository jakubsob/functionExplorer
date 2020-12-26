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
        uiOutput(ns("repo_name")),
        conditionalPanel(
          condition = "output.data_initialized",
          ns = ns,
          div(class = "ui horizontal divider", "Functions", style = "font-size: 12px;"),
          DT::DTOutput(
            ns("functions_table"),
            width = "100%"
          ) %>%
            shinycssloaders::withSpinner(),
          div(class = "ui horizontal divider", "Dependencies", style = "font-size: 12px;"),
          DT::DTOutput(
            ns("dependencies_table"),
            width = "100%"
          ) %>%
            shinycssloaders::withSpinner()
        ),
        conditionalPanel(
          condition = "!output.data_initialized",
          ns = ns,
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
  
  # Init triggers
  gargoyle::init(ns("parsed"), ns("resetted"), ns("downloaded"))
  
  # Create reactive for conditional panel
  output$data_initialized <- reactive({
    gargoyle::watch(ns("parsed"))
    gargoyle::watch(ns("resetted"))
    data$is_initialized()
  })
  
  # Set to send value to hidden output
  outputOptions(output, "data_initialized", suspendWhenHidden = FALSE)  
  
  # Create downloading modal
  create_download_modal(ns)  
  
  
  # On click on `help` button trigger tutorial
  observeEvent(help(), {
    # Trigger tutorial for this tab only when this tab is selected
    # Without this check, tutorial steps from tabs are messed up
    if (active_tab() != "load_tab") return()
    rintrojs::introjs(
      session, 
      options = list(steps = reactive(tutorial_mod_load_data(ns))())
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  
  # On github button click show download modal
  observeEvent(input$github_input, {
    shiny.semantic::show_modal(ns("github_modal"), session = session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  # Cached data flag
  output$data_cached <- reactive({
    req(input$repository_name, input$repository_branch)
    dir.exists(data$create_path(input$repository_name, input$repository_branch))
  })
  
  # Set to send value to hidden output
  outputOptions(output, "data_cached", suspendWhenHidden = FALSE)  
  
  
  # On download button click download data
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
    
    if (result) {
      gargoyle::trigger(ns("downloaded"))
    }
    
    if (!result) {
      shiny.semantic::showNotification(
        glue::glue("Error downloading from {repo} branch {branch}. Check if repository exists."),
        type = "error",
        session = session
      )
    }
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  
  # On upload button click set path to cached data
  observeEvent(input$repository_load, {
    shiny.semantic::showNotification(
      "Loading repository from cache...", 
      id = ns("downloading_notif"),
      duration = 2, 
      type = "message",
      closeButton = TRUE,
      session = session
    )
    
    repo <- input$repository_name
    branch <- input$repository_branch
    data$set_path(data$create_path(repo, branch))
    gargoyle::trigger(ns("downloaded"))
  }, ignoreInit = TRUE)
  
  
  # On parse button click parse data
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
    shiny.semantic::removeNotification(ns("parse_notif"), session)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  # On reset button click reset data
  observeEvent(input$reset, {
    data$reset()
    output$repo_name <- renderUI(NULL)
    gargoyle::trigger(ns("resetted"))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  ### RENDER #######################################################################################
  
  # Render repository name
  output$repo_name <- renderUI({
    gargoyle::watch(ns("downloaded"))
    if (is.null(data$get_path())) return(NULL)
    h1(strsplit(basename(data$get_path()), "-")[[1]][1])
  })
  
  
  # Render data table with functions data
  output$functions_table <- DT::renderDT({
    # Watch `parsed` trigger to render when conditional panel changes
    gargoyle::watch(ns("parsed"))
    DT::datatable(
      data$get_functions(),
      options = list(
        scrollX = TRUE,
        autowidth = TRUE,
        dom = "lrt"
      ),
      class = "ui small compact table",
      rownames = FALSE,
      filter = list(position = "top", clear = TRUE)
    )
  })
  
  
  # Render data table with dependencies data
  output$dependencies_table <- DT::renderDT({
    # Watch `parsed` trigger to render when conditional panel changes
    gargoyle::watch(ns("parsed"))
    DT::datatable(
      data$get_dependencies(),
      options = list(
        scrollX = TRUE,
        autowidth = TRUE,
        dom = "lrt"
      ),
      class = "ui small compact table",
      rownames = FALSE,
      filter = list(position = "top", clear = TRUE)
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
            placeholder = const_github$default_repo,
            width = "100%"
          )
        ),
        div(
          style = "text-align: center;",
          div(class = "ui horizontal divider", "Branch name", style = "font-size: 12px;"),
          shiny.semantic::textInput(
            ns("repository_branch"),
            label = "",
            placeholder = const_github$default_branch,
            width = "100%"
          )
        ),
        div(class = "ui horizontal divider", "Download", style = "font-size: 12px;"),
        div(
          class = "ui buttons",
          style = "display: flex;",
          div(
            class = "ui vertical animated basic fade button left floated",
            id = ns("repository_download"),
            style = "flex: 1",
            div(
              class = "visible content",
              icon("download")
            ),
            div(
              class = "hidden content",
              "Download"
            )
          ),
          div(class = "or"),
          div(
            style = "flex: 1;",
            conditionalPanel(
              condition = "!output.data_cached",
              ns = ns,
              div(
                class = "ui disabled button",
                style = "width: 100%",
                icon("upload disc")
              )
            ),
            conditionalPanel(
              condition = "output.data_cached",
              ns = ns,
              div(
                class = "ui vertical animated basic fade button",
                style = "width: 100%",
                id = ns("repository_load"),
                div(
                  class = "visible content",
                  icon("upload disc")
                ),
                div(
                  class = "hidden content",
                  "Load cached"
                )
              )
           )
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
 
