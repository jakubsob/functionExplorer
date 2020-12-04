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
            shiny.semantic::toggle(ns("user_defined"), "", FALSE)
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
mod_load_data_server <- function(input, output, session, help, tab){
  ns <- session$ns
  
  intro <- reactive(
    data.frame(
      element = paste0("#", ns(c("github_input_div", "toggle", "parse", "reset"))),
      intro = c(
        "Click to download repository for exploration",
        "If this toggle is on then only user defined functions will be parsed",
        "Click this button to run parser",
        "Click this button to delete uploaded files"
      )
    )
  )

  
  observeEvent(help(), {
    # Trigger tutorial for this tab only when this tab is selected
    # Without this check, tutorial steps from tabs are messed up
    if (tab() != "load_tab") return()
    rintrojs::introjs(session, options = list(steps = intro()))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  
  data <- reactiveValues(
    loaded_files = NULL,
    envir = NULL,
    functions = NULL,
    dependencies = NULL
  )

  
  observeEvent(input$reset, {
    data$loaded_files <- NULL
    data$envir <- NULL
    data$functions <- NULL
    data$dependencies <- NULL
    data$temp_dir <- NULL
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
  observeEvent(input$parse, {
    if (is.null(data$temp_dir)) return()
    shiny.semantic::showNotification(
      "Parsing data",
      duration = 0,
      closeButton = FALSE,
      type = "message",
      id = ns("parse_notif"),
      session = session
    )
    
    data$envir <- new.env()
    data$functions <- functiondepends::find_functions(
      data$temp_dir, 
      envir = data$envir, 
      recursive = TRUE
    ) %>%
      tidyr::unite("Path", tidyselect::starts_with("Level"), sep = "/")
    data$dependencies <- functiondepends::find_dependencies(
      unique(data$functions$Function),
      envir = data$envir,
      in_envir = input$user_defined,
      add_info = TRUE
    )
    
    shiny.semantic::removeNotification(
      ns("parse_notif"),
      session = session
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  
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
            placeholder = "WelcomeToMyVirtualHome/functionExplorer",
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
    repo <- input$repository_name
    branch <- input$repository_branch
    destfile <- file.path(tempdir(), sprintf("%s-%s.zip", basename(repo), branch))
    
    tryCatch({
      suppressWarnings(
        download.file(
          url = sprintf("https://github.com/%s/archive/%s.zip", repo, branch),
          destfile = destfile,
          quiet = TRUE
        )
      )
      unzip(destfile, exdir = tempdir(), overwrite = TRUE)
      unlink(destfile)
      data$temp_dir <- gsub(".zip$", "", destfile)
      },
      error = function(e) {
        shiny.semantic::showNotification(
          "Error downloading repository. Check if repository exists.",
          type = "error",
          session = session
        )
        NULL
      }
    )
    shiny.semantic::removeNotification(
      id = ns("downloading_notif"), 
      session = session
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

    
  output$tables <- renderUI({
    if (is.null(data$functions)) {
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
      data$functions,
      options = list(
        scrollX = TRUE
      )
    )
  })
  
  
  output$dependencies_table <- DT::renderDT({
    DT::datatable(
      data$dependencies,
      options = list(
        scrollX = TRUE
      )
    )
  })
 
  return(
    list(
      envir = reactive(data$envir),
      functions = reactive(data$functions),
      dependencies = reactive(data$dependencies)
    )
  ) 
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
