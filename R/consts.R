#' @title Consts
#' @description  R6 class for storing settings
Consts <- R6::R6Class(
  "Consts",
  lock_objects = FALSE,
  public = list(
    #' @description Initialize using a JSON file. Content of JSON is assigned to
    #' members of class
    #' @param config Character, path to config file
    initialize = function(config) {
      if (tools::file_ext(config) != "json") {
        stop("Config file is not a json file;")
      }
      name <- basename(config)
      name <- gsub(".json$", "", name)
      data <- jsonlite::read_json(config)
      purrr::iwalk(data, ~ { self[[.y]] <- .x })
    }
  )
)
