incrementButton <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "inst/app/www/increment_button.js"))),
    tags$button(
      id = inputId,
      class = "increment btn btn-default",
      type = "button",
      as.character(value)
    )
  )
}