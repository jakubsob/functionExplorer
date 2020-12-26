test_that("FunctionData class works", {
  data <- FunctionData$new()
  
  expect_is(data, "FunctionData")
  expect_is(data, "R6")
})

test_that("Downloading data", {
  data <- FunctionData$new()
  repo <- "jakubsob/functionExplorer"
  branch <- "master"
  result <- data$download_github(repo, branch)
  
  expect_equal(data$get_path(), file.path(tempdir(), paste0(basename(repo), "-", branch)))
  expect_equal(result, TRUE)
})

test_that("Resetting data", {
  data <- FunctionData$new()
  repo <- "jakubsob/functionExplorer"
  branch <- "master"
  result <- data$download_github(repo, branch)
  data$reset()
  expect_equal(data$get_path(), NULL)
  expect_equal(ls(data$get_envir()), character(0))
  expect_equal(data$get_functions(), NULL)
  expect_equal(data$get_dependencies(), NULL)
  expect_equal(data$is_initialized(), FALSE)
})
