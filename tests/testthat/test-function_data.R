test_that("FunctionData class works", {
  data <- FunctionData$new()
  
  expect_is(data, "FunctionData")
  expect_is(data, "R6")
})

test_that("Downloading data", {
  data <- FunctionData$new()
  repo <- "WelcomeToMyVirtualHome/functionExplorer"
  branch <- "master"
  result <- data$download_github(repo, branch)
  
  expect_equal(data$get_path(), file.path(tempdir(), paste0(basename(repo), "-", branch)))
  expect_equal(result, TRUE)
})
