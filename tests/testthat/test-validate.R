test_that("clean seed has no errors or warnings", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  res <- validate_data(read_all(con))
  expect_length(res$errors, 0)
  expect_length(res$warnings, 0)
})

test_that("two responsible people on one project is an error", {
  seed <- read_seed()
  i <- project_idx(seed, "CITYPULSE")
  seed$projects[[i]]$team <- c(seed$projects[[i]]$team,
                               list(list(employee = "an.claes@example.org", role = "responsible")))
  con <- db_connect(":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_error(import_seed(con, seed), "CITYPULSE")
})

test_that("end before start is an error", {
  seed <- read_seed()
  i <- project_idx(seed, "CITYPULSE")
  seed$projects[[i]]$end_date <- "2023-01-01"
  con <- db_connect(":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_error(import_seed(con, seed), "not after start_date")
})

test_that("approval before application is an error", {
  seed <- read_seed()
  i <- project_idx(seed, "CITYPULSE")
  seed$projects[[i]]$approval_date <- "2020-01-01"
  con <- db_connect(":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_error(import_seed(con, seed), "before application_date")
})

test_that("120 percent membership is a warning only", {
  seed <- read_seed()
  seed$employees[[1]]$groups <- list(list(group = "DSAI", percentage = 100),
                                     list(group = "HCD", percentage = 20))
  con <- db_connect(":memory:")
  on.exit(DBI::dbDisconnect(con))
  res <- import_seed(con, seed)
  expect_length(res$errors, 0)
  expect_length(res$warnings, 1)
  expect_match(res$warnings, "120")
})
