test_that("seed imports with the expected counts", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  t <- read_all(con)
  n <- map_int(t, nrow)
  expect_equal(n[["research_group"]], 3)
  expect_equal(n[["skill"]], 16)
  expect_equal(n[["employee"]], 20)
  expect_equal(n[["company"]], 6)
  expect_equal(n[["company_department"]], 8)
  expect_equal(n[["external_contact"]], 15)
  expect_equal(n[["project"]], 10)
  expect_equal(n[c("member_of", "has_skill", "works_on", "funds_group",
                   "participates_in", "works_at", "part_of")],
               c(member_of = 27, has_skill = 45, works_on = 29, funds_group = 14,
                 participates_in = 22, works_at = 15, part_of = 8))
  counts <- table(t$project$status)
  expect_equal(as.list(counts)[c("running", "finished", "approved", "submitted", "rejected")],
               list(running = 5L, finished = 2L, approved = 1L, submitted = 1L, rejected = 1L))
})

test_that("column types are clean", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  t <- read_all(con)
  expect_s3_class(t$project$start_date, "Date")
  expect_true(is.ordered(t$has_skill$level))
  expect_type(t$company_department$key_goals, "list")
  expect_type(t$employee$active, "logical")
  expect_true(all(is.na(t$works_on$fte_percentage)))
  expect_equal(nrow(t$works_on), 29)
})

test_that("rejected project is kept without dates", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  p <- read_projects(con) |> filter(acronym == "SHELFVISION")
  expect_equal(p$status, "rejected")
  expect_true(is.na(p$start_date) && is.na(p$end_date))
})

test_that("import_seed(export_seed()) round-trips", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  path <- tempfile(fileext = ".json")
  export_seed(con, path)
  con2 <- new_db(path)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_equal(read_all(con2), read_all(con))
})

test_that("import refuses a non-empty database unless overwrite = TRUE", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(import_seed(con), "already has data")
  expect_no_error(import_seed(con, overwrite = TRUE))
  expect_equal(nrow(read_projects(con)), 10)
})

test_that("failed validation rolls the import back", {
  seed <- read_seed()
  i <- project_idx(seed, "CITYPULSE")
  seed$projects[[i]]$team <- c(seed$projects[[i]]$team,
                               list(list(employee = "an.claes@example.org", role = "responsible")))
  con <- db_connect(":memory:")
  on.exit(DBI::dbDisconnect(con))
  expect_error(import_seed(con, seed), "responsible")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM employee")$n, 0)
})

test_that("unknown skills are created and names are case-folded", {
  seed <- read_seed()
  seed$employees[[1]]$skills <- c(seed$employees[[1]]$skills,
                                  list(list(skill = "  Underwater Basketry ", level = "basic")),
                                  list(list(skill = "PYTHON", level = "basic")))
  seed$employees[[1]]$skills <- seed$employees[[1]]$skills[
    !duplicated(tolower(trimws(map_chr(seed$employees[[1]]$skills, "skill"))), fromLast = TRUE)]
  con <- new_db(seed)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(nrow(read_skills(con)), 17)
})

test_that("a company without departments gets 'General'", {
  seed <- read_seed()
  seed$companies <- c(seed$companies, list(list(name = "Empty BV", type = "private")))
  con <- new_db(seed)
  on.exit(DBI::dbDisconnect(con))
  d <- read_company_departments(con) |> filter(company == "Empty BV")
  expect_equal(d$name, "General")
})

test_that("delete rules from the schema", {
  con <- new_db()
  on.exit(DBI::dbDisconnect(con))
  expect_error(DBI::dbExecute(con, "DELETE FROM company WHERE name = 'Stad Kesselberg'"))
  expect_error(DBI::dbExecute(con, "DELETE FROM company_department WHERE name = 'Smart City Office'"))
  DBI::dbExecute(con, "DELETE FROM employee WHERE email = 'femke.jacobs@example.org'")
  expect_equal(nrow(filter(read_works_on(con), employee == "femke.jacobs@example.org")), 0)
})
