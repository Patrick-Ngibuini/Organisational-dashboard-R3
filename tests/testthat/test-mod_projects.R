library(shiny)
library(bslib)
source(here::here("R", "mod_projects.R"))

con <- new_db()
tables <- read_all(con)
DBI::dbDisconnect(con)
pe <- projects_eff(tables$project, tables$funds_group, as.Date("2026-10-18"))

test_that("status_check_table lists exactly the mismatching projects", {
  chk <- status_check_table(pe)
  expect_setequal(chk$acronym, pe$acronym[pe$status_mismatch])
  expect_equal(nrow(chk), 7)
  expect_false(is.unsorted(chk$start_date, na.rm = TRUE))
})

test_that("selecting a row in check_table writes selected_node", {
  sel <- reactiveVal(NULL)
  testServer(
    mod_projects_server,
    args = list(tables = tables, projects_eff = reactive(pe), selected_node = sel, group = reactive("DSAI"),
                period = reactive(NULL), status_filter = reactive(STATUSES)),
    {
      session$setInputs(check_table_rows_selected = 2)
      expect_equal(sel(), paste0("proj:", status_check_table(pe)$acronym[2]))
    }
  )
})

test_that("budget_split_table matches the fixed budget numbers", {
  ok <- c("approved", "running", "finished")
  d <- budget_split_table(pe, tables$funds_group, status_filter = ok)
  expect_equal(sum(d$budget[d$research_group == "DSAI"]), 755000)
  expect_equal(d$research_group[1], "DSAI")  # largest total first

  tetra <- budget_split_table(pe, tables$funds_group, "VLAIO TETRA", ok)
  by_group <- tetra |> summarise(b = sum(budget), .by = research_group) |> arrange(research_group)
  expect_equal(by_group$b, c(480000, 90000, 210000))  # DSAI, HCD, SET
  expect_true(all(tetra$funding_programme == "VLAIO TETRA"))

  with_submitted <- budget_split_table(pe, tables$funds_group, "VLAIO TETRA", c(ok, "submitted"))
  expect_equal(sum(with_submitted$budget[with_submitted$research_group == "DSAI"]), 740000)
  expect_equal(nrow(budget_split_table(pe, tables$funds_group, character(), ok)), 0)
})

test_that("budget_bar renders and follows the status filter", {
  status <- reactiveVal(STATUSES)
  testServer(
    mod_projects_server,
    args = list(tables = tables, projects_eff = reactive(pe), selected_node = reactiveVal(NULL),
                group = reactive("DSAI"), period = reactive(NULL), status_filter = status),
    {
      session$setInputs(programme = unique(tables$project$funding_programme))
      all_rows <- nrow(budget_split())
      expect_equal(all_rows, 14)
      expect_true(nchar(output$budget_bar) > 0)
      status("running")
      expect_lt(nrow(budget_split()), all_rows)
    }
  )
})
