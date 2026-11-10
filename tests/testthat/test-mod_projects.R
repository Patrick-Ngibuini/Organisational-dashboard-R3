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
                period = reactive(NULL), status_filter = reactive(STATUSES), hide_rejected = reactive(FALSE)),
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
                group = reactive("DSAI"), period = reactive(NULL), status_filter = status,
                hide_rejected = reactive(FALSE)),
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

test_that("timeline_table keeps dated projects and applies group, period and selection", {
  all <- timeline_table(pe, tables$funds_group)
  expect_equal(nrow(all), 9)  # SHELFVISION has no dates
  expect_false("SHELFVISION" %in% all$acronym)
  expect_false(is.unsorted(all$start_date))
  expect_false(any(all$selected))

  hcd <- timeline_table(pe, tables$funds_group, group = "HCD")
  funded <- tables$funds_group$project[tables$funds_group$research_group == "HCD"]
  expect_setequal(hcd$acronym, intersect(funded, all$acronym))
  expect_equal(nrow(timeline_table(pe, tables$funds_group, group = c("DSAI", "HCD", "SET"))), 9)
  expect_equal(nrow(timeline_table(pe, tables$funds_group, group = character())), 0)

  late <- timeline_table(pe, tables$funds_group, period = as.Date(c("2026-06-01", "2027-12-31")))
  expect_true(all(late$end_date >= as.Date("2026-06-01")))
  expect_lt(nrow(late), 9)

  sel <- timeline_table(pe, tables$funds_group, selected_node = "proj:XAICARE")
  expect_equal(sel$acronym[sel$selected], "XAICARE")
})

test_that("timeline renders and follows selected_node", {
  sel <- reactiveVal(NULL)
  testServer(
    mod_projects_server,
    args = list(tables = tables, projects_eff = reactive(pe), selected_node = sel,
                group = reactive(c("DSAI", "HCD", "SET")),
                period = reactive(as.Date(c("2000-01-01", "2100-12-31"))),
                status_filter = reactive(STATUSES), hide_rejected = reactive(FALSE)),
    {
      expect_equal(nrow(timeline_data()), 9)
      expect_true(nchar(output$timeline) > 0)
      sel("proj:CITYPULSE")
      expect_equal(timeline_data()$acronym[timeline_data()$selected], "CITYPULSE")
    }
  )
})

test_that("budget_split_table hides rejected projects by default", {
  shown <- budget_split_table(pe, tables$funds_group)
  expect_false("SHELFVISION" %in% shown$project)
  withr::with_options(list(), {
    all <- budget_split_table(pe, tables$funds_group, hide_rejected = FALSE)
    expect_true("SHELFVISION" %in% all$project)
  })
  vlaio <- budget_split_table(pe, tables$funds_group, "VLAIO TETRA")
  by_group <- vlaio |> summarise(b = sum(budget), .by = research_group)
  expect_equal(by_group$research_group, c("DSAI", "SET", "HCD"))  # sorted by total
  expect_equal(by_group$b, c(740000, 210000, 90000))
})
