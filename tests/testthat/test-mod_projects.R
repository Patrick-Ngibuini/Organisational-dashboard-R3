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
    args = list(projects_eff = reactive(pe), selected_node = sel, group = reactive("DSAI"),
                period = reactive(NULL), status_filter = reactive(STATUSES)),
    {
      session$setInputs(check_table_rows_selected = 2)
      expect_equal(sel(), paste0("proj:", status_check_table(pe)$acronym[2]))
    }
  )
})
