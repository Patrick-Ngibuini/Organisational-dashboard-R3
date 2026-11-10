library(shiny)
library(bslib)
source(here::here("R", "mod_kpis.R"))

con <- new_db()
tables <- read_all(con)
DBI::dbDisconnect(con)
pe <- projects_eff(tables$project, tables$funds_group, as.Date("2026-10-18"))
kpi <- \(...) kpi_summary(pe, tables$funds_group, tables$member_of, tables$employee, ...)

test_that("kpi_summary totals over all groups and years", {
  k <- kpi()$cards
  expect_equal(k$projects, 10)
  expect_equal(k$running, sum(pe$eff_status == "running"))
  rejected <- pe$acronym[pe$status == "rejected"]
  expect_equal(k$budget, sum(tables$funds_group$budget[!tables$funds_group$project %in% rejected]))
  expect_equal(k$success_rate, 8 / 9)  # 8 approved/running/finished, 1 rejected
  expect_equal(k$researchers, 19)      # 19 of 20 employees are active
  active <- tables$employee$email[tables$employee$active]
  expect_equal(k$fte, sum(tables$member_of$percentage[tables$member_of$employee %in% active]) / 100)
})

test_that("kpi_summary follows group and year range", {
  dsai <- kpi(group = "DSAI")
  funded <- tables$funds_group$project[tables$funds_group$research_group == "DSAI"]
  expect_equal(dsai$cards$projects, length(funded))
  expect_setequal(dsai$projects$acronym, funded)
  expect_equal(unique(dsai$years$research_group), "DSAI")
  # budget is only DSAI's share of each project
  expect_equal(dsai$cards$budget,
               sum(tables$funds_group$budget[tables$funds_group$research_group == "DSAI" &
                                             !tables$funds_group$project %in% "SHELFVISION"]))

  yrs <- kpi(year_range = c(2024L, 2025L))
  expect_true(all(yrs$projects$year %in% 2024:2025))
  expect_equal(sort(unique(yrs$years$year)), 2024:2025)  # empty years are zero-filled

  expect_equal(kpi(group = character())$cards$projects, 0)
  expect_true(is.na(kpi(group = character())$cards$success_rate))
})

test_that("year budgets add up to the card budget", {
  k <- kpi()
  expect_equal(sum(k$years$budget), k$cards$budget)
})

test_that("kpi outputs render and the report downloads", {
  testServer(
    mod_kpis_server,
    args = list(tables = tables, projects_eff = reactive(pe), group = reactive(c("DSAI", "HCD", "SET"))),
    {
      session$setInputs(year_range = c(2020L, 2026L))
      expect_equal(kpis()$cards$projects, 10)
      expect_true(nchar(output$kpi_years) > 0)
      expect_true(nchar(as.character(output$kpi_cards$html)) > 0)
      expect_setequal(readxl::excel_sheets(output$download_report), c("KPIs", "Per year", "Projects"))
    }
  )
})
