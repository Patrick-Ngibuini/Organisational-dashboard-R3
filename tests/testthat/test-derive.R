con <- new_db()
tables <- read_all(con)
DBI::dbDisconnect(con)
ref <- as.Date("2026-10-18")
pe <- projects_eff(tables$project, tables$funds_group, ref)

test_that("status mismatches at 2026-10-18", {
  expect_setequal(
    pe$acronym[pe$status_mismatch],
    c("CITYPULSE", "FRESHCAST", "SENSEHOME", "WARMWIJK", "XAICARE", "SENIORLINK", "AIAUDIT")
  )
  expect_true(all(!is.na(pe$mismatch_reason[pe$status_mismatch])))
})

test_that("SHELFVISION stays rejected without errors", {
  p <- filter(pe, acronym == "SHELFVISION")
  expect_equal(p$eff_status, "rejected")
  expect_true(is.na(p$duration_months))
  expect_equal(p$approved, 0L)
})

test_that("budgets are sums of funds_group edges", {
  expect_equal(pe$total_budget[pe$acronym == "XAICARE"], 290000)
  fg <- tables$funds_group |>
    inner_join(tables$project, by = c("project" = "acronym"))

  tetra <- fg |> filter(funding_programme == "VLAIO TETRA")
  by_group <- \(d) d |> summarise(b = sum(budget), .by = research_group) |> arrange(research_group)
  ok <- c("approved", "running", "finished")
  expect_equal(by_group(filter(tetra, status %in% ok))$b, c(480000, 90000, 210000))  # DSAI, HCD, SET
  expect_equal(sum(filter(tetra, research_group == "DSAI", status != "rejected")$budget), 740000)
  expect_equal(sum(filter(fg, research_group == "DSAI", status %in% ok)$budget), 755000)
})

test_that("approved codes", {
  expect_equal(pe$approved[pe$acronym == "AIAUDIT"], 2L)
  expect_true(all(pe$approved[pe$status %in% c("running", "finished")] == 1L))
})

test_that("nobody is above 100 percent", {
  expect_false(any(group_percentage_total(tables$member_of)$over_100))
})

test_that("build_graph returns a consistent node and edge table", {
  g <- build_graph(tables, pe)
  expect_equal(nrow(g$nodes), 3 + 16 + 20 + 10 + 6 + 8 + 15)
  expect_equal(nrow(g$edges), 27 + 45 + 29 + 14 + 22 + 15 + 8)
  expect_false(anyDuplicated(g$nodes$id) > 0)
  expect_true(all(c(g$edges$from, g$edges$to) %in% g$nodes$id))
  expect_setequal(sub(":.*", "", g$nodes$id), c("emp", "grp", "skill", "proj", "comp", "dept", "con"))
  proj_edges <- filter(g$edges, type %in% c("works_on", "funds_group", "participates_in"))
  expect_false(anyNA(proj_edges$eff_status))
})

test_that("filter_graph drops hidden projects and their edges", {
  g <- build_graph(tables, pe)
  f <- filter_graph(g, pe, hide_rejected = TRUE, status_filter = STATUSES)
  expect_false("proj:SHELFVISION" %in% f$nodes$id)
  expect_false(any(f$edges$from == "proj:SHELFVISION" | f$edges$to == "proj:SHELFVISION"))
  all <- filter_graph(g, pe, hide_rejected = FALSE, status_filter = STATUSES)
  expect_equal(nrow(all$nodes), nrow(g$nodes))
})

test_that("ego_graph keeps the centre and its direct neighbours only", {
  g <- list(
    nodes = tibble::tibble(id = c("a", "b", "c", "d")),
    edges = tibble::tibble(from = c("a", "b", "c"), to = c("b", "c", "d"))
  )
  ego <- ego_graph(g, "b")
  expect_setequal(ego$nodes$id, c("a", "b", "c"))
  expect_equal(nrow(ego$edges), 2)
  expect_setequal(ego_graph(g, "b", order = 2)$nodes$id, c("a", "b", "c", "d"))
  expect_identical(ego_graph(g, NULL), g)
  expect_identical(ego_graph(g, "zzz"), g)
})

test_that("group filter keeps the group's members, funded projects and their links", {
  g <- build_graph(tables, pe)
  f <- filter_graph(g, pe, hide_rejected = FALSE, status_filter = STATUSES, group = "HCD")
  expect_equal(filter(f$nodes, type == "group")$id, "grp:HCD")
  members <- tables$member_of$employee[tables$member_of$research_group == "HCD"]
  expect_setequal(filter(f$nodes, type == "employee")$id, paste0("emp:", members))
  funded <- tables$funds_group$project[tables$funds_group$research_group == "HCD"]
  expect_setequal(filter(f$nodes, type == "project")$id, paste0("proj:", funded))
  expect_true(all(c(f$edges$from, f$edges$to) %in% f$nodes$id))
  # no group filter when every group is selected, or when group is NULL
  expect_equal(nrow(filter_graph(g, pe, FALSE, STATUSES, group = tables$research_group$abbreviation)$nodes), nrow(g$nodes))
  expect_equal(nrow(filter_graph(g, pe, FALSE, STATUSES)$nodes), nrow(g$nodes))
  # nothing selected -> nothing group-related
  expect_equal(nrow(filter_graph(g, pe, FALSE, STATUSES, group = character())$nodes), 0)
})
