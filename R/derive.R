# Derived values. Nothing here is stored in the database.

#' Projects with effective status and derived columns
#'
#' The stored `status` is out of date for several projects, so an effective
#' status is derived from the dates relative to `ref_date`.
#'
#' Note: unlike the sketch in CLAUDE.md this also takes `funds_group`, because
#' `total_budget` is the sum of those edges.
#'
#' @param projects Tibble from [read_projects()].
#' @param funds_group Tibble from [read_funds_group()].
#' @param ref_date Reference date.
#' @return `projects` plus `eff_status`, `status_mismatch`, `mismatch_reason`,
#'   `total_budget`, `duration_months`, `approved`.
projects_eff <- function(projects, funds_group, ref_date = Sys.Date()) {
  ref_date <- as.Date(ref_date)
  budgets <- funds_group |>
    dplyr::summarise(total_budget = sum(budget), .by = project) |>
    dplyr::rename(acronym = project)

  projects |>
    dplyr::left_join(budgets, by = "acronym") |>
    dplyr::mutate(
      total_budget = dplyr::coalesce(total_budget, 0L),
      eff_status = dplyr::case_when(
        status %in% c("rejected", "draft", "submitted") ~ status,
        is.na(start_date) | is.na(end_date) ~ status,
        ref_date < start_date ~ "approved",
        ref_date <= end_date ~ "running",
        TRUE ~ "finished"
      ),
      status_mismatch = eff_status != status |
        (status %in% c("approved", "submitted") & !is.na(start_date) & start_date < ref_date),
      mismatch_reason = dplyr::case_when(
        !status_mismatch ~ NA_character_,
        eff_status == "finished" ~ paste0(status, ", but ended ", end_date),
        eff_status == "running" ~ paste0(status, ", but started ", start_date),
        eff_status == "approved" ~ paste0(status, ", but starts ", start_date),
        TRUE ~ paste0(status, ", but start date ", start_date, " has passed")
      ),
      duration_months = as.integer(round(
        lubridate::time_length(lubridate::interval(start_date, end_date), "month")
      )),
      approved = dplyr::case_when(
        status == "rejected" ~ 0L,
        status %in% c("approved", "running", "finished") ~ 1L,
        status == "submitted" ~ 2L,
        TRUE ~ NA_integer_
      )
    )
}

#' Projects whose stored status disagrees with their dates
#'
#' @param projects_eff Result of [projects_eff()].
#' @return The mismatching projects, earliest start first, with the columns the
#'   status-check table shows.
status_check_table <- function(projects_eff) {
  projects_eff |>
    dplyr::filter(status_mismatch) |>
    dplyr::arrange(start_date) |>
    dplyr::select(acronym, title, status, eff_status, mismatch_reason, start_date, end_date)
}

#' Budget per research group, one row per funds_group edge
#'
#' Projects are kept when their effective status is in `status_filter` and
#' their funding programme is in `programme`.
#'
#' @param projects_eff Result of [projects_eff()].
#' @param funds_group Tibble from [read_funds_group()].
#' @param programme Funding programmes to keep; `NULL` keeps all.
#' @param status_filter Effective statuses to keep.
#' @param hide_rejected Drop rejected projects.
#' @return `research_group`, `project`, `budget`, `funding_programme`,
#'   `eff_status`, ordered by group total (largest first).
budget_split_table <- function(projects_eff, funds_group, programme = NULL,
                               status_filter = STATUSES, hide_rejected = TRUE) {
  keep <- projects_eff |>
    dplyr::filter(eff_status %in% status_filter,
                  !(hide_rejected & eff_status == "rejected"),
                  is.null(programme) | funding_programme %in% programme) |>
    dplyr::select(project = acronym, funding_programme, eff_status)
  funds_group |>
    dplyr::inner_join(keep, by = "project") |>
    dplyr::mutate(group_total = sum(budget), .by = research_group) |>
    dplyr::arrange(dplyr::desc(group_total), research_group, dplyr::desc(budget)) |>
    dplyr::select(research_group, project, budget, funding_programme, eff_status)
}

#' One row per dated project, for the timeline
#'
#' Projects without start and end date (e.g. rejected proposals) cannot be drawn
#' and are left out.
#'
#' @param projects_eff Result of [projects_eff()].
#' @param funds_group Tibble from [read_funds_group()].
#' @param group Research groups to keep (projects they fund). `NULL`, or every
#'   funded group, means no group filter.
#' @param period Length-2 Date vector; keeps projects that overlap it.
#' @param selected_node Node id; its project gets `selected = TRUE`.
#' @return `acronym`, `title`, `status`, `eff_status`, `status_mismatch`,
#'   `mismatch_reason`, `start_date`, `end_date`, `groups` (text), `selected`,
#'   earliest start first.
timeline_table <- function(projects_eff, funds_group, group = NULL, period = NULL,
                           selected_node = NULL) {
  groups_txt <- funds_group |>
    dplyr::summarise(groups = paste(sort(research_group), collapse = ", "), .by = project)

  d <- projects_eff |>
    dplyr::filter(!is.na(start_date), !is.na(end_date)) |>
    dplyr::left_join(groups_txt, by = c("acronym" = "project"))

  if (!is.null(group) && !all(unique(funds_group$research_group) %in% group)) {
    funded <- funds_group$project[funds_group$research_group %in% group]
    d <- dplyr::filter(d, acronym %in% funded)
  }
  if (!is.null(period)) {
    d <- dplyr::filter(d, start_date <= period[2], end_date >= period[1])
  }
  d |>
    dplyr::arrange(start_date, acronym) |>
    dplyr::mutate(selected = paste0("proj:", acronym) %in% (selected_node %||% character())) |>
    dplyr::select(acronym, title, status, eff_status, status_mismatch, mismatch_reason,
                  start_date, end_date, groups, selected)
}

#' KPIs for the selected research groups and years
#'
#' A project counts in the year of its start date (application date when it has
#' no start date, e.g. rejected proposals). Its budget is the part funded by the
#' selected groups, and rejected projects add no budget.
#'
#' @param projects_eff Result of [projects_eff()].
#' @param funds_group,member_of,employees Tibbles from the `read_*()` functions.
#' @param group Research groups to include; `NULL` means all.
#' @param year_range Length-2 integer vector; `NULL` means all years.
#' @return `list(cards, years, projects)`:
#'   `cards` one row with `projects`, `running`, `budget`, `success_rate`
#'   (approved / (approved + rejected), `NA` without decisions), `researchers`
#'   (active members) and `fte` (sum of their group percentages / 100);
#'   `years` budget and project count per year and group;
#'   `projects` the projects behind the numbers.
kpi_summary <- function(projects_eff, funds_group, member_of, employees,
                        group = NULL, year_range = NULL) {
  groups <- group %||% union(funds_group$research_group, member_of$research_group)
  in_years <- \(year) {
    if (is.null(year_range)) !is.na(year) else !is.na(year) & year >= year_range[1] & year <= year_range[2]
  }

  pe <- projects_eff |>
    dplyr::mutate(year = lubridate::year(dplyr::coalesce(start_date, application_date)))
  funded <- funds_group |>
    dplyr::filter(research_group %in% groups) |>
    dplyr::inner_join(dplyr::select(pe, acronym, year, eff_status), by = c("project" = "acronym")) |>
    dplyr::filter(in_years(year))

  projects <- pe |>
    dplyr::filter(acronym %in% funded$project) |>
    dplyr::left_join(
      funded |> dplyr::summarise(budget = sum(budget), .by = project),
      by = c("acronym" = "project")
    ) |>
    dplyr::mutate(budget = ifelse(eff_status == "rejected", 0, budget)) |>
    dplyr::arrange(year, acronym) |>
    dplyr::select(acronym, title, year, status, eff_status, budget)

  active <- employees$email[employees$active]
  members <- dplyr::filter(member_of, research_group %in% groups, employee %in% active)
  approved <- sum(projects$status %in% c("approved", "running", "finished"))
  rejected <- sum(projects$status == "rejected")

  n_projects <- nrow(projects)
  n_running <- sum(projects$eff_status == "running")
  total_budget <- sum(projects$budget)
  cards <- dplyr::tibble(
    projects = n_projects,
    running = n_running,
    budget = total_budget,
    success_rate = if (approved + rejected > 0) approved / (approved + rejected) else NA_real_,
    researchers = dplyr::n_distinct(members$employee),
    fte = sum(members$percentage) / 100
  )

  years_seq <- if (is.null(year_range)) sort(unique(projects$year)) else seq(year_range[1], year_range[2])
  years <- funded |>
    dplyr::filter(eff_status != "rejected") |>
    dplyr::summarise(budget = sum(budget), projects = dplyr::n(), .by = c(year, research_group)) |>
    tidyr::complete(year = years_seq, research_group = sort(groups),
                    fill = list(budget = 0, projects = 0)) |>
    dplyr::arrange(year, research_group)

  list(cards = cards, years = years, projects = projects)
}

#' Total group membership per employee
#'
#' @param member_of Tibble from [read_member_of()].
#' @return One row per employee: `employee`, `total_percentage`, `over_100`.
group_percentage_total <- function(member_of) {
  member_of |>
    dplyr::summarise(total_percentage = sum(percentage), .by = employee) |>
    dplyr::mutate(over_100 = total_percentage > 100)
}

#' Build the node and edge tables for the network and igraph
#'
#' Node ids are prefixed: `emp:`, `grp:`, `skill:`, `proj:`, `comp:`,
#' `dept:<company>|<name>`, `con:`. The `group` column is a colouring hint:
#' primary research group for employees, best-funded group for projects,
#' category for skills, company type for companies, company name for
#' departments and contacts.
#'
#' @param tables Named list from [read_all()].
#' @param projects_eff Result of [projects_eff()].
#' @return `list(nodes, edges)`; edges carry `type`, `attr` (text), and for
#'   project edges the project's `status` and `eff_status`.
build_graph <- function(tables, projects_eff) {
  t <- tables
  dept_id <- \(company, name) paste0("dept:", company, "|", name)

  primary_group <- t$member_of |>
    dplyr::slice_max(percentage, by = employee, n = 1, with_ties = FALSE) |>
    dplyr::select(employee, group = research_group)
  top_funded <- t$funds_group |>
    dplyr::slice_max(budget, by = project, n = 1, with_ties = FALSE) |>
    dplyr::select(project, group = research_group)
  contact_company <- t$works_at |> dplyr::select(external_contact, group = company)

  nodes <- dplyr::bind_rows(
    t$employee |>
      dplyr::left_join(primary_group, by = c("email" = "employee")) |>
      dplyr::transmute(id = paste0("emp:", email), type = "employee",
                       label = paste(first_name, last_name), group),
    t$research_group |>
      dplyr::transmute(id = paste0("grp:", abbreviation), type = "group",
                       label = abbreviation, group = abbreviation),
    t$skill |>
      dplyr::transmute(id = paste0("skill:", tolower(name)), type = "skill",
                       label = name, group = category),
    t$project |>
      dplyr::left_join(top_funded, by = c("acronym" = "project")) |>
      dplyr::transmute(id = paste0("proj:", acronym), type = "project",
                       label = acronym, group),
    t$company |>
      dplyr::transmute(id = paste0("comp:", name), type = "company",
                       label = name, group = type),
    t$company_department |>
      dplyr::transmute(id = dept_id(company, name), type = "department",
                       label = name, group = company),
    t$external_contact |>
      dplyr::left_join(contact_company, by = c("email" = "external_contact")) |>
      dplyr::transmute(id = paste0("con:", email), type = "contact",
                       label = paste(first_name, last_name), group)
  )

  proj_status <- projects_eff |>
    dplyr::transmute(from_proj = paste0("proj:", acronym), status, eff_status)
  with_status <- \(edges, col) {
    edges |>
      dplyr::mutate(from_proj = .data[[col]]) |>
      dplyr::left_join(proj_status, by = "from_proj") |>
      dplyr::select(-from_proj)
  }

  edges <- dplyr::bind_rows(
    t$member_of |>
      dplyr::transmute(from = paste0("emp:", employee), to = paste0("grp:", research_group),
                       type = "member_of", attr = paste0(percentage, "%")),
    t$has_skill |>
      dplyr::transmute(from = paste0("emp:", employee), to = paste0("skill:", tolower(skill)),
                       type = "has_skill", attr = as.character(level)),
    t$works_on |>
      dplyr::transmute(from = paste0("emp:", employee), to = paste0("proj:", project),
                       type = "works_on", attr = role) |>
      with_status("to"),
    t$funds_group |>
      dplyr::transmute(from = paste0("proj:", project), to = paste0("grp:", research_group),
                       type = "funds_group", attr = as.character(budget)) |>
      with_status("from"),
    t$participates_in |>
      dplyr::transmute(from = paste0("con:", external_contact), to = paste0("proj:", project),
                       type = "participates_in", attr = role) |>
      with_status("to"),
    t$works_at |>
      dplyr::transmute(from = paste0("con:", external_contact), to = dept_id(company, department),
                       type = "works_at", attr = NA_character_),
    t$part_of |>
      dplyr::transmute(from = dept_id(company, department), to = paste0("comp:", company),
                       type = "part_of", attr = NA_character_)
  ) |>
    dplyr::select(from, to, type, attr, status, eff_status)

  list(nodes = nodes, edges = edges)
}

#' Restrict a graph to the projects selected in the sidebar
#'
#' Removes project nodes (and their edges) that are hidden, outside the
#' status filter, or do not overlap `period`. Projects without dates always
#' pass the period filter.
#'
#' @param graph Result of [build_graph()].
#' @param projects_eff Result of [projects_eff()].
#' @param hide_rejected Drop rejected projects.
#' @param status_filter Effective statuses to keep.
#' @param period Length-2 Date vector.
#' @param group Research group abbreviations to keep. `NULL` (or every group)
#'   means no group filter. Otherwise the graph keeps the selected groups, their
#'   members, the projects they fund, the skills of those members, the contacts
#'   on those projects and those contacts' departments and companies.
filter_graph <- function(graph, projects_eff, hide_rejected = TRUE,
                         status_filter = STATUSES, period = NULL, group = NULL) {
  keep <- projects_eff |>
    dplyr::filter(eff_status %in% (status_filter %||% character()))
  if (isTRUE(hide_rejected)) keep <- dplyr::filter(keep, eff_status != "rejected")
  if (!is.null(period)) {
    keep <- keep |>
      dplyr::filter(is.na(start_date) | is.na(end_date) |
                      (start_date <= period[2] & end_date >= period[1]))
  }
  dropped <- paste0("proj:", setdiff(projects_eff$acronym, keep$acronym))
  graph <- list(
    nodes = dplyr::filter(graph$nodes, !id %in% dropped),
    edges = dplyr::filter(graph$edges, !from %in% dropped, !to %in% dropped)
  )
  filter_graph_by_group(graph, group)
}

# Group filter: follow the edges outward from the selected groups.
filter_graph_by_group <- function(graph, group) {
  all_groups <- graph$nodes$id[graph$nodes$type == "group"]
  grp <- paste0("grp:", group)
  if (is.null(group) || all(all_groups %in% grp)) return(graph)

  e <- graph$edges
  ends_from <- \(type, from_ids) e$to[e$type == type & e$from %in% from_ids]
  emp <- e$from[e$type == "member_of" & e$to %in% grp]
  proj <- e$from[e$type == "funds_group" & e$to %in% grp]
  con <- e$from[e$type == "participates_in" & e$to %in% proj]
  dept <- ends_from("works_at", con)
  keep <- c(intersect(grp, all_groups), emp, proj, ends_from("has_skill", emp),
            con, dept, ends_from("part_of", dept))
  list(
    nodes = dplyr::filter(graph$nodes, id %in% keep),
    edges = dplyr::filter(graph$edges, from %in% keep, to %in% keep)
  )
}

#' Ego view: a node and its direct neighbours
#'
#' @param graph Result of [build_graph()] / [filter_graph()].
#' @param center Node id, or `NULL` for the whole graph.
#' @param order Number of steps away from `center` to keep.
#' @return `graph` restricted to nodes within `order` steps of `center`, and
#'   the edges between them. Unchanged if `center` is `NULL` or not present.
ego_graph <- function(graph, center, order = 1) {
  if (is.null(center) || !center %in% graph$nodes$id) return(graph)
  keep <- center
  for (i in seq_len(order)) {
    hit <- graph$edges$from %in% keep | graph$edges$to %in% keep
    keep <- union(keep, c(graph$edges$from[hit], graph$edges$to[hit]))
  }
  list(
    nodes = dplyr::filter(graph$nodes, id %in% keep),
    edges = dplyr::filter(graph$edges, from %in% keep, to %in% keep)
  )
}

#' Details of one node, for the side panel
#'
#' @param id Node id as made by [build_graph()].
#' @param tables Named list from [read_all()].
#' @param projects_eff Result of [projects_eff()].
#' @return Named character vector (field -> value), empty fields dropped;
#'   `NULL` for an unknown id.
node_details <- function(id, tables, projects_eff) {
  if (is.null(id)) return(NULL)
  prefix <- sub(":.*$", "", id)
  key <- sub("^[a-z]+:", "", id)
  row <- switch(prefix,
    emp = dplyr::filter(tables$employee, email == key),
    grp = dplyr::filter(tables$research_group, abbreviation == key),
    skill = dplyr::filter(tables$skill, tolower(name) == key),
    proj = projects_eff |>
      dplyr::filter(acronym == key) |>
      dplyr::select(acronym, title, status, eff_status, mismatch_reason,
                    funding_programme, total_budget, start_date, end_date,
                    duration_months, description),
    comp = dplyr::filter(tables$company, name == key),
    dept = tables$company_department |>
      dplyr::filter(paste0(company, "|", name) == key) |>
      dplyr::mutate(key_goals = purrr::map_chr(key_goals, \(g) paste(g, collapse = "; "))),
    con = dplyr::filter(tables$external_contact, email == key),
    NULL
  )
  if (is.null(row) || nrow(row) == 0) return(NULL)
  vals <- vapply(row[1, ], \(v) if (is.na(v[[1]])) "" else format(v[[1]]), character(1))
  vals[nzchar(vals)]
}
