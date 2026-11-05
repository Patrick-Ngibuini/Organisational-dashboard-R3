# SQLite schema, connection, seed import and export.

STATUSES <- c("draft", "submitted", "approved", "running", "finished", "rejected")
COMPANY_TYPES <- c("vzw", "research_institute", "private", "government")
SKILL_LEVELS <- c("basic", "intermediate", "advanced", "expert")
WORKS_ON_ROLES <- c("responsible", "researcher", "support")
PARTICIPATES_ROLES <- c(
  "steering_committee", "funding_partner", "consortium_partner",
  "subcontractor", "advisory_board", "end_user", "other"
)

# Tables in dependency order (parents first); dropped in reverse.
DB_TABLES <- c(
  "research_group", "skill", "company", "company_department", "employee",
  "external_contact", "project", "member_of", "has_skill", "works_on",
  "funds_group", "participates_in", "works_at"
)

sql_in <- function(x) paste0("'", x, "'", collapse = ", ")

schema_statements <- function() {
  c(
    "CREATE TABLE IF NOT EXISTS research_group (
       abbreviation TEXT PRIMARY KEY,
       name TEXT NOT NULL,
       description TEXT)",
    "CREATE TABLE IF NOT EXISTS skill (
       name TEXT PRIMARY KEY,
       category TEXT,
       key TEXT NOT NULL UNIQUE)",
    sprintf("CREATE TABLE IF NOT EXISTS company (
       name TEXT PRIMARY KEY,
       type TEXT NOT NULL CHECK (type IN (%s)),
       description TEXT, website TEXT, city TEXT, country TEXT)",
      sql_in(COMPANY_TYPES)),
    # part_of (department -> company) is the company column; RESTRICT refuses
    # deleting a company that still has departments.
    "CREATE TABLE IF NOT EXISTS company_department (
       company TEXT NOT NULL REFERENCES company(name) ON DELETE RESTRICT,
       name TEXT NOT NULL,
       description TEXT,
       key_goals TEXT NOT NULL DEFAULT '[]',
       PRIMARY KEY (company, name))",
    "CREATE TABLE IF NOT EXISTS employee (
       email TEXT PRIMARY KEY,
       first_name TEXT NOT NULL,
       last_name TEXT NOT NULL,
       job_title TEXT, bio TEXT,
       active INTEGER NOT NULL DEFAULT 1)",
    "CREATE TABLE IF NOT EXISTS external_contact (
       email TEXT PRIMARY KEY,
       first_name TEXT NOT NULL,
       last_name TEXT NOT NULL,
       job_title TEXT, notes TEXT)",
    sprintf("CREATE TABLE IF NOT EXISTS project (
       acronym TEXT PRIMARY KEY,
       title TEXT NOT NULL,
       description TEXT, funding_programme TEXT,
       status TEXT NOT NULL CHECK (status IN (%s)),
       application_date TEXT, approval_date TEXT,
       start_date TEXT, end_date TEXT)",
      sql_in(STATUSES)),
    "CREATE TABLE IF NOT EXISTS member_of (
       employee TEXT NOT NULL REFERENCES employee(email) ON DELETE CASCADE,
       research_group TEXT NOT NULL REFERENCES research_group(abbreviation) ON DELETE CASCADE,
       percentage REAL NOT NULL CHECK (percentage BETWEEN 0 AND 100),
       PRIMARY KEY (employee, research_group))",
    sprintf("CREATE TABLE IF NOT EXISTS has_skill (
       employee TEXT NOT NULL REFERENCES employee(email) ON DELETE CASCADE,
       skill TEXT NOT NULL REFERENCES skill(name) ON DELETE CASCADE,
       level TEXT NOT NULL CHECK (level IN (%s)),
       PRIMARY KEY (employee, skill))",
      sql_in(SKILL_LEVELS)),
    sprintf("CREATE TABLE IF NOT EXISTS works_on (
       employee TEXT NOT NULL REFERENCES employee(email) ON DELETE CASCADE,
       project TEXT NOT NULL REFERENCES project(acronym) ON DELETE CASCADE,
       role TEXT NOT NULL CHECK (role IN (%s)),
       fte_percentage REAL,
       PRIMARY KEY (employee, project))",
      sql_in(WORKS_ON_ROLES)),
    "CREATE TABLE IF NOT EXISTS funds_group (
       project TEXT NOT NULL REFERENCES project(acronym) ON DELETE CASCADE,
       research_group TEXT NOT NULL REFERENCES research_group(abbreviation) ON DELETE CASCADE,
       budget INTEGER NOT NULL CHECK (budget >= 0),
       PRIMARY KEY (project, research_group))",
    sprintf("CREATE TABLE IF NOT EXISTS participates_in (
       external_contact TEXT NOT NULL REFERENCES external_contact(email) ON DELETE CASCADE,
       project TEXT NOT NULL REFERENCES project(acronym) ON DELETE CASCADE,
       role TEXT NOT NULL CHECK (role IN (%s)),
       PRIMARY KEY (external_contact, project))",
      sql_in(PARTICIPATES_ROLES)),
    # RESTRICT on the department: a department with contacts cannot be deleted.
    "CREATE TABLE IF NOT EXISTS works_at (
       external_contact TEXT NOT NULL REFERENCES external_contact(email) ON DELETE CASCADE,
       company TEXT NOT NULL,
       department TEXT NOT NULL,
       PRIMARY KEY (external_contact),
       FOREIGN KEY (company, department) REFERENCES company_department(company, name) ON DELETE RESTRICT)"
  )
}

#' Connect to the app database
#'
#' @param path SQLite file, or `":memory:"`. The directory is created if needed.
#' @return A DBI connection with foreign keys enforced.
db_connect <- function(path = here::here("db", "app.sqlite")) {
  if (path != ":memory:") dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

create_schema <- function(con) {
  purrr::walk(schema_statements(), \(s) DBI::dbExecute(con, s))
}

drop_schema <- function(con) {
  purrr::walk(rev(DB_TABLES), \(t) DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", t)))
}

#' TRUE when the database already holds rows
db_has_data <- function(con) {
  all(c("project", "employee") %in% DBI::dbListTables(con)) &&
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM project")$n > 0
}

# ---- seed parsing helpers --------------------------------------------------

pull_chr <- function(x, field) {
  purrr::map_chr(x, \(r) {
    v <- r[[field]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
  })
}

# Flatten a nested relation (e.g. employees[].groups[]) into a tibble with the
# parent key in `.parent` and one character column per child field.
nested_tbl <- function(records, parent, rel, fields) {
  template <- dplyr::as_tibble(
    purrr::set_names(rep(list(character()), length(fields) + 1), c(".parent", fields))
  )
  rows <- purrr::map(records, \(r) {
    kids <- r[[rel]]
    if (length(kids) == 0) return(NULL)
    kid_cols <- purrr::map(purrr::set_names(fields), \(f) pull_chr(kids, f))
    dplyr::as_tibble(c(list(.parent = rep(as.character(r[[parent]]), length(kids))), kid_cols))
  })
  dplyr::bind_rows(template, rows)
}

key_goals_json <- function(goals) {
  as.character(jsonlite::toJSON(as.character(unlist(goals)), auto_unbox = FALSE))
}

insert_seed <- function(con, seed) {
  append <- \(name, df) if (nrow(df) > 0) DBI::dbAppendTable(con, name, as.data.frame(df))

  # research groups
  groups <- seed$research_groups
  append("research_group", dplyr::tibble(
    abbreviation = pull_chr(groups, "abbreviation"),
    name = pull_chr(groups, "name"),
    description = pull_chr(groups, "description")
  ))

  # skills: trimmed + case-folded key; unknown names used by employees create a
  # new skill (rule 2). The first spelling seen is kept as display name.
  employees <- seed$employees
  emp_skills <- nested_tbl(employees, "email", "skills", c("skill", "level"))
  skills <- dplyr::bind_rows(
    dplyr::tibble(name = pull_chr(seed$skills, "name"), category = pull_chr(seed$skills, "category")),
    dplyr::tibble(name = emp_skills$skill, category = NA_character_)
  ) |>
    dplyr::mutate(name = trimws(name), key = tolower(name)) |>
    dplyr::filter(!is.na(name), name != "") |>
    dplyr::distinct(key, .keep_all = TRUE)
  append("skill", skills |> dplyr::select(name, category, key))

  # companies + departments; rule 3: a company without departments gets "General"
  companies <- seed$companies
  append("company", dplyr::tibble(
    name = pull_chr(companies, "name"),
    type = pull_chr(companies, "type"),
    description = pull_chr(companies, "description"),
    website = pull_chr(companies, "website"),
    city = pull_chr(companies, "city"),
    country = pull_chr(companies, "country")
  ))
  depts <- purrr::map(companies, \(co) {
    ds <- co$departments
    if (length(ds) == 0) ds <- list(list(name = "General", description = NULL, key_goals = list()))
    dplyr::tibble(
      company = co$name,
      name = pull_chr(ds, "name"),
      description = pull_chr(ds, "description"),
      key_goals = purrr::map_chr(ds, \(d) key_goals_json(d$key_goals))
    )
  }) |> dplyr::bind_rows()
  append("company_department", depts)

  # employees and their edges
  append("employee", dplyr::tibble(
    email = pull_chr(employees, "email"),
    first_name = pull_chr(employees, "first_name"),
    last_name = pull_chr(employees, "last_name"),
    job_title = pull_chr(employees, "job_title"),
    bio = pull_chr(employees, "bio"),
    active = purrr::map_int(employees, \(e) as.integer(e$active %||% TRUE))
  ))
  member_of <- nested_tbl(employees, "email", "groups", c("group", "percentage"))
  append("member_of", dplyr::tibble(
    employee = member_of$.parent, research_group = member_of$group,
    percentage = as.numeric(member_of$percentage)
  ))
  canonical <- purrr::set_names(skills$name, skills$key)
  append("has_skill", dplyr::tibble(
    employee = emp_skills$.parent,
    skill = unname(canonical[tolower(trimws(emp_skills$skill))]),
    level = emp_skills$level
  ))

  # external contacts (+ works_at)
  contacts <- seed$contacts
  append("external_contact", dplyr::tibble(
    email = pull_chr(contacts, "email"),
    first_name = pull_chr(contacts, "first_name"),
    last_name = pull_chr(contacts, "last_name"),
    job_title = pull_chr(contacts, "job_title"),
    notes = pull_chr(contacts, "notes")
  ))
  append("works_at", dplyr::tibble(
    external_contact = pull_chr(contacts, "email"),
    company = pull_chr(contacts, "company"),
    department = pull_chr(contacts, "department")
  ))

  # projects (rejected ones are kept) and their edges
  projects <- seed$projects
  append("project", dplyr::tibble(
    acronym = pull_chr(projects, "acronym"),
    title = pull_chr(projects, "title"),
    description = pull_chr(projects, "description"),
    funding_programme = pull_chr(projects, "funding_programme"),
    status = pull_chr(projects, "status"),
    application_date = pull_chr(projects, "application_date"),
    approval_date = pull_chr(projects, "approval_date"),
    start_date = pull_chr(projects, "start_date"),
    end_date = pull_chr(projects, "end_date")
  ))
  funds <- nested_tbl(projects, "acronym", "groups", c("group", "budget"))
  append("funds_group", dplyr::tibble(
    project = funds$.parent, research_group = funds$group,
    budget = as.integer(funds$budget)
  ))
  team <- nested_tbl(projects, "acronym", "team", c("employee", "role", "fte_percentage"))
  append("works_on", dplyr::tibble(
    employee = team$employee, project = team$.parent, role = team$role,
    fte_percentage = as.numeric(team$fte_percentage)
  ))
  part <- nested_tbl(projects, "acronym", "contacts", c("contact", "role"))
  append("participates_in", dplyr::tibble(
    external_contact = part$contact, project = part$.parent, role = part$role
  ))
}

#' Import a seed document into the database
#'
#' Runs in one transaction: on any validation error everything is rolled back.
#'
#' @param con DBI connection from [db_connect()].
#' @param seed Path to a seed JSON file, or an already parsed list.
#' @param overwrite Drop and rebuild all tables first.
#' @return Invisibly, the [validate_data()] result (errors is empty; check
#'   `warnings`).
import_seed <- function(con, seed = here::here("data", "seed.json"), overwrite = FALSE) {
  if (is.character(seed)) seed <- jsonlite::fromJSON(seed, simplifyVector = FALSE)
  if (overwrite) drop_schema(con)
  create_schema(con)
  if (db_has_data(con)) stop("Database already has data; use overwrite = TRUE.", call. = FALSE)

  result <- DBI::dbWithTransaction(con, {
    insert_seed(con, seed)
    res <- validate_data(read_all(con))
    if (length(res$errors) > 0) {
      stop("Seed validation failed:\n", paste0("- ", res$errors, collapse = "\n"), call. = FALSE)
    }
    res
  })
  invisible(result)
}

#' Export the database in the seed format
#'
#' @param con DBI connection.
#' @param path Optional file to write; the list is returned either way.
#' @return Invisibly, the seed document as a list.
export_seed <- function(con, path = NULL) {
  t <- read_all(con)
  records <- \(df) unname(purrr::pmap(df, list))
  rel <- \(df, id, col) df[df[[col]] == id, , drop = FALSE]

  employees <- purrr::map(seq_len(nrow(t$employee)), \(i) {
    e <- as.list(t$employee[i, ])
    c(e, list(
      groups = records(rel(t$member_of, e$email, "employee") |>
        dplyr::transmute(group = research_group, percentage)),
      skills = records(rel(t$has_skill, e$email, "employee") |>
        dplyr::transmute(skill, level = as.character(level)))
    ))
  })

  companies <- purrr::map(seq_len(nrow(t$company)), \(i) {
    co <- as.list(t$company[i, ])
    ds <- rel(t$company_department, co$name, "company")
    c(co, list(departments = purrr::map(seq_len(nrow(ds)), \(j) {
      list(name = ds$name[j], description = ds$description[j],
           key_goals = as.list(ds$key_goals[[j]]))
    })))
  })

  contacts <- t$external_contact |>
    dplyr::left_join(t$works_at, by = c("email" = "external_contact")) |>
    dplyr::relocate(company, department, .after = last_col()) |>
    records()

  projects <- purrr::map(seq_len(nrow(t$project)), \(i) {
    p <- t$project[i, ] |> dplyr::mutate(dplyr::across(dplyr::ends_with("_date"), as.character)) |> as.list()
    c(p, list(
      groups = records(rel(t$funds_group, p$acronym, "project") |>
        dplyr::transmute(group = research_group, budget)),
      team = records(rel(t$works_on, p$acronym, "project") |>
        dplyr::select(employee, role, fte_percentage)),
      contacts = records(rel(t$participates_in, p$acronym, "project") |>
        dplyr::transmute(contact = external_contact, role))
    ))
  })

  seed <- list(
    research_groups = records(t$research_group),
    skills = records(t$skill),
    employees = employees,
    companies = companies,
    contacts = contacts,
    projects = projects
  )
  if (!is.null(path)) {
    jsonlite::write_json(seed, path, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA)
  }
  invisible(seed)
}
