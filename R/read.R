# Read functions: one clean tibble per node table and per edge table.

read_table <- function(con, sql) dplyr::as_tibble(DBI::dbGetQuery(con, sql))

# ---- node tables -----------------------------------------------------------

read_research_groups <- function(con) {
  read_table(con, "SELECT abbreviation, name, description FROM research_group ORDER BY abbreviation")
}

read_skills <- function(con) {
  read_table(con, "SELECT name, category FROM skill ORDER BY key")
}

read_employees <- function(con) {
  read_table(con, "SELECT email, first_name, last_name, job_title, bio, active
                   FROM employee ORDER BY email") |>
    dplyr::mutate(active = as.logical(active))
}

read_companies <- function(con) {
  read_table(con, "SELECT name, type, description, website, city, country
                   FROM company ORDER BY name")
}

# key_goals comes back as a list column of character vectors
read_company_departments <- function(con) {
  read_table(con, "SELECT company, name, description, key_goals
                   FROM company_department ORDER BY company, name") |>
    dplyr::mutate(key_goals = purrr::map(key_goals, \(s) {
      as.character(unlist(jsonlite::fromJSON(s, simplifyVector = FALSE)))
    }))
}

read_external_contacts <- function(con) {
  read_table(con, "SELECT email, first_name, last_name, job_title, notes
                   FROM external_contact ORDER BY email")
}

read_projects <- function(con) {
  read_table(con, "SELECT acronym, title, description, funding_programme, status,
                          application_date, approval_date, start_date, end_date
                   FROM project ORDER BY acronym") |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_date"), as.Date))
}

# ---- edge tables -----------------------------------------------------------

read_member_of <- function(con) {
  read_table(con, "SELECT employee, research_group, percentage
                   FROM member_of ORDER BY employee, research_group")
}

read_has_skill <- function(con) {
  read_table(con, "SELECT employee, skill, level FROM has_skill ORDER BY employee, skill") |>
    dplyr::mutate(level = factor(level, levels = SKILL_LEVELS, ordered = TRUE))
}

read_works_on <- function(con) {
  read_table(con, "SELECT employee, project, role, fte_percentage
                   FROM works_on ORDER BY project, employee")
}

read_funds_group <- function(con) {
  read_table(con, "SELECT project, research_group, budget
                   FROM funds_group ORDER BY project, research_group")
}

read_participates_in <- function(con) {
  read_table(con, "SELECT external_contact, project, role
                   FROM participates_in ORDER BY project, external_contact")
}

read_works_at <- function(con) {
  read_table(con, "SELECT external_contact, company, department
                   FROM works_at ORDER BY external_contact")
}

# part_of (department -> company) lives in company_department.company
read_part_of <- function(con) {
  read_table(con, "SELECT company, name AS department FROM company_department
                   ORDER BY company, name")
}

#' Read every node and edge table
#'
#' @param con DBI connection.
#' @return Named list of tibbles, named after the schema tables
#'   (`project`, `member_of`, ...) plus `part_of`.
read_all <- function(con) {
  list(
    research_group = read_research_groups(con),
    skill = read_skills(con),
    employee = read_employees(con),
    company = read_companies(con),
    company_department = read_company_departments(con),
    external_contact = read_external_contacts(con),
    project = read_projects(con),
    member_of = read_member_of(con),
    has_skill = read_has_skill(con),
    works_on = read_works_on(con),
    funds_group = read_funds_group(con),
    participates_in = read_participates_in(con),
    works_at = read_works_at(con),
    part_of = read_part_of(con)
  )
}
