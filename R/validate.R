# Business-rule validation. Rules 2, 3 and 7 are enforced at import / by the
# schema; the checks below cover the rest.

#' Validate the tables against the business rules
#'
#' @param tables Named list from [read_all()].
#' @return `list(errors = character(), warnings = character())`.
validate_data <- function(tables) {
  errors <- character()
  warnings <- character()

  # rule 1: at most one responsible person per project
  resp <- tables$works_on |>
    dplyr::filter(role == "responsible") |>
    dplyr::count(project) |>
    dplyr::filter(n > 1)
  errors <- c(errors, sprintf("Project %s has %d people with role 'responsible' (max 1).",
                              resp$project, resp$n))

  # rule 3: every company has at least one department
  no_dept <- setdiff(tables$company$name, tables$company_department$company)
  errors <- c(errors, sprintf("Company %s has no department.", no_dept))

  # rule 4: group membership above 100% is a warning
  over <- group_percentage_total(tables$member_of) |> dplyr::filter(over_100)
  warnings <- c(warnings, sprintf("Employee %s is member of groups for %s%% in total.",
                                  over$employee, over$total_percentage))

  # rule 5: date order
  bad_run <- tables$project |>
    dplyr::filter(!is.na(start_date), !is.na(end_date), end_date <= start_date)
  errors <- c(errors, sprintf("Project %s: end_date %s is not after start_date %s.",
                              bad_run$acronym, bad_run$end_date, bad_run$start_date))
  bad_appr <- tables$project |>
    dplyr::filter(!is.na(application_date), !is.na(approval_date), approval_date < application_date)
  errors <- c(errors, sprintf("Project %s: approval_date %s is before application_date %s.",
                              bad_appr$acronym, bad_appr$approval_date, bad_appr$application_date))

  list(errors = errors, warnings = warnings)
}
