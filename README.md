# R3 ReE Research Network Dashboard

R Shiny dashboard for a fictional research department, modelled as a graph of
people, groups, skills, projects, companies and contacts.
Data description: `data/` (source of truth: `data/seed.json`).

## Setup

```r
install.packages("renv")
renv::restore()
```

## Run

```r
shiny::runApp()   # builds db/app.sqlite from data/seed.json on first start
```

Delete `db/app.sqlite` (or call `import_seed(con, overwrite = TRUE)`) to rebuild.

## Test

```r
testthat::test_dir("tests/testthat")
```

## Layout

| Path | Purpose |
|---|---|
| `app.R` | UI + server, loads data once |
| `R/db.R` | schema, `db_connect()`, `import_seed()`, `export_seed()` |
| `R/read.R` | `read_*()` tibbles, `read_all()` |
| `R/derive.R` | `projects_eff()`, `build_graph()`, `filter_graph()`, `group_percentage_total()` |
| `R/validate.R` | `validate_data()` -> `list(errors, warnings)` |
| `R/mod_*.R` | one Shiny module per tab (placeholders) |
