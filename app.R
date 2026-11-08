library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(visNetwork)

for (f in list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)) source(f)

# Load data once, outside `server`.
con <- db_connect()
if (!db_has_data(con)) import_seed(con)
tables <- read_all(con)
DBI::dbDisconnect(con)

# The reactive in `server` is called projects_eff (as in the diagram) and would
# shadow the function of the same name, so keep a handle on the function.
compute_projects_eff <- projects_eff

period_default <- range(c(tables$project$start_date, tables$project$end_date), na.rm = TRUE)
group_choices <- tables$research_group$abbreviation

ui <- page_navbar(
  title = "R3 ReE Research Network",
  sidebar = sidebar(
    dateInput("ref_date", "Reference date", value = Sys.Date()),
    checkboxInput("hide_rejected", "Hide rejected projects", value = TRUE),
    checkboxGroupInput("status_filter", "Project status", choices = STATUSES, selected = STATUSES),
    dateRangeInput("period", "Period", start = period_default[1], end = period_default[2]),
    selectInput("group", "Research group", choices = group_choices,
                selected = group_choices, multiple = TRUE)
  ),
  nav_panel("Network", mod_network_ui("network")),
  nav_panel("Projects", mod_projects_ui("projects")),
  nav_panel("People & skills", mod_people_ui("people")),
  nav_panel("KPIs & export", mod_kpis_ui("kpis"))
)

server <- function(input, output, session) {
  projects_eff <- reactive({
    req(input$ref_date)
    compute_projects_eff(tables$project, tables$funds_group, input$ref_date)
  })

  filtered_graph <- reactive({
    build_graph(tables, projects_eff()) |>
      filter_graph(projects_eff(), input$hide_rejected, input$status_filter, input$period,
                   input$group %||% character())
  })

  # Written by observers only (node clicks, table row selections).
  selected_node <- reactiveVal(NULL)
  # Plain handles on inputs, so modules get reactives instead of reading input.
  group <- reactive(input$group)
  period <- reactive(input$period)
  status_filter <- reactive(input$status_filter)

  mod_network_server("network", tables, projects_eff, filtered_graph, selected_node)
  mod_projects_server("projects", projects_eff, selected_node, group, period, status_filter)
  mod_people_server("people", projects_eff, group)
  mod_kpis_server("kpis", projects_eff, group)
}

shinyApp(ui, server)
