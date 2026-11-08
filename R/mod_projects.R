# Projects tab.
# Built: status_check() -> check_table (Q3), budget_split() -> budget_bar (Q6).
# Still to come: timeline (Q4: projects_eff, group, period, selected_node).

mod_projects_ui <- function(id, programmes) {
  ns <- NS(id)
  # One view at a time, so each chart can use the full height of the window.
  navset_card_tab(
    height = "calc(100vh - 120px)",
    full_screen = TRUE,
    nav_panel(
      "Status check",
      p(class = "text-muted", "Projects whose status disagrees with their dates."),
      DT::DTOutput(ns("check_table"))
    ),
    nav_panel(
      "Budget split",
      selectInput(ns("programme"), "Funding programme", choices = programmes,
                  selected = programmes, multiple = TRUE),
      plotly::plotlyOutput(ns("budget_bar"), height = "100%")
    ),
    nav_panel("Timeline", p("Q4 · coming soon"))
  )
}

mod_projects_server <- function(id, tables, projects_eff, selected_node, group, period, status_filter) {
  moduleServer(id, function(input, output, session) {
    status_check <- reactive(status_check_table(projects_eff()))

    # Q3
    output$check_table <- DT::renderDT({
      status_check() |>
        dplyr::mutate(dplyr::across(c(start_date, end_date), as.character)) |>
        DT::datatable(
          selection = "single", rownames = FALSE,
          colnames = c("Acronym", "Title", "Status", "Effective status", "Reason", "Start", "End"),
          options = list(dom = "t", paging = FALSE)
        )
    })

    # Budget per group for the chosen programmes and statuses.
    budget_split <- reactive({
      budget_split_table(projects_eff(), tables$funds_group,
                         input$programme %||% character(), status_filter())
    })

    # Q6
    output$budget_bar <- plotly::renderPlotly({
      d <- budget_split()
      validate(need(nrow(d) > 0, "No projects match the selected programmes and statuses."))
      d |>
        dplyr::mutate(research_group = factor(research_group, levels = unique(research_group)),
                      hover = sprintf("%s<br>%s · %s<br>EUR %s", project, funding_programme,
                                      eff_status, formatC(budget, format = "d", big.mark = ","))) |>
        plotly::plot_ly(x = ~research_group, y = ~budget, color = ~project, text = ~hover,
                        hoverinfo = "text", type = "bar",
                        colors = grDevices::hcl.colors(max(3, dplyr::n_distinct(d$project)), "Dark 3")) |>
        plotly::layout(barmode = "stack",
                       xaxis = list(title = "Research group"),
                       yaxis = list(title = "Budget (EUR)"))
    })

    # Row click -> selected_node (state only, no output values computed here)
    observeEvent(input$check_table_rows_selected, {
      row <- input$check_table_rows_selected
      selected_node(paste0("proj:", status_check()$acronym[row]))
    })
  })
}
