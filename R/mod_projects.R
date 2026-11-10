# Projects tab.
# Built: status_check() -> check_table (Q3), budget_split() -> budget_bar (Q6),
# timeline_data() -> timeline (Q4).

STATUS_COLOURS <- c(
  draft = "#B8B8B8", submitted = "#F2B134", approved = "#4C78A8",
  running = "#54A24B", finished = "#7F7F7F", rejected = "#E45756"
)

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
    nav_panel(
      "Timeline",
      p(class = "text-muted", "Dated projects; the project selected elsewhere is outlined."),
      plotly::plotlyOutput(ns("timeline"), height = "100%")
    )
  )
}

mod_projects_server <- function(id, tables, projects_eff, selected_node, group, period, status_filter,
                                hide_rejected) {
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
                         input$programme %||% character(), status_filter(), hide_rejected())
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

    # Dated projects for the chosen groups and period; marks the selected project.
    timeline_data <- reactive({
      timeline_table(projects_eff(), tables$funds_group, group() %||% character(),
                     period(), selected_node())
    })

    # Q4
    output$timeline <- plotly::renderPlotly({
      d <- timeline_data()
      validate(need(nrow(d) > 0, "No dated projects match the selected groups and period."))
      day_ms <- 86400000
      d <- d |>
        dplyr::mutate(
          acronym = factor(acronym, levels = rev(acronym)),  # earliest on top
          start_ms = as.numeric(start_date) * day_ms,
          length_ms = as.numeric(end_date - start_date) * day_ms,
          hover = paste0("<b>", acronym, "</b> · ", title,
                         "<br>", start_date, " to ", end_date,
                         "<br>status: ", eff_status,
                         ifelse(status_mismatch, paste0(" (", mismatch_reason, ")"), ""),
                         "<br>groups: ", dplyr::coalesce(groups, "none")),
          label = paste("ends", format(end_date, "%b %Y")),
          outline = ifelse(selected, "#000000", "rgba(0,0,0,0)"),
          outline_width = ifelse(selected, 3, 0)
        )
      plotly::plot_ly(d, type = "bar", orientation = "h", y = ~acronym, x = ~length_ms,
                      base = ~start_ms, color = ~eff_status, colors = STATUS_COLOURS,
                      text = ~label, textposition = "outside", cliponaxis = FALSE,
                      hovertext = ~hover, hoverinfo = "text",
                      marker = list(line = list(color = ~outline, width = ~outline_width))) |>
        plotly::layout(barmode = "overlay",
                       xaxis = list(type = "date", title = ""),
                       yaxis = list(title = ""),
                       legend = list(title = list(text = "Status")))
    })

    # Row click -> selected_node (state only, no output values computed here)
    observeEvent(input$check_table_rows_selected, {
      row <- input$check_table_rows_selected
      selected_node(paste0("proj:", status_check()$acronym[row]))
    })
  })
}
