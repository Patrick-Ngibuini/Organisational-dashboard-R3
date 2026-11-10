# KPIs & export tab.
# Built: kpis() -> kpi_cards, kpi_years, download_report (all Q11).

kpi_card <- function(title, value, icon_name) {
  value_box(title = title, value = value, showcase = bsicons::bs_icon(icon_name))
}

mod_kpis_ui <- function(id, years) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      sliderInput(ns("year_range"), "Years", min = min(years), max = max(years),
                  value = range(years), step = 1, sep = ""),
      downloadButton(ns("download_report"), "Download report (Excel)"),
      width = 260
    ),
    uiOutput(ns("kpi_cards")),
    # Fixed height: the cards above are not fillable, so a 100% height collapses.
    card(
      full_screen = TRUE,
      min_height = "480px",
      card_header("Budget per year and research group"),
      plotly::plotlyOutput(ns("kpi_years"), height = "420px")
    )
  )
}

mod_kpis_server <- function(id, tables, projects_eff, group) {
  moduleServer(id, function(input, output, session) {
    kpis <- reactive({
      kpi_summary(projects_eff(), tables$funds_group, tables$member_of, tables$employee,
                  group() %||% character(), input$year_range)
    })

    # Q11
    output$kpi_cards <- renderUI({
      k <- kpis()$cards
      eur <- \(x) paste("EUR", formatC(x, format = "d", big.mark = ","))
      layout_columns(
        fill = FALSE, col_widths = c(4, 4, 4),
        kpi_card("Projects", k$projects, "journal-text"),
        kpi_card("Running now", k$running, "play-circle"),
        kpi_card("Budget (excl. rejected)", eur(k$budget), "cash-coin"),
        kpi_card("Success rate",
                 if (is.na(k$success_rate)) "n/a" else sprintf("%.0f%%", 100 * k$success_rate),
                 "check2-circle"),
        kpi_card("Researchers", k$researchers, "people"),
        kpi_card("FTE", sprintf("%.1f", k$fte), "person-workspace")
      )
    })

    # Q11
    output$kpi_years <- plotly::renderPlotly({
      d <- kpis()$years
      validate(need(nrow(d) > 0, "No research groups selected."))
      d |>
        dplyr::mutate(hover = sprintf("%s %d<br>EUR %s<br>%d project(s)", research_group, year,
                                      formatC(budget, format = "d", big.mark = ","), projects)) |>
        plotly::plot_ly(x = ~year, y = ~budget, color = ~research_group, text = ~hover,
                        hoverinfo = "text", type = "bar") |>
        plotly::layout(barmode = "stack",
                       xaxis = list(title = "Year", dtick = 1),
                       yaxis = list(title = "Budget (EUR)"))
    })

    # Q11
    output$download_report <- downloadHandler(
      filename = \() paste0("ree_report_", Sys.Date(), ".xlsx"),
      content = function(file) {
        k <- kpis()
        writexl::write_xlsx(list(
          KPIs = tidyr::pivot_longer(k$cards, dplyr::everything(),
                                     names_to = "kpi", values_to = "value"),
          `Per year` = k$years,
          Projects = k$projects
        ), file)
      }
    )
  })
}
