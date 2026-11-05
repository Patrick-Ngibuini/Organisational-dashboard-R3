# KPIs & export tab. Placeholder module.
# Will hold: kpi_cards, kpi_years, download_report (all Q11; need projects_eff, group, year_range).

mod_kpis_ui <- function(id) {
  ns <- NS(id)
  card(card_header("KPIs & export"), textOutput(ns("placeholder")))
}

mod_kpis_server <- function(id, projects_eff, group) {
  moduleServer(id, function(input, output, session) {
    # Q11 placeholder
    output$placeholder <- renderText({
      sprintf("Q11 · KPIs · coming soon (%d projects)", nrow(projects_eff()))
    })
  })
}
