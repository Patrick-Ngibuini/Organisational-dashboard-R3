# Projects tab. Placeholder module.
# Will hold: check_table (Q3, writes selected_node), timeline (Q4: projects_eff,
# group, period, selected_node), budget_bar (Q6: projects_eff, status_filter).

mod_projects_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Projects"), textOutput(ns("placeholder")))
}

mod_projects_server <- function(id, projects_eff, selected_node, group, period, status_filter) {
  moduleServer(id, function(input, output, session) {
    # Q3 placeholder
    output$placeholder <- renderText({
      sprintf("Q3 · status check · coming soon (%d projects)", nrow(projects_eff()))
    })
  })
}
