# Network tab. Placeholder module.
# Will hold: contacts_table (Q1), network + ego_panel (Q2), sim_network (Q7),
# path_steps (Q9).

mod_network_ui <- function(id) {
  ns <- NS(id)
  card(card_header("Network"), textOutput(ns("placeholder")))
}

mod_network_server <- function(id, projects_eff, filtered_graph, selected_node) {
  moduleServer(id, function(input, output, session) {
    # Q2 placeholder
    output$placeholder <- renderText({
      sprintf("Q2 · network · coming soon (%d projects, %d nodes)",
              nrow(projects_eff()), nrow(filtered_graph()$nodes))
    })
  })
}
