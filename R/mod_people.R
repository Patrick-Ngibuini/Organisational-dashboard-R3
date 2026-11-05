# People & skills tab. Placeholder module.
# Will hold: skill_matrix (Q5), free_staff (Q8: projects_eff, group), skill_heatmap (Q10).

mod_people_ui <- function(id) {
  ns <- NS(id)
  card(card_header("People & skills"), textOutput(ns("placeholder")))
}

mod_people_server <- function(id, projects_eff, group) {
  moduleServer(id, function(input, output, session) {
    # Q5 placeholder
    output$placeholder <- renderText({
      sprintf("Q5 · skill matrix · coming soon (%d projects)", nrow(projects_eff()))
    })
  })
}
