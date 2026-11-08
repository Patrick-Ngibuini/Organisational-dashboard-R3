# Projects tab.
# Built: status_check() -> check_table (Q3).
# Still to come: timeline (Q4: projects_eff, group, period, selected_node),
# budget_bar (Q6: projects_eff, status_filter).

mod_projects_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Status check · projects whose status disagrees with their dates"),
      DT::DTOutput(ns("check_table"))
    ),
    card(card_header("Timeline and budget split"), p("Q4 · Q6 · coming soon"))
  )
}

mod_projects_server <- function(id, projects_eff, selected_node, group, period, status_filter) {
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

    # Row click -> selected_node (state only, no output values computed here)
    observeEvent(input$check_table_rows_selected, {
      row <- input$check_table_rows_selected
      selected_node(paste0("proj:", status_check()$acronym[row]))
    })
  })
}
