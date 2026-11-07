# Network tab: the interactive graph (Q2).
# Click a node -> details panel (writes selected_node).
# Double-click a node -> ego view (that node + direct neighbours).
# Still to come here: contacts_table (Q1), sim_network (Q7), path_steps (Q9).

NODE_TYPES <- c("employee", "group", "skill", "project", "company", "department", "contact")

# One fixed colour and shape per node type, so the legend never changes.
NODE_STYLE <- tibble::tibble(
  type  = NODE_TYPES,
  color = c("#4C78A8", "#F58518", "#B8B8B8", "#54A24B", "#B279A2", "#D4A6C8", "#E45756"),
  shape = c("dot", "diamond", "triangle", "square", "star", "dot", "dot")
)

mod_network_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(9, 3),
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        textOutput(ns("view_label"), inline = TRUE),
        actionButton(ns("reset_ego"), "Show whole network", class = "btn-sm")
      ),
      checkboxGroupInput(ns("node_types"), NULL, choices = NODE_TYPES, inline = TRUE,
                         selected = setdiff(NODE_TYPES, "skill")),
      visNetwork::visNetworkOutput(ns("network"), height = "650px")
    ),
    card(card_header("Details"), uiOutput(ns("ego_panel")))
  )
}

#' @param tables Named list from [read_all()].
#' @param projects_eff,filtered_graph Reactives from the main server.
#' @param selected_node reactiveVal shared by all tabs.
mod_network_server <- function(id, tables, projects_eff, filtered_graph, selected_node) {
  moduleServer(id, function(input, output, session) {
    ego_center <- reactiveVal(NULL)

    # Sidebar filters -> node-type filter -> ego view.
    ego_graph_r <- reactive({
      g <- filtered_graph()
      g$nodes <- dplyr::filter(g$nodes, type %in% (input$node_types %||% character()))
      g$edges <- dplyr::filter(g$edges, from %in% g$nodes$id, to %in% g$nodes$id)
      ego_graph(g, ego_center())
    })

    output$view_label <- renderText({
      g <- ego_graph_r()
      centre <- if (is.null(ego_center())) "Whole network" else paste("Ego view:", ego_center())
      sprintf("%s (%d nodes, %d edges)", centre, nrow(g$nodes), nrow(g$edges))
    })

    output$network <- visNetwork::renderVisNetwork({
      g <- ego_graph_r()
      nodes <- g$nodes |>
        dplyr::left_join(NODE_STYLE, by = "type") |>
        dplyr::transmute(id, label, color, shape,
                         group = type,  # visNetwork uses `group` for the legend
                         title = paste0("<b>", label, "</b><br>", type),
                         size = ifelse(id == (ego_center() %||% ""), 30, 18))
      is_budget <- g$edges$type == "funds_group"
      attr_txt <- g$edges$attr
      attr_txt[is_budget] <- paste0("EUR ", formatC(as.numeric(attr_txt[is_budget]),
                                                    format = "d", big.mark = ","))
      edges <- g$edges |>
        dplyr::mutate(
          title = ifelse(is.na(attr_txt), type, paste0(type, ": ", attr_txt)),
          dashes = !is.na(eff_status) & eff_status %in% c("submitted", "draft", "rejected")
        ) |>
        dplyr::select(from, to, title, dashes)

      ns <- session$ns
      visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) |>
        visNetwork::visPhysics(solver = "forceAtlas2Based", stabilization = list(iterations = 200)) |>
        visNetwork::visLayout(randomSeed = 42) |>
        visNetwork::visEdges(color = list(color = "#BBBBBB", highlight = "#333333")) |>
        visNetwork::visInteraction(navigationButtons = TRUE) |>
        visNetwork::visEvents(
          click = sprintf(
            "function(p) { Shiny.setInputValue('%s', p.nodes.length ? p.nodes[0] : null, {priority: 'event'}); }",
            ns("network_click")),
          doubleClick = sprintf(
            "function(p) { if (p.nodes.length) Shiny.setInputValue('%s', p.nodes[0], {priority: 'event'}); }",
            ns("network_dblclick"))
        )
    })

    # observeEvent: store network click -> selected_node
    observeEvent(input$network_click, selected_node(input$network_click), ignoreNULL = FALSE)

    observeEvent(input$network_dblclick, {
      ego_center(input$network_dblclick)
      selected_node(input$network_dblclick)
    })
    observeEvent(input$reset_ego, ego_center(NULL))

    # observe: highlight on network (proxy), e.g. when a table row sets selected_node.
    observe({
      id <- selected_node()
      req(id, id %in% ego_graph_r()$nodes$id)
      visNetwork::visNetworkProxy(session$ns("network")) |>
        visNetwork::visSelectNodes(id = id)
    })

    output$ego_panel <- renderUI({
      id <- selected_node()
      if (is.null(id)) return(p(class = "text-muted",
        "Click a node for details. Double-click to show only its neighbourhood."))
      fields <- node_details(id, tables, projects_eff())
      if (is.null(fields)) return(p("No details for ", code(id)))

      g <- filtered_graph()
      links <- dplyr::filter(g$edges, from == id | to == id) |>
        dplyr::mutate(other = ifelse(from == id, to, from)) |>
        dplyr::left_join(dplyr::select(g$nodes, id, label), by = c("other" = "id"))

      tagList(
        h5(sub("^[a-z]+:", "", id)),
        tags$dl(purrr::imap(fields, \(v, k) tagList(tags$dt(k), tags$dd(v)))),
        h6(sprintf("Connections (%d)", nrow(links))),
        tags$ul(purrr::pmap(links, \(type, label, attr, ...) {
          tags$li(tags$b(label), " – ", type, if (!is.na(attr)) paste0(" (", attr, ")"))
        }))
      )
    })
  })
}
