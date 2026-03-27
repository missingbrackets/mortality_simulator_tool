library(shiny)
library(DT)
library(dplyr)
library(tibble)

mod_reinsurance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        8,
        h4("Layer stack"),
        p("Layers are stacked bottom-up. The first row is the retention/SIR. Attachment points are computed automatically from cumulative limits."),
        DTOutput(ns("layers_table")),
        br(),
        wellPanel(
          h5("Add layer"),
          fluidRow(
            column(4, textInput(ns("new_name"), "Layer name", value = "New layer")),
            column(3, numericInput(ns("new_limit"), "Limit (USD m)", value = 50, min = 0.01, step = 1)),
            column(3, checkboxInput(ns("new_is_ours"), "Tag as our layer", value = FALSE)),
            column(2, br(), actionButton(ns("add_layer"), "Add", class = "btn-success btn-sm"))
          ),
          actionButton(ns("remove_last"), "Remove last layer", class = "btn-warning btn-sm")
        )
      ),
      column(
        4,
        h4("Structure overview"),
        tableOutput(ns("structure_summary")),
        br(),
        helpText(
          "Retention = first layer (SIR).",
          tags$br(),
          "Our layer = layers tagged as 'our layer'.",
          tags$br(),
          "Primary = all other layers above retention.",
          tags$br(),
          "Losses exceeding the top of the stack are uninsured."
        )
      )
    )
  )
}

mod_reinsurance_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    default_layers <- tibble(
      name         = c("SIR (Retention)", "Primary market", "Our layer"),
      limit_m      = c(75, 100, 50),
      is_our_layer = c(FALSE, FALSE, TRUE)
    )

    layers_raw <- reactiveVal(default_layers)

    layers_computed <- reactive({
      df <- layers_raw()
      df %>%
        mutate(
          attachment_m = cumsum(c(0, head(limit_m, -1))),
          exhaustion_m = attachment_m + limit_m
        ) %>%
        select(name, attachment_m, limit_m, exhaustion_m, is_our_layer)
    })

    output$layers_table <- renderDT({
      df <- layers_computed() %>%
        rename(
          "Layer"              = name,
          "Attachment (USD m)" = attachment_m,
          "Limit (USD m)"      = limit_m,
          "Exhaustion (USD m)" = exhaustion_m,
          "Our layer?"         = is_our_layer
        )
      datatable(df, rownames = TRUE,
                options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE, ordering = FALSE)) %>%
        formatRound(c("Attachment (USD m)", "Limit (USD m)", "Exhaustion (USD m)"), digits = 1)
    })

    output$structure_summary <- renderTable({
      df <- layers_computed()
      our_rows <- df[df$is_our_layer, ]
      tibble(
        Metric = c(
          "No. layers",
          "Total capacity (USD m)",
          "Our layer attachment (USD m)",
          "Our layer total limit (USD m)"
        ),
        Value = c(
          as.character(nrow(df)),
          format(round(sum(df$limit_m), 1), nsmall = 1),
          if (nrow(our_rows) > 0) format(round(min(our_rows$attachment_m), 1), nsmall = 1) else "—",
          if (nrow(our_rows) > 0) format(round(sum(our_rows$limit_m), 1), nsmall = 1) else "—"
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

    observeEvent(input$add_layer, {
      req(nchar(trimws(input$new_name)) > 0, input$new_limit > 0)
      new_row <- tibble(
        name         = trimws(input$new_name),
        limit_m      = input$new_limit,
        is_our_layer = isTRUE(input$new_is_ours)
      )
      layers_raw(bind_rows(layers_raw(), new_row))
      updateTextInput(session, "new_name", value = "New layer")
      updateNumericInput(session, "new_limit", value = 50)
      updateCheckboxInput(session, "new_is_ours", value = FALSE)
    })

    observeEvent(input$remove_last, {
      df <- layers_raw()
      if (nrow(df) > 1) layers_raw(df[-nrow(df), ])
    })

    list(
      layers = layers_computed
    )
  })
}
