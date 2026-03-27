library(shiny)
library(DT)
library(dplyr)
library(tibble)

mod_reinsurance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Click a row to select it for editing or removal. Layers are stacked bottom-up; the first row is the retention/SIR. Attachment points are computed automatically."),
    DTOutput(ns("layers_table")),
    br(),
    fluidRow(
      # Edit panel
      column(4,
        wellPanel(
          h5("Edit selected layer"),
          uiOutput(ns("edit_selection_msg")),
          textInput(ns("edit_name"),  "Name",           value = ""),
          numericInput(ns("edit_limit"), "Limit (USD m)", value = 50, min = 0.01, step = 1),
          fluidRow(
            column(6, checkboxInput(ns("edit_is_ours"), "Our layer", value = FALSE)),
            column(6, checkboxInput(ns("edit_is_agg"),  "Aggregate basis", value = FALSE))
          ),
          fluidRow(
            column(6, actionButton(ns("save_edit"),       "Save",   class = "btn-primary btn-sm w-100")),
            column(6, actionButton(ns("remove_selected"), "Remove", class = "btn-danger  btn-sm w-100"))
          )
        )
      ),
      # Add panel
      column(4,
        wellPanel(
          h5("Add new layer"),
          textInput(ns("new_name"),  "Name",           value = "New layer"),
          numericInput(ns("new_limit"), "Limit (USD m)", value = 50, min = 0.01, step = 1),
          fluidRow(
            column(6, checkboxInput(ns("new_is_ours"), "Our layer",       value = FALSE)),
            column(6, checkboxInput(ns("new_is_agg"),  "Aggregate basis", value = FALSE))
          ),
          actionButton(ns("add_layer"), "Add layer", class = "btn-success btn-sm")
        )
      ),
      # Overview
      column(4,
        h5("Structure overview"),
        tableOutput(ns("structure_summary")),
        br(),
        helpText(
          strong("Occurrence basis:"), "each individual event is tested against the layer independently — multiple events can each penetrate.", tags$br(), tags$br(),
          strong("Aggregate basis:"), "the cumulative total loss for the period erodes this layer (stop-loss / aggregate XL). Attritional losses also contribute.", tags$br(), tags$br(),
          "Retention = row 1. Our layer = rows tagged 'Our layer'. Primary = everything else above retention."
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
      is_our_layer = c(FALSE, FALSE, TRUE),
      is_aggregate = c(FALSE, FALSE, FALSE)
    )

    layers_raw <- reactiveVal(default_layers)

    layers_computed <- reactive({
      df <- layers_raw()
      df %>%
        mutate(
          attachment_m = cumsum(c(0, head(limit_m, -1))),
          exhaustion_m = attachment_m + limit_m
        ) %>%
        select(name, attachment_m, limit_m, exhaustion_m, is_our_layer, is_aggregate)
    })

    # --- Table ---
    output$layers_table <- renderDT({
      df <- layers_computed() %>%
        mutate(
          Basis       = ifelse(is_aggregate, "Aggregate", "Occurrence"),
          `Our layer` = ifelse(is_our_layer, "Yes", "")
        ) %>%
        select(name, attachment_m, limit_m, exhaustion_m, Basis, `Our layer`) %>%
        rename(
          "Layer"              = name,
          "Attachment (USD m)" = attachment_m,
          "Limit (USD m)"      = limit_m,
          "Exhaustion (USD m)" = exhaustion_m
        )
      datatable(
        df,
        selection = "single",
        rownames  = FALSE,
        options   = list(dom = "t", scrollX = TRUE, autoWidth = TRUE, ordering = FALSE)
      ) %>%
        formatRound(c("Attachment (USD m)", "Limit (USD m)", "Exhaustion (USD m)"), digits = 1)
    })

    # --- Populate edit form when row is selected ---
    output$edit_selection_msg <- renderUI({
      i <- input$layers_table_rows_selected
      if (is.null(i) || length(i) == 0)
        tags$small(style = "color:#999;", em("Click a row in the table above to select it."))
    })

    observeEvent(input$layers_table_rows_selected, {
      i <- input$layers_table_rows_selected
      if (is.null(i) || length(i) == 0) return()
      df  <- layers_raw()
      row <- df[i, ]
      updateTextInput(   session, "edit_name",  value = row$name)
      updateNumericInput(session, "edit_limit", value = row$limit_m)
      updateCheckboxInput(session, "edit_is_ours", value = isTRUE(row$is_our_layer))
      updateCheckboxInput(session, "edit_is_agg",  value = isTRUE(row$is_aggregate))
    })

    # --- Save edits ---
    observeEvent(input$save_edit, {
      i <- input$layers_table_rows_selected
      req(length(i) > 0)
      req(nchar(trimws(input$edit_name)) > 0, input$edit_limit > 0)
      df            <- layers_raw()
      df$name[i]         <- trimws(input$edit_name)
      df$limit_m[i]      <- input$edit_limit
      df$is_our_layer[i] <- isTRUE(input$edit_is_ours)
      df$is_aggregate[i] <- isTRUE(input$edit_is_agg)
      layers_raw(df)
    })

    # --- Remove selected ---
    observeEvent(input$remove_selected, {
      i <- input$layers_table_rows_selected
      req(length(i) > 0)
      df <- layers_raw()
      if (nrow(df) > 1) layers_raw(df[-i, ])
    })

    # --- Add new layer ---
    observeEvent(input$add_layer, {
      req(nchar(trimws(input$new_name)) > 0, input$new_limit > 0)
      new_row <- tibble(
        name         = trimws(input$new_name),
        limit_m      = input$new_limit,
        is_our_layer = isTRUE(input$new_is_ours),
        is_aggregate = isTRUE(input$new_is_agg)
      )
      layers_raw(bind_rows(layers_raw(), new_row))
      updateTextInput(   session, "new_name",    value = "New layer")
      updateNumericInput(session, "new_limit",   value = 50)
      updateCheckboxInput(session, "new_is_ours", value = FALSE)
      updateCheckboxInput(session, "new_is_agg",  value = FALSE)
    })

    # --- Overview table ---
    output$structure_summary <- renderTable({
      df       <- layers_computed()
      our_rows <- df[df$is_our_layer, ]
      tibble(
        Metric = c(
          "No. layers",
          "Total capacity (USD m)",
          "Our layer attachment (USD m)",
          "Our layer total limit (USD m)",
          "Aggregate layers"
        ),
        Value = c(
          as.character(nrow(df)),
          format(round(sum(df$limit_m), 1), nsmall = 1),
          if (nrow(our_rows) > 0) format(round(min(our_rows$attachment_m), 1), nsmall = 1) else "—",
          if (nrow(our_rows) > 0) format(round(sum(our_rows$limit_m),      1), nsmall = 1) else "—",
          as.character(sum(df$is_aggregate))
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

    list(
      layers = layers_computed
    )
  })
}
