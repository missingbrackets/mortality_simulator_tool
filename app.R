library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(openxlsx)
library(scales)

source("R/utils.R")
source("R/curve_fitting.R")
source("R/simulation_engine.R")
source("R/export.R")
source("R/mod_curve_fit.R")
source("R/mod_mortality_calc.R")
source("R/mod_reinsurance.R")
source("R/mod_sensitivity.R")

metric_card <- function(title, value) {
  card(
    card_header(title),
    card_body(style = "font-size: 1.6rem; font-weight: 600;", as.character(value))
  )
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("table.dataTable tbody td { white-space: nowrap; } .shiny-notification { width: 420px; }"))
  ),
  titlePanel("Contingency Film Production Risk Pricing Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Core assumptions"),
      dateInput("inception_date", "Inception date", value = "2026-04-01", format = "yyyy-mm-dd"),
      numericInput("exposure_years", "Exposure period (years)", value = 4, min = 0.25, step = 0.25),
      helpText("Number of productions and production duration are set in the Mortality Calculator tab."),

      tags$hr(),
      h4("Film budgets"),
      numericInput("total_budget_m", "Total budget across all films (USD m)", value = 3000, min = 1, step = 1),
      numericInput("min_budget_m", "Minimum individual film budget (USD m)", value = 20, min = 0.1, step = 0.1),
      numericInput("max_budget_m", "Maximum individual film budget (USD m)", value = 100, min = 0.1, step = 0.1),
      helpText("Budgets are randomly assigned between min and max, forced to sum to the total."),

      tags$hr(),
      h4("Frequency"),
      radioButtons(
        "freq_basis",
        "Frequency basis",
        choices  = c("Per year" = "per_year", "Per production" = "per_production"),
        selected = "per_year",
        inline   = TRUE
      ),
      uiOutput("freq_input_ui"),
      uiOutput("freq_info"),
      helpText("Use the Mortality Calculator tab to derive frequency from actor mortality assumptions."),

      tags$hr(),
      h4("Attritional"),
      numericInput("avg_attritional", "Average attritional ground-up loss per simulation (USD m)", value = 8, min = 0, step = 0.1),
      numericInput("attritional_cv", "Attritional coefficient of variation", value = 0.35, min = 0, step = 0.05),

      tags$hr(),
      h4("Severity input interpretation"),
      radioButtons(
        "severity_rp_type",
        "Return period supplied as",
        choices  = c("Years" = "years", "Claims" = "claims"),
        selected = "years",
        inline   = TRUE
      ),

      tags$hr(),
      h4("Simulation controls"),
      numericInput("n_sims", "Number of simulations", value = 5000, min = 100, step = 100),
      numericInput("seed", "Simulation seed", value = 12345, min = 1, step = 1),
      actionButton("run_model", "Run model", class = "btn-primary"),
      br(), br(),
      textOutput("run_status"),
      br(),
      downloadButton("download_excel", "Download simulation workbook")
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel(
          "Data inputs",
          br(),
          h4("Aggregate claims history (last 5 years)"),
          p("Optional; used for context and benchmarking only."),
          fileInput("claims_history_file", "Upload CSV", accept = c(".csv")),
          DTOutput("claims_history_preview"),
          br(),
          h4("Severity table"),
          p("Required columns: loss_m and one of return_period / return_period_years / return_period_claims"),
          fileInput("severity_file", "Upload severity CSV", accept = c(".csv")),
          uiOutput("severity_status"),
          DTOutput("severity_preview")
        ),
        tabPanel(
          "Curve fitting",
          br(),
          mod_curve_fit_ui("curve_fit")
        ),
        tabPanel(
          "Mortality calculator",
          br(),
          mod_mortality_calc_ui("mort_calc")
        ),
        tabPanel(
          "Reinsurance structure",
          br(),
          mod_reinsurance_ui("reinsurance")
        ),
        tabPanel(
          "Results",
          br(),
          fluidRow(
            column(4, uiOutput("vb_ground_up")),
            column(4, uiOutput("vb_primary")),
            column(4, uiOutput("vb_our_layer"))
          ),
          fluidRow(
            column(4, uiOutput("vb_avg_events")),
            column(4, uiOutput("vb_avg_sev")),
            column(4, uiOutput("vb_prob_hit"))
          ),
          br(),
          plotOutput("layer_dist_plot", height = "320px"),
          br(),
          DTOutput("summary_table"),
          br(),
          DTOutput("distribution_check_table")
        ),
        tabPanel(
          "Sensitivity",
          br(),
          mod_sensitivity_ui("sensitivity")
        ),
        tabPanel(
          "Simulation detail",
          br(),
          DTOutput("production_schedule_table"),
          br(),
          DTOutput("sim_summary_table"),
          br(),
          DTOutput("event_detail_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  claims_history_data <- reactiveVal(tibble(year = integer(), aggregate_loss_m = numeric()))
  mortality_data      <- reactiveVal(clean_mortality_table(default_mortality_table()))
  severity_raw_data   <- reactiveVal(default_severity_table())

  sim_results      <- reactiveVal(NULL)
  run_status_txt   <- reactiveVal("No model run yet.")
  severity_status_txt <- reactiveVal("Using default severity table.")

  # --- Modules (initialised before assumptions_reactive so reactives resolve lazily) ---
  mort_calc <- mod_mortality_calc_server(
    "mort_calc",
    exposure_years = reactive(input$exposure_years),
    mortality_data = mortality_data
  )

  observeEvent(mort_calc$use_freq_trigger(), {
    updateNumericInput(session, "annual_claim_freq", value = round(mort_calc$computed_freq(), 4))
    updateRadioButtons(session, "freq_basis", selected = "per_year")
  })

  reinsurance <- mod_reinsurance_server("reinsurance")

  # --- Dynamic frequency input (label changes with basis) ---
  output$freq_input_ui <- renderUI({
    label <- if (input$freq_basis == "per_production") {
      "Expected claims per production"
    } else {
      "Expected claims per year"
    }
    numericInput("annual_claim_freq", label, value = 0.12, min = 0, step = 0.01)
  })

  output$freq_info <- renderUI({
    req(input$annual_claim_freq)
    n_prods <- mort_calc$n_productions()
    if (input$freq_basis == "per_production") {
      total <- input$annual_claim_freq * n_prods
      tags$small(style = "color:#555;",
        sprintf("Total expected events: %.2f  (%d productions × %.4f)",
                total, n_prods, input$annual_claim_freq))
    } else {
      total <- input$annual_claim_freq * input$exposure_years
      tags$small(style = "color:#555;",
        sprintf("Total expected events over %.1f-year exposure: %.2f",
                input$exposure_years, total))
    }
  })

  # --- Core assumptions list ---
  assumptions_reactive <- reactive({
    req(input$annual_claim_freq)
    list(
      n_productions            = mort_calc$n_productions(),
      inception_date           = as.Date(input$inception_date),
      exposure_years           = input$exposure_years,
      production_duration_days = mort_calc$production_duration_days(),
      total_budget_m           = input$total_budget_m,
      min_budget_m             = input$min_budget_m,
      max_budget_m             = input$max_budget_m,
      avg_attritional_m        = input$avg_attritional,
      attritional_cv           = input$attritional_cv,
      annual_claim_freq        = input$annual_claim_freq,
      freq_basis               = input$freq_basis,
      n_sims                   = input$n_sims,
      seed                     = input$seed
    )
  })

  annual_claim_freq <- reactive(input$annual_claim_freq)

  severity_tbl <- reactive({
    standardize_severity_table(
      severity_tbl_raw       = severity_raw_data(),
      input_type             = input$severity_rp_type,
      annual_claim_frequency = annual_claim_freq()
    )
  })

  output$run_status     <- renderText(run_status_txt())
  output$severity_status <- renderUI(tags$small(style = "color:#555;", severity_status_txt()))

  # --- File uploads ---
  observeEvent(input$claims_history_file, {
    req(input$claims_history_file$datapath)
    tryCatch({
      claims_history_data(read_csv(input$claims_history_file$datapath, show_col_types = FALSE))
      showNotification("Claims history uploaded.", type = "message")
    }, error = function(e) {
      showNotification(paste("Claims history upload failed:", e$message), type = "error", duration = NULL)
    })
  })

  observeEvent(input$severity_file, {
    req(input$severity_file$datapath)
    tryCatch({
      raw_df   <- read_csv(input$severity_file$datapath, show_col_types = FALSE)
      clean_df <- clean_severity_table(raw_df)
      severity_raw_data(raw_df)
      severity_status_txt(sprintf("Uploaded severity table loaded: %s rows.", nrow(clean_df)))
      showNotification("Severity table uploaded.", type = "message")
    }, error = function(e) {
      severity_status_txt("Using previous/default severity table. Upload failed.")
      showNotification(paste("Severity upload failed:", e$message), type = "error", duration = NULL)
    })
  })

  # --- Data preview tables ---
  output$claims_history_preview <- renderDT({
    dt <- datatable(claims_history_data(), options = list(pageLength = 5, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    if ("aggregate_loss_m" %in% names(claims_history_data())) dt <- dt %>% formatRound("aggregate_loss_m", digits = 2)
    style_dt(dt)
  })

  output$severity_preview <- renderDT({
    datatable(
      severity_tbl() %>% select(loss_m, input_type, input_return_period,
                                return_period_years, return_period_claims,
                                exceed_prob_fit, percentile),
      options  = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(c("loss_m", "input_return_period", "return_period_years", "return_period_claims"), digits = 2) %>%
      formatRound(c("exceed_prob_fit", "percentile"), digits = 4)
  })

  # --- Curve fit module ---
  curve_fit <- mod_curve_fit_server("curve_fit", severity_tbl = severity_tbl)

  # --- Sensitivity module ---
  mod_sensitivity_server(
    "sensitivity",
    base_assumptions = assumptions_reactive,
    severity_tbl     = severity_tbl,
    selected_fit     = reactive(curve_fit$selected_fit()),
    layers           = reinsurance$layers
  )

  # --- Model run ---
  observeEvent(input$run_model, {
    req(nrow(severity_tbl()) >= 3)
    run_status_txt("Running model...")

    withProgress(message = "Running model", value = 0, {
      incProgress(0.10, detail = "Validating inputs")
      isolate({
        assign_production_budgets(
          n              = mort_calc$n_productions(),
          total_budget_m = input$total_budget_m,
          min_budget_m   = input$min_budget_m,
          max_budget_m   = input$max_budget_m
        )
      })

      incProgress(0.20, detail = "Capturing severity fit")
      fit_obj <- isolate(curve_fit$selected_fit())
      fit_tbl <- isolate(curve_fit$comparison_table())

      incProgress(0.55, detail = "Simulating portfolio")
      res <- simulate_portfolio_losses(
        assumptions    = assumptions_reactive(),
        severity_tbl   = severity_tbl(),
        fit            = fit_obj,
        layers         = reinsurance$layers(),
        claims_history = claims_history_data()
      )

      incProgress(0.10, detail = "Finalising outputs")
      res$severity_fit_comparison <- fit_tbl
      sim_results(res)
      run_status_txt(sprintf("Model run complete. %s simulations completed.", format(input$n_sims, big.mark = ",")))
      incProgress(0.05, detail = "Done")
    })
  })

  # --- Results outputs ---
  output$vb_ground_up <- renderUI({
    req(sim_results())
    metric_card("Expected ground-up loss", sprintf("$%.2fm", sim_results()$summary$expected_ground_up_m))
  })
  output$vb_primary <- renderUI({
    req(sim_results())
    metric_card("Expected primary loss", sprintf("$%.2fm", sim_results()$summary$expected_primary_loss_m))
  })
  output$vb_our_layer <- renderUI({
    req(sim_results())
    metric_card("Expected loss to our layer", sprintf("$%.2fm", sim_results()$summary$expected_our_layer_loss_m))
  })
  output$vb_avg_events <- renderUI({
    req(sim_results())
    metric_card("Average events", number(sim_results()$summary$avg_cast_events, accuracy = 0.001))
  })
  output$vb_avg_sev <- renderUI({
    req(sim_results())
    metric_card("Average event severity", sprintf("$%.2fm", sim_results()$summary$avg_event_severity_m))
  })
  output$vb_prob_hit <- renderUI({
    req(sim_results())
    metric_card("Probability our layer is hit", percent(sim_results()$summary$prob_our_layer_hit, accuracy = 0.01))
  })

  output$layer_dist_plot <- renderPlot({
    req(sim_results())
    ggplot(sim_results()$simulation_summary, aes(x = our_layer_loss_m)) +
      geom_histogram(bins = 50) +
      labs(title = "Distribution of loss to our layer", x = "Our layer loss (USD m)", y = "Simulation count") +
      theme_minimal(base_size = 13)
  })

  output$summary_table <- renderDT({
    req(sim_results())
    datatable(sim_results()$summary_table, options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
  })

  output$distribution_check_table <- renderDT({
    req(sim_results())
    datatable(sim_results()$severity_fit_comparison, options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE) %>%
      formatRound(c("loss_m", "return_period_years", "return_period_claims", "fitted_loss_at_input_percentile_m"), digits = 2) %>%
      formatRound(c("input_exceed_prob", "percentile", "fitted_percentile", "fitted_exceed_prob"), digits = 4) %>%
      formatPercentage("loss_error_pct", digits = 2)
  })

  output$production_schedule_table <- renderDT({
    req(sim_results())
    datatable(sim_results()$production_schedule, options = list(pageLength = 12, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE) %>%
      formatRound(c("exposure_days", "exposure_years", "budget_m"), digits = 2)
  })

  output$sim_summary_table <- renderDT({
    req(sim_results())
    dt <- datatable(sim_results()$simulation_summary, options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    numeric_cols <- names(sim_results()$simulation_summary)[vapply(sim_results()$simulation_summary, is.numeric, logical(1))]
    dt %>% formatRound(columns = numeric_cols, digits = 2)
  })

  output$event_detail_table <- renderDT({
    req(sim_results())
    dt <- datatable(sim_results()$event_detail, options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    numeric_cols <- names(sim_results()$event_detail)[vapply(sim_results()$event_detail, is.numeric, logical(1))]
    dt %>% formatRound(columns = numeric_cols, digits = 2)
  })

  output$download_excel <- downloadHandler(
    filename    = function() paste0("film_contingency_pricing_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(sim_results())
      fit_tbl <- tryCatch(isolate(curve_fit$comparison_table()), error = function(e) data.frame(note = e$message))
      export_simulation_workbook(results = sim_results(), fit_comparison = fit_tbl, path = file)
    }
  )
}

shinyApp(ui, server)
