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
      numericInput("n_productions", "Number of productions", value = 65, min = 1, step = 1),
      dateInput("inception_date", "Inception date", value = "2026-04-01", format = "yyyy-mm-dd"),
      numericInput("exposure_years", "Exposure period (years)", value = 4, min = 0.25, step = 0.25),
      numericInput("production_duration_days", "Average production duration (days)", value = 180, min = 1, step = 1),

      tags$hr(),
      h4("Film budgets"),
      numericInput("total_budget_m", "Total budget across all films (USD m)", value = 3000, min = 1, step = 1),
      numericInput("min_budget_m", "Minimum individual film budget (USD m)", value = 20, min = 0.1, step = 0.1),
      numericInput("max_budget_m", "Maximum individual film budget (USD m)", value = 100, min = 0.1, step = 0.1),
      helpText("Budgets are randomly assigned between min and max, and forced to sum to the total portfolio budget."),

      tags$hr(),
      h4("Layer structure"),
      numericInput("sir", "SIR attachment (USD m)", value = 75, min = 0, step = 1),
      numericInput("primary_limit", "Primary limit (USD m)", value = 100, min = 0, step = 1),
      numericInput("our_limit", "Our layer limit (USD m)", value = 50, min = 0, step = 1),

      tags$hr(),
      h4("Attritional / cast assumptions"),
      numericInput("avg_attritional", "Average attritional ground-up loss per simulation (USD m)", value = 8, min = 0, step = 0.1),
      numericInput("attritional_cv", "Attritional coefficient of variation", value = 0.35, min = 0, step = 0.05),
      numericInput("avg_key_actors", "Average number of key actors per production", value = 4, min = 1, step = 1),
      numericInput("key_actor_age", "Assumed key actor age", value = 45, min = 0, max = 110, step = 1),
      numericInput("injury_load", "Injury rate multiple to mortality", value = 1.5, min = 0, step = 0.1),

      tags$hr(),
      h4("Severity input interpretation"),
      radioButtons(
        "severity_rp_type",
        "Return period supplied as",
        choices = c("Years" = "years", "Claims" = "claims"),
        selected = "years",
        inline = TRUE
      ),
      uiOutput("annual_freq_info"),

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
          p("Optional; used for context and benchmarking only in this version."),
          fileInput("claims_history_file", "Upload CSV", accept = c(".csv")),
          DTOutput("claims_history_preview"),
          br(),
          fluidRow(
            column(
              6,
              h4("Mortality table"),
              p("Required columns: Birth Year, Age, qx, lx"),
              fileInput("mortality_file", "Upload mortality CSV", accept = c(".csv")),
              uiOutput("mortality_status"),
              DTOutput("mortality_preview")
            ),
            column(
              6,
              h4("Cast death/injury severity table"),
              p("Required columns: loss_m and one of return_period / return_period_years / return_period_claims"),
              fileInput("severity_file", "Upload severity CSV", accept = c(".csv")),
              uiOutput("severity_status"),
              DTOutput("severity_preview")
            )
          )
        ),
        tabPanel(
          "Curve fitting",
          br(),
          mod_curve_fit_ui("curve_fit")
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
  mortality_data <- reactiveVal(clean_mortality_table(default_mortality_table()))
  severity_raw_data <- reactiveVal(default_severity_table())

  sim_results <- reactiveVal(NULL)
  run_status_txt <- reactiveVal("No model run yet.")
  mortality_status_txt <- reactiveVal("Using default mortality table.")
  severity_status_txt <- reactiveVal("Using default severity table.")

  assumptions_reactive <- reactive({
    list(
      n_productions = input$n_productions,
      inception_date = as.Date(input$inception_date),
      exposure_years = input$exposure_years,
      production_duration_days = input$production_duration_days,
      total_budget_m = input$total_budget_m,
      min_budget_m = input$min_budget_m,
      max_budget_m = input$max_budget_m,
      sir_m = input$sir,
      primary_limit_m = input$primary_limit,
      our_limit_m = input$our_limit,
      avg_attritional_m = input$avg_attritional,
      attritional_cv = input$attritional_cv,
      avg_key_actors = input$avg_key_actors,
      key_actor_age = input$key_actor_age,
      injury_load = input$injury_load,
      n_sims = input$n_sims,
      seed = input$seed
    )
  })

  annual_claim_freq <- reactive({
    expected_annual_claim_frequency(assumptions_reactive(), mortality_data())
  })

  severity_tbl <- reactive({
    standardize_severity_table(
      severity_tbl_raw = severity_raw_data(),
      input_type = input$severity_rp_type,
      annual_claim_frequency = annual_claim_freq()
    )
  })

  output$run_status <- renderText(run_status_txt())
  output$mortality_status <- renderUI(tags$small(style = "color:#555;", mortality_status_txt()))
  output$severity_status <- renderUI(tags$small(style = "color:#555;", severity_status_txt()))
  output$annual_freq_info <- renderUI({
    tags$small(
      style = "color:#555;",
      sprintf(
        "Expected annual cast-event frequency used for year-to-claim conversion: %.4f claims/year.",
        annual_claim_freq()
      )
    )
  })

  observeEvent(input$claims_history_file, {
    req(input$claims_history_file$datapath)
    tryCatch({
      claims_history_data(read_csv(input$claims_history_file$datapath, show_col_types = FALSE))
      showNotification("Claims history uploaded.", type = "message")
    }, error = function(e) {
      showNotification(paste("Claims history upload failed:", e$message), type = "error", duration = NULL)
    })
  })

  observeEvent(input$mortality_file, {
    req(input$mortality_file$datapath)
    tryCatch({
      clean_df <- clean_mortality_table(read_csv(input$mortality_file$datapath, show_col_types = FALSE))
      mortality_data(clean_df)
      mortality_status_txt(sprintf("Uploaded mortality table loaded: %s rows.", nrow(clean_df)))
      showNotification("Mortality table uploaded.", type = "message")
    }, error = function(e) {
      mortality_status_txt("Using previous/default mortality table. Upload failed.")
      showNotification(paste("Mortality upload failed:", e$message), type = "error", duration = NULL)
    })
  })

  observeEvent(input$severity_file, {
    req(input$severity_file$datapath)
    tryCatch({
      raw_df <- read_csv(input$severity_file$datapath, show_col_types = FALSE)
      clean_df <- clean_severity_table(raw_df)
      severity_raw_data(raw_df)
      severity_status_txt(sprintf("Uploaded severity table loaded: %s rows.", nrow(clean_df)))
      showNotification("Severity table uploaded.", type = "message")
    }, error = function(e) {
      severity_status_txt("Using previous/default severity table. Upload failed.")
      showNotification(paste("Severity upload failed:", e$message), type = "error", duration = NULL)
    })
  })

  output$claims_history_preview <- renderDT({
    dt <- datatable(claims_history_data(), options = list(pageLength = 5, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    if ("aggregate_loss_m" %in% names(claims_history_data())) dt <- dt %>% formatRound("aggregate_loss_m", digits = 2)
    style_dt(dt)
  })

  output$mortality_preview <- renderDT({
    datatable(mortality_data(), options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE) %>%
      formatRound("qx", digits = 6) %>%
      formatRound("lx", digits = 0)
  })

  output$severity_preview <- renderDT({
    datatable(
      severity_tbl() %>% select(loss_m, input_type, input_return_period, return_period_years, return_period_claims, exceed_prob_fit, percentile),
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(c("loss_m", "input_return_period", "return_period_years", "return_period_claims"), digits = 2) %>%
      formatRound(c("exceed_prob_fit", "percentile"), digits = 4)
  })

  curve_fit <- mod_curve_fit_server("curve_fit", severity_tbl = severity_tbl)

  observeEvent(input$run_model, {
    req(nrow(mortality_data()) > 0)
    req(nrow(severity_tbl()) >= 3)
    run_status_txt("Running model...")

    withProgress(message = "Running model", value = 0, {
      incProgress(0.10, detail = "Validating inputs")
      isolate({
        assign_production_budgets(
          n = input$n_productions,
          total_budget_m = input$total_budget_m,
          min_budget_m = input$min_budget_m,
          max_budget_m = input$max_budget_m
        )
      })

      incProgress(0.20, detail = "Capturing severity fit")
      fit_obj <- isolate(curve_fit$selected_fit())
      fit_tbl <- isolate(curve_fit$comparison_table())

      incProgress(0.55, detail = "Simulating portfolio")
      res <- simulate_portfolio_losses(
        assumptions = assumptions_reactive(),
        mortality_tbl = mortality_data(),
        severity_tbl = severity_tbl(),
        fit = fit_obj,
        claims_history = claims_history_data()
      )

      incProgress(0.10, detail = "Finalising outputs")
      res$severity_fit_comparison <- fit_tbl
      sim_results(res)
      run_status_txt(sprintf("Model run complete. %s simulations completed.", format(input$n_sims, big.mark = ",")))
      incProgress(0.05, detail = "Done")
    })
  })

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
    metric_card("Average cast death/injury events", number(sim_results()$summary$avg_cast_events, accuracy = 0.001))
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
    filename = function() paste0("film_contingency_pricing_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      req(sim_results())
      fit_tbl <- tryCatch(isolate(curve_fit$comparison_table()), error = function(e) data.frame(note = e$message))
      export_simulation_workbook(results = sim_results(), fit_comparison = fit_tbl, path = file)
    }
  )
}

shinyApp(ui, server)
