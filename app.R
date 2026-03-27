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
  titlePanel("Risk Pricing Tool — Frequency Severity Model"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Core assumptions"),
      dateInput("inception_date", "Inception date", value = "2026-04-01", format = "yyyy-mm-dd"),
      numericInput("exposure_years", "Exposure period (years)", value = 4, min = 0.25, step = 0.25),

      tags$hr(),
      h4("Portfolio structure"),
      numericInput("n_productions", "Number of risks / exposure units", value = 65, min = 1, step = 1),
      numericInput("production_duration_days", "Average risk duration (days)", value = 180, min = 1, step = 1),
      helpText("If an exposure schedule is uploaded in Data Inputs, the risk count is taken from that file instead."),

      tags$hr(),
      h4("Per-risk exposure limits"),
      uiOutput("per_risk_limits_ui"),

      tags$hr(),
      h4("Frequency"),
      radioButtons(
        "freq_basis",
        "Frequency basis",
        choices  = c("Per year" = "per_year", "Per risk" = "per_production"),
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
          h4("Exposure schedule"),
          p("Optional. Upload a CSV with one row per risk. When provided, overrides the sidebar risk count and per-risk limit inputs."),
          p(tags$b("Required column:"), " limit_m (aliases: limit, sum_insured, budget_m, tsi_m)"),
          p(tags$b("Optional columns:"), " name, start_date, end_date (if omitted, dates are spread evenly using sidebar inputs)"),
          fileInput("risk_schedule_file", "Upload exposure schedule CSV", accept = c(".csv")),
          uiOutput("risk_schedule_status"),
          DTOutput("risk_schedule_preview"),
          br(),
          h4("Severity table"),
          p("Required columns: loss_m and one of return_period / return_period_years / return_period_claims"),
          fileInput("severity_file", "Upload severity CSV", accept = c(".csv")),
          uiOutput("severity_status"),
          DTOutput("severity_preview"),
          br(),
          h4("Aggregate claims history (last 5 years)"),
          p("Optional; used for context and benchmarking only."),
          fileInput("claims_history_file", "Upload CSV", accept = c(".csv")),
          DTOutput("claims_history_preview")
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
          h4("Loss by reinsurance layer"),
          DTOutput("layer_summary_table"),
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
        ),
        tabPanel(
          "Methodology",
          br(),
          fluidRow(column(10, offset = 1,
            h3("Model Methodology"),
            p(style = "color:#555;", "This document describes how the tool constructs its outputs from the inputs provided."),

            tags$hr(),
            h4("1. Overall structure"),
            p("The tool uses a ", strong("frequency-severity Monte Carlo simulation"), " framework. For each simulation trial,
              the number of large loss events is drawn from a frequency distribution, each event is assigned an independent
              severity drawn from a fitted parametric distribution, and the resulting portfolio losses are passed through
              attritional loading and the reinsurance layer structure. The process is repeated for the specified number
              of simulations (default 5,000) to build a full loss distribution."),

            tags$hr(),
            h4("2. Production schedule"),
            p("A portfolio of productions is generated according to the number of productions and the exposure period.
              Start dates are spread evenly across the exposure period. Each production is assigned a budget drawn
              uniformly between the minimum and maximum budget inputs, with budgets forced to sum to the specified
              total portfolio budget. The budget cap is used later to limit the loss attributable to any single production."),

            tags$hr(),
            h4("3. Frequency model"),
            p("The number of large loss events in each simulation is drawn from a ", strong("Poisson distribution"),
              " with mean equal to the total expected events over the exposure period."),
            p("The expected total depends on the ", strong("frequency basis"), " selected:"),
            tags$ul(
              tags$li(strong("Per year:"), " Total events = annual claim frequency × exposure years."),
              tags$li(strong("Per production:"), " Total events = per-production frequency × number of productions.")
            ),
            p("Events are then allocated uniformly at random across productions in the portfolio.
              The Poisson model captures overdispersion naturally: some simulations will have zero events,
              some will have many, consistent with the rare but heavy-tailed nature of large cast claims."),

            tags$hr(),
            h4("4. Severity model"),
            h5("4a. Input data"),
            p("The user uploads a severity table of empirical ", em("(loss, return period)"), " pairs.
              Return periods may be expressed in years or per-claim terms — the toggle in the sidebar
              controls interpretation. The tool converts all inputs to a common exceedance probability
              scale for fitting."),
            p("Return period to exceedance probability conversion:"),
            tags$ul(
              tags$li(strong("Claims basis (default for fitting):"),
                      " exceedance probability = 1 / return_period_claims."),
              tags$li(strong("Years basis input:"),
                      " return_period_claims = return_period_years × annual_claim_freq,
                      then exceedance probability = 1 / return_period_claims.")
            ),
            p("Only points with return period > the minimum RP cutoff (set in the Curve Fitting tab) are used
              in fitting, because points at or below RP = 1 imply an exceedance probability ≥ 1, which is
              outside the valid range of a CDF."),

            h5("4b. Distribution fitting"),
            p("Four parametric families are fitted by least-squares regression in transformed probability space:"),
            tags$ul(
              tags$li(strong("Lognormal:"), " log(loss) regressed against the normal quantile of (1 − exceedance probability)."),
              tags$li(strong("Weibull:"), " log(loss) regressed against the log of the Weibull quantile."),
              tags$li(strong("Gamma:"), " fitted via moment matching on log-transformed probabilities."),
              tags$li(strong("Generalised Pareto (GPD):"), " tail-focused fit via method of moments on exceedance probabilities.")
            ),
            p("The best-fitting distribution is selected automatically by lowest root-mean-squared error
              on the log-loss scale, but the user can override this in the Curve Fitting tab. The selected
              fit is used to sample severities during simulation via the distribution's random-draw function."),

            tags$hr(),
            h4("5. Budget cap"),
            p("For each production, the uncapped aggregate loss from all events assigned to it is compared
              to that production's budget. If the raw aggregate exceeds the budget, a ", strong("pro-rata cap factor"),
              " (budget ÷ raw aggregate) is applied to all individual event severities for that production.
              This reflects the fact that a production cannot sustain losses in excess of its total cost —
              the insured value is implicitly limited to the production budget."),

            tags$hr(),
            h4("6. Attritional losses"),
            p("A per-simulation attritional loss is added to represent the aggregate of smaller, frequent
              claims not captured by the severity curve (which is calibrated to large events only).
              Attritional losses are drawn from a ", strong("lognormal distribution"), " parameterised by
              the user-supplied mean and coefficient of variation (CV). The lognormal parameters are derived as:"),
            tags$ul(
              tags$li("σ = sqrt(log(1 + CV²))"),
              tags$li("μ = log(mean) − 0.5 σ²")
            ),
            p("Ground-up loss for each simulation = capped large-loss + attritional."),

            tags$hr(),
            h4("7. Reinsurance layer structure"),
            p("The reinsurance structure is defined in the ", strong("Reinsurance Structure tab"), " as an ordered
              stack of layers. Each layer has a name, a limit, an optional 'our layer' tag, and a ", strong("basis"), "."),
            p("Attachment points are computed automatically as the cumulative sum of all limits beneath:"),
            tags$ul(
              tags$li("Layer 1 (retention/SIR): attaches at 0, exhausts at its limit."),
              tags$li("Layer 2: attaches where Layer 1 exhausts, and so on.")
            ),
            h5("Occurrence basis (standard XL)"),
            p("Each individual large-loss event is tested against the layer independently:"),
            tags$blockquote(
              style = "background:#f8f9fa; border-left:4px solid #18bc9c; padding:8px 16px; font-family:monospace;",
              "event_layer_loss = min( max(event_severity − attachment, 0), limit )",
              tags$br(),
              "sim_layer_loss = Σ event_layer_loss  across all events in that simulation"
            ),
            p("Multiple events can each penetrate the layer in the same period. Attritional losses are a single
              aggregate number per simulation and do not appear as individual events, so they do not
              contribute to occurrence layer losses (consistent with attritional claims being too small to
              individually breach the SIR)."),
            h5("Aggregate basis (stop-loss / aggregate XL)"),
            p("The total cumulative loss for the period erodes the layer once:"),
            tags$blockquote(
              style = "background:#f8f9fa; border-left:4px solid #18bc9c; padding:8px 16px; font-family:monospace;",
              "sim_layer_loss = min( max(ground_up_m − attachment, 0), limit )"
            ),
            p("Here ground_up_m = capped large-loss total + attritional. Attritional losses therefore
              contribute to the erosion of aggregate layers, which is the correct treatment for
              aggregate stop-loss covers."),
            p("The three reported output aggregates are:"),
            tags$ul(
              tags$li(strong("SIR erosion:"), " loss to the first (retention) layer."),
              tags$li(strong("Primary loss:"), " sum of losses to all layers that are neither the retention nor tagged as our layer."),
              tags$li(strong("Our layer loss:"), " sum of losses to all layers tagged as 'our layer'.")
            ),
            p("Losses in excess of the top of the defined stack are uninsured and are not allocated to any layer."),

            tags$hr(),
            h4("8. Output metrics"),
            tags$ul(
              tags$li(strong("Expected loss (any layer):"), " arithmetic mean of per-simulation losses to that layer across all simulations."),
              tags$li(strong("Probability our layer is hit:"), " proportion of simulations in which our layer loss > 0."),
              tags$li(strong("Average events:"), " mean number of large-loss events per simulation (Poisson mean ≈ total expected events)."),
              tags$li(strong("Average event severity:"), " mean of all individual event severities across all simulations (after budget cap).")
            ),

            tags$hr(),
            h4("9. Mortality calculator (optional)"),
            p("The Mortality Calculator tab provides a tool to derive an implied annual claim frequency
              from underlying actor mortality assumptions. It is not used directly by the simulation — it
              is a helper to inform the frequency input."),
            p("The derivation proceeds as follows:"),
            tags$ol(
              tags$li("Look up the annual death probability (qx) for the assumed key actor age from the mortality table."),
              tags$li("Compute the injury probability as qx × injury rate multiple."),
              tags$li("Convert annual probabilities to per-production probabilities using the production duration:
                       P(event during production) = 1 − (1 − annual_q)^(duration_days / 365.25)."),
              tags$li("Multiply per-production probabilities by total key actors across the portfolio
                       (number of productions × average key actors per production)."),
              tags$li("Divide total expected events by exposure years to obtain annual claim frequency.")
            ),
            p("Clicking ", strong("'Use this frequency'"), " pushes the computed annual frequency to the sidebar
              frequency input and sets the basis to 'per year'."),

            tags$hr(),
            h4("10. Sensitivity analysis"),
            p("The Sensitivity tab perturbs each parameter independently by ±% (user-configurable) while
              holding all others at their base values. One base scenario and two scenarios per parameter
              (low and high) are simulated, each using a reduced simulation count for speed.
              Results are displayed as a tornado chart showing the change in the selected output metric
              relative to the base, sorted by absolute impact.")
          ))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  claims_history_data <- reactiveVal(tibble(year = integer(), aggregate_loss_m = numeric()))
  mortality_data      <- reactiveVal(clean_mortality_table(default_mortality_table()))
  severity_raw_data   <- reactiveVal(default_severity_table())
  risk_schedule_data  <- reactiveVal(NULL)

  sim_results           <- reactiveVal(NULL)
  run_status_txt        <- reactiveVal("No model run yet.")
  severity_status_txt   <- reactiveVal("Using default severity table.")
  risk_schedule_status_txt <- reactiveVal("")

  # --- Modules ---
  mort_calc <- mod_mortality_calc_server(
    "mort_calc",
    n_productions            = reactive(input$n_productions),
    production_duration_days = reactive(input$production_duration_days),
    exposure_years           = reactive(input$exposure_years),
    mortality_data           = mortality_data
  )

  observeEvent(mort_calc$use_freq_trigger(), {
    updateNumericInput(session, "annual_claim_freq", value = round(mort_calc$computed_freq(), 4))
    updateRadioButtons(session, "freq_basis", selected = "per_year")
  })

  reinsurance <- mod_reinsurance_server("reinsurance")

  # --- Sidebar: per-risk limit section (conditional on schedule upload) ---
  output$per_risk_limits_ui <- renderUI({
    sched <- risk_schedule_data()
    if (!is.null(sched)) {
      div(class = "alert alert-info p-2 mb-0",
        tags$small(
          sprintf("%d risks loaded from schedule. Total limit: $%.0fm  |  Avg: $%.0fm  |  Min: $%.0fm  |  Max: $%.0fm",
                  nrow(sched), sum(sched$limit_m), mean(sched$limit_m),
                  min(sched$limit_m), max(sched$limit_m))
        )
      )
    } else {
      tagList(
        numericInput("total_budget_m", "Total limit across all risks (USD m)", value = 3000, min = 1, step = 1),
        numericInput("min_budget_m",   "Minimum per-risk limit (USD m)",       value = 20,   min = 0.1, step = 0.1),
        numericInput("max_budget_m",   "Maximum per-risk limit (USD m)",       value = 100,  min = 0.1, step = 0.1),
        helpText("Limits randomly assigned between min and max, forced to sum to total.")
      )
    }
  })

  # --- Dynamic frequency input ---
  output$freq_input_ui <- renderUI({
    label <- if (isTRUE(input$freq_basis == "per_production")) "Expected claims per risk" else "Expected claims per year"
    numericInput("annual_claim_freq", label, value = 0.12, min = 0, step = 0.01)
  })

  output$freq_info <- renderUI({
    req(input$annual_claim_freq)
    sched   <- risk_schedule_data()
    n_prods <- if (!is.null(sched)) nrow(sched) else (input$n_productions %||% 65)
    if (isTRUE(input$freq_basis == "per_production")) {
      total <- input$annual_claim_freq * n_prods
      tags$small(style = "color:#555;",
        sprintf("Total expected events: %.2f  (%d risks × %.4f)", total, n_prods, input$annual_claim_freq))
    } else {
      total <- input$annual_claim_freq * input$exposure_years
      tags$small(style = "color:#555;",
        sprintf("Total expected events over %.1f-year exposure: %.2f", input$exposure_years, total))
    }
  })

  # --- Core assumptions ---
  assumptions_reactive <- reactive({
    req(input$annual_claim_freq)
    sched   <- risk_schedule_data()
    n_prods <- if (!is.null(sched)) nrow(sched) else (input$n_productions %||% 65)
    list(
      n_productions            = n_prods,
      inception_date           = as.Date(input$inception_date),
      exposure_years           = input$exposure_years,
      production_duration_days = input$production_duration_days %||% 180,
      total_budget_m           = input$total_budget_m %||% 3000,
      min_budget_m             = input$min_budget_m   %||% 20,
      max_budget_m             = input$max_budget_m   %||% 100,
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

  output$run_status      <- renderText(run_status_txt())
  output$severity_status <- renderUI(tags$small(style = "color:#555;", severity_status_txt()))
  output$risk_schedule_status <- renderUI(tags$small(style = "color:#555;", risk_schedule_status_txt()))

  # --- File uploads ---
  observeEvent(input$risk_schedule_file, {
    req(input$risk_schedule_file$datapath)
    tryCatch({
      raw  <- read_csv(input$risk_schedule_file$datapath, show_col_types = FALSE)
      sched <- clean_risk_schedule(raw)
      risk_schedule_data(sched)
      risk_schedule_status_txt(sprintf(
        "Schedule loaded: %d risks, total limit $%.0fm, avg $%.0fm.",
        nrow(sched), sum(sched$limit_m), mean(sched$limit_m)
      ))
      showNotification(sprintf("Exposure schedule loaded: %d risks.", nrow(sched)), type = "message")
    }, error = function(e) {
      risk_schedule_status_txt(paste("Upload failed:", e$message))
      showNotification(paste("Schedule upload failed:", e$message), type = "error", duration = NULL)
    })
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
  output$risk_schedule_preview <- renderDT({
    req(risk_schedule_data())
    df <- risk_schedule_data()
    dt <- datatable(df, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    dt %>% formatRound("limit_m", digits = 1)
  })

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
    layers           = reinsurance$layers,
    risk_schedule    = risk_schedule_data
  )

  # --- Model run ---
  observeEvent(input$run_model, {
    req(nrow(severity_tbl()) >= 3)
    run_status_txt("Running model...")

    withProgress(message = "Running model", value = 0, {
      incProgress(0.20, detail = "Capturing severity fit")
      fit_obj <- isolate(curve_fit$selected_fit())
      fit_tbl <- isolate(curve_fit$comparison_table())

      incProgress(0.55, detail = "Simulating portfolio")
      res <- simulate_portfolio_losses(
        assumptions    = assumptions_reactive(),
        severity_tbl   = severity_tbl(),
        fit            = fit_obj,
        layers         = reinsurance$layers(),
        risk_schedule  = risk_schedule_data(),
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

  output$layer_summary_table <- renderDT({
    req(sim_results())
    df <- sim_results()$layer_summary %>%
      transmute(
        Layer           = layer,
        Basis           = basis,
        `Our layer`     = ifelse(our_layer, "Yes", ""),
        `Attachment (USD m)` = attachment_m,
        `Limit (USD m)` = limit_m,
        `Expected loss (USD m)` = expected_loss_m,
        `Prob. penetrated`      = prob_hit
      )
    datatable(df, rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE, ordering = FALSE)) %>%
      formatRound(c("Attachment (USD m)", "Limit (USD m)", "Expected loss (USD m)"), digits = 2) %>%
      formatPercentage("Prob. penetrated", digits = 1)
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
