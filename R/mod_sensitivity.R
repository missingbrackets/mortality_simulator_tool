library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

mod_sensitivity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        3,
        h4("Sensitivity settings"),
        numericInput(ns("pct_change"), "Perturbation (+/- %)", value = 25, min = 1, max = 200, step = 5),
        numericInput(ns("sens_sims"), "Simulations per scenario", value = 2000, min = 100, step = 100),
        selectInput(ns("output_metric"), "Output metric", choices = c(
          "Expected ground-up loss (USD m)" = "ground_up",
          "Expected our layer loss (USD m)" = "our_layer",
          "Probability our layer is hit" = "prob_hit",
          "Expected primary loss (USD m)" = "primary"
        )),
        actionButton(ns("run_sensitivity"), "Run sensitivity", class = "btn-primary"),
        br(), br(),
        textOutput(ns("sens_status"))
      ),
      column(
        9,
        plotOutput(ns("tornado_plot"), height = "450px"),
        br(),
        DTOutput(ns("sens_table"))
      )
    )
  )
}

extract_metric <- function(res, metric) {
  switch(metric,
    ground_up = res$summary$expected_ground_up_m,
    our_layer = res$summary$expected_our_layer_loss_m,
    prob_hit  = res$summary$prob_our_layer_hit,
    primary   = res$summary$expected_primary_loss_m
  )
}

metric_label <- function(metric) {
  switch(metric,
    ground_up = "Expected ground-up loss (USD m)",
    our_layer = "Expected our layer loss (USD m)",
    prob_hit  = "Probability our layer is hit",
    primary   = "Expected primary loss (USD m)"
  )
}

mod_sensitivity_server <- function(id, base_assumptions, severity_tbl, selected_fit) {
  moduleServer(id, function(input, output, session) {
    sens_data <- reactiveVal(NULL)
    sens_status_txt <- reactiveVal("")

    observeEvent(input$run_sensitivity, {
      req(base_assumptions())
      req(severity_tbl())
      req(selected_fit())

      sens_status_txt("Running sensitivity analysis...")
      assumptions <- base_assumptions()
      fit <- selected_fit()
      sev_tbl <- severity_tbl()
      pct <- input$pct_change / 100
      n_sims <- input$sens_sims
      metric <- input$output_metric

      params <- list(
        list(name = "Annual claim frequency", key = "annual_claim_freq"),
        list(name = "Attritional mean (USD m)", key = "avg_attritional_m"),
        list(name = "Attritional CV", key = "attritional_cv"),
        list(name = "Number of productions", key = "n_productions"),
        list(name = "Total budget (USD m)", key = "total_budget_m"),
        list(name = "SIR attachment (USD m)", key = "sir_m"),
        list(name = "Primary limit (USD m)", key = "primary_limit_m"),
        list(name = "Our layer limit (USD m)", key = "our_limit_m")
      )

      withProgress(message = "Sensitivity analysis", value = 0, {
        base_a <- assumptions
        base_a$n_sims <- n_sims
        base_res <- simulate_portfolio_losses(base_a, sev_tbl, fit)
        base_val <- extract_metric(base_res, metric)
        incProgress(1 / (length(params) + 1), detail = "Base scenario done")

        results <- map_dfr(params, function(p) {
          incProgress(1 / (length(params) + 1), detail = p$name)

          base_input <- assumptions[[p$key]]

          run_scenario <- function(multiplier) {
            a <- assumptions
            a$n_sims <- n_sims
            val <- base_input * multiplier
            if (p$key == "n_productions") val <- max(1L, as.integer(round(val)))
            a[[p$key]] <- val
            tryCatch({
              res <- simulate_portfolio_losses(a, sev_tbl, fit)
              list(input_val = val, result = extract_metric(res, metric))
            }, error = function(e) list(input_val = val, result = NA_real_))
          }

          low <- run_scenario(1 - pct)
          high <- run_scenario(1 + pct)

          tibble(
            parameter = p$name,
            base_input = base_input,
            low_input = low$input_val,
            high_input = high$input_val,
            low_result = low$result,
            base_result = base_val,
            high_result = high$result
          )
        })
      })

      sens_data(results)
      sens_status_txt(sprintf("Done. Base %s = %.4f", metric_label(metric), base_val))
    })

    output$sens_status <- renderText(sens_status_txt())

    output$tornado_plot <- renderPlot({
      req(sens_data())
      df <- sens_data() %>%
        filter(!is.na(low_result), !is.na(high_result)) %>%
        mutate(
          low_delta = low_result - base_result,
          high_delta = high_result - base_result,
          swing = abs(high_result - low_result)
        ) %>%
        arrange(swing) %>%
        mutate(parameter = factor(parameter, levels = parameter))

      if (nrow(df) == 0) return(NULL)

      df_long <- df %>%
        select(parameter, low_delta, high_delta) %>%
        pivot_longer(cols = c(low_delta, high_delta), names_to = "direction", values_to = "delta")

      ggplot(df_long, aes(x = delta, y = parameter, fill = direction)) +
        geom_col(position = "identity") +
        scale_fill_manual(
          values = c("low_delta" = "#E74C3C", "high_delta" = "#27AE60"),
          labels = c("Low (-)", "High (+)")
        ) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
        labs(
          title = paste0("Tornado: sensitivity to +/- ", input$pct_change, "% perturbation"),
          x = paste("Change in", metric_label(input$output_metric)),
          y = NULL,
          fill = "Direction"
        ) +
        theme_minimal(base_size = 13)
    })

    output$sens_table <- renderDT({
      req(sens_data())
      df <- sens_data() %>%
        mutate(swing = abs(high_result - low_result)) %>%
        arrange(desc(swing))
      datatable(df, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, autoWidth = TRUE)) %>%
        formatRound(c("base_input", "low_input", "high_input", "low_result", "base_result", "high_result", "swing"), digits = 4)
    })
  })
}
