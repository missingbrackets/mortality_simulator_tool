library(shiny)
library(DT)
library(ggplot2)

curve_param_names <- function(distribution) {
  switch(
    distribution,
    lognormal = c("meanlog", "sdlog"),
    weibull = c("shape", "scale"),
    gamma = c("shape", "scale"),
    gpd = c("xi", "beta", "threshold")
  )
}

mod_curve_fit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        selectInput(ns("distribution"), "Candidate curve", choices = c("lognormal", "weibull", "gamma", "gpd"), selected = "lognormal"),
        radioButtons(ns("prob_view"), "Probability view", choices = c("Exceedance" = "exceed", "CDF" = "cdf"), selected = "exceed", inline = TRUE),
        numericInput(ns("param1"), "meanlog", value = NA, step = 0.001),
        numericInput(ns("param2"), "sdlog", value = NA, step = 0.001),
        numericInput(ns("param3"), "Unused", value = NA, step = 0.001),
        helpText("Changing the candidate curve re-fits to the converted claim-based severity table and loads fitted starting parameters. You can then tweak them manually."),
        verbatimTextOutput(ns("param_help"))
      ),
      column(
        8,
        plotOutput(ns("fit_plot"), height = "420px"),
        DTOutput(ns("comparison_table"))
      )
    )
  )
}

mod_curve_fit_server <- function(id, severity_tbl) {
  moduleServer(id, function(input, output, session) {
    auto_fit <- reactive({
      req(severity_tbl())
      req(nrow(severity_tbl()) >= 3)
      fit_severity_curve(severity_tbl(), distribution = input$distribution, params_override = list())
    })

    observeEvent(list(input$distribution, severity_tbl()), {
      fit <- auto_fit()
      param_names <- curve_param_names(input$distribution)
      param_values <- unname(unlist(fit$params))
      labels <- c(param_names, rep("Unused", 3 - length(param_names)))
      values <- c(param_values, rep(NA_real_, 3 - length(param_values)))

      updateNumericInput(session, "param1", label = labels[1], value = values[1])
      updateNumericInput(session, "param2", label = labels[2], value = values[2])
      updateNumericInput(session, "param3", label = labels[3], value = values[3])
    }, ignoreInit = FALSE)

    output$param_help <- renderText({
      fit <- auto_fit()
      paste0(
        "Base fitted parameters for ", input$distribution, ":\n",
        paste(names(fit$params), formatC(unlist(fit$params), format = "f", digits = 6), sep = " = ", collapse = "\n")
      )
    })

    override_params <- reactive({
      vals <- c(input$param1, input$param2, input$param3)
      param_names <- curve_param_names(input$distribution)
      out <- list()
      for (i in seq_along(param_names)) {
        if (!is.null(vals[i]) && is.finite(vals[i])) out[[param_names[i]]] <- as.numeric(vals[i])
      }
      out
    })

    selected_fit <- eventReactive(
      list(severity_tbl(), input$distribution, input$param1, input$param2, input$param3),
      {
        req(severity_tbl())
        req(nrow(severity_tbl()) >= 3)
        fit_severity_curve(severity_tbl(), distribution = input$distribution, params_override = override_params())
      },
      ignoreInit = FALSE
    )

    comparison_table <- reactive({
      selected_fit()$comparison_table
    })

    output$comparison_table <- renderDT({
      req(comparison_table())
      datatable(
        comparison_table(),
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE)
      ) %>%
        formatStyle(
          "source",
          target = "row",
          fontWeight = styleEqual(c("Input", "Padded"), c("bold", "normal"))
        ) %>%
        formatRound(c("loss_m", "return_period_years", "return_period_claims", "fitted_loss_at_input_percentile_m"), digits = 2) %>%
        formatRound(c("input_exceed_prob", "percentile", "fitted_percentile", "fitted_exceed_prob"), digits = 4) %>%
        formatPercentage("loss_error_pct", digits = 2)
    })

    output$fit_plot <- renderPlot({
      req(comparison_table())
      fit_obj <- selected_fit()
      df <- comparison_table()

      min_p <- max(min(df$percentile, na.rm = TRUE) * 0.8, 1e-6)
      max_p <- min(max(df$percentile, na.rm = TRUE) + 0.01, 0.9999)

      p_grid <- unique(c(
        seq(min_p, min(0.95, max_p), length.out = 120),
        seq(max(0.95, min_p), max_p, length.out = 120)
      ))
      p_grid <- sort(safe_p(p_grid))

      curve_df <- data.frame(
        percentile = p_grid,
        loss_m = fit_obj$qfun(p_grid)
      )

      if (input$prob_view == "cdf") {
        df$yval <- ifelse(df$source == "Input", df$percentile, df$fitted_percentile)
        curve_df$yval <- curve_df$percentile
        y_lab <- "CDF / Percentile"
      } else {
        df$yval <- ifelse(df$source == "Input", df$input_exceed_prob, df$fitted_exceed_prob)
        curve_df$yval <- 1 - curve_df$percentile
        y_lab <- "Exceedance probability"
      }

      x_rng <- range(c(df$loss_m, curve_df$loss_m), na.rm = TRUE)

      p <- ggplot() +
        geom_line(data = curve_df, aes(x = loss_m, y = yval), linewidth = 1) +
        geom_point(data = df, aes(x = loss_m, y = yval, colour = source), size = 3, alpha = 0.9) +
        scale_colour_manual(values = c("Input" = "#D55E00", "Padded" = "#0072B2")) +
        coord_cartesian(xlim = x_rng) +
        labs(
          title = paste("Severity fit:", input$distribution),
          x = "Loss (USD m)",
          y = y_lab,
          colour = "Point type"
        ) +
        theme_minimal(base_size = 13)

      if (input$prob_view == "exceed") {
        p <- p + scale_y_continuous(trans = "log10")
      }

      p
    })

    list(
      selected_fit = selected_fit,
      comparison_table = comparison_table
    )
  })
}
