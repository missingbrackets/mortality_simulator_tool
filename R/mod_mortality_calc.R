library(shiny)
library(DT)

mod_mortality_calc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        h4("Portfolio structure"),
        numericInput(ns("n_productions"), "Number of productions", value = 65, min = 1, step = 1),
        numericInput(ns("production_duration_days"), "Average production duration (days)", value = 180, min = 1, step = 1),
        tags$hr(),
        h4("Mortality-based frequency calculator"),
        p("Derive an annual claim frequency from actor mortality assumptions, then push it to the sidebar."),
        numericInput(ns("key_actor_age"), "Assumed key actor age", value = 45, min = 0, max = 110, step = 1),
        numericInput(ns("avg_key_actors"), "Average key actors per production", value = 4, min = 1, step = 1),
        numericInput(ns("injury_load"), "Injury rate multiple to mortality", value = 1.5, min = 0, step = 0.1),
        helpText("Exposure period is read from the main sidebar."),
        tags$hr(),
        verbatimTextOutput(ns("freq_result")),
        actionButton(ns("use_freq"), "Use this frequency", class = "btn-primary mt-2")
      ),
      column(
        8,
        h4("Mortality table"),
        fileInput(ns("mortality_file"), "Upload mortality CSV", accept = c(".csv")),
        uiOutput(ns("mortality_status")),
        h5("Derivation breakdown"),
        tableOutput(ns("derivation_table")),
        br(),
        h5("Mortality table preview"),
        DTOutput(ns("mort_preview"))
      )
    )
  )
}

mod_mortality_calc_server <- function(id, exposure_years, mortality_data) {
  moduleServer(id, function(input, output, session) {

    mortality_status_txt <- reactiveVal("Using default/previously loaded mortality table.")

    observeEvent(input$mortality_file, {
      req(input$mortality_file$datapath)
      tryCatch({
        clean_df <- clean_mortality_table(read_csv(input$mortality_file$datapath, show_col_types = FALSE))
        mortality_data(clean_df)
        mortality_status_txt(sprintf("Mortality table loaded: %s rows.", nrow(clean_df)))
        showNotification("Mortality table uploaded.", type = "message")
      }, error = function(e) {
        mortality_status_txt("Upload failed. Using previous table.")
        showNotification(paste("Mortality upload failed:", e$message), type = "error", duration = NULL)
      })
    })

    output$mortality_status <- renderUI(tags$small(style = "color:#555;", mortality_status_txt()))

    computed <- reactive({
      req(input$key_actor_age, input$avg_key_actors, input$injury_load)
      req(input$n_productions, input$production_duration_days)
      mort <- mortality_data()
      req(mort, nrow(mort) > 0)
      exp_years <- exposure_years()
      req(exp_years)

      probs <- per_actor_event_probs(
        age                    = input$key_actor_age,
        mortality_tbl          = mort,
        production_duration_days = input$production_duration_days,
        injury_load            = input$injury_load
      )

      n_actors_total    <- input$n_productions * input$avg_key_actors
      expected_deaths   <- n_actors_total * probs$death_prob
      expected_injuries <- n_actors_total * probs$injury_prob
      expected_total    <- expected_deaths + expected_injuries
      annual_freq       <- pmax(expected_total / exp_years, 1e-8)

      list(
        annual_freq       = annual_freq,
        probs             = probs,
        n_actors_total    = n_actors_total,
        expected_deaths   = expected_deaths,
        expected_injuries = expected_injuries,
        expected_total    = expected_total
      )
    })

    output$freq_result <- renderText({
      res <- computed()
      paste0(
        "Annual claim frequency: ", formatC(res$annual_freq, format = "f", digits = 4), "\n",
        "Total expected events over exposure: ", formatC(res$expected_total, format = "f", digits = 2)
      )
    })

    output$derivation_table <- renderTable({
      res <- computed()
      data.frame(
        Step = c(
          "Annual death probability (qx)",
          "Annual injury probability (qx x injury load)",
          "Per-production death probability",
          "Per-production injury probability",
          "Total key actors across portfolio",
          "Expected death events (full term)",
          "Expected injury events (full term)",
          "Total expected events (full term)",
          "Annual claim frequency"
        ),
        Value = c(
          formatC(res$probs$qx_death_annual,  format = "f", digits = 6),
          formatC(res$probs$qx_injury_annual, format = "f", digits = 6),
          formatC(res$probs$death_prob,        format = "f", digits = 6),
          formatC(res$probs$injury_prob,       format = "f", digits = 6),
          as.character(res$n_actors_total),
          formatC(res$expected_deaths,         format = "f", digits = 4),
          formatC(res$expected_injuries,       format = "f", digits = 4),
          formatC(res$expected_total,          format = "f", digits = 4),
          formatC(res$annual_freq,             format = "f", digits = 4)
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")

    output$mort_preview <- renderDT({
      req(mortality_data())
      datatable(mortality_data(), options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE) %>%
        formatRound("qx", digits = 6) %>%
        formatRound("lx", digits = 0)
    })

    list(
      computed_freq          = reactive(computed()$annual_freq),
      use_freq_trigger       = reactive(input$use_freq),
      n_productions          = reactive(input$n_productions),
      production_duration_days = reactive(input$production_duration_days)
    )
  })
}
