library(dplyr)
library(tidyr)
library(purrr)

simulate_portfolio_losses <- function(assumptions, severity_tbl, fit, layers, claims_history = NULL) {
  set.seed(assumptions$seed)

  production_schedule <- make_production_schedule(
    n_productions = assumptions$n_productions,
    inception_date = assumptions$inception_date,
    exposure_years = assumptions$exposure_years,
    production_duration_days = assumptions$production_duration_days
  ) %>%
    mutate(
      budget_m = assign_production_budgets(
        n = assumptions$n_productions,
        total_budget_m = assumptions$total_budget_m,
        min_budget_m = assumptions$min_budget_m,
        max_budget_m = assumptions$max_budget_m
      )
    )

  total_expected_events <- if (isTRUE(assumptions$freq_basis == "per_production")) {
    assumptions$annual_claim_freq * assumptions$n_productions
  } else {
    assumptions$annual_claim_freq * assumptions$exposure_years
  }
  n_prods <- nrow(production_schedule)
  budgets <- production_schedule$budget_m

  empty_events <- tibble(
    sim_id = integer(),
    production_id = integer(),
    event_id = integer(),
    raw_severity_m = numeric(),
    severity_m = numeric(),
    production_budget_m = numeric(),
    production_raw_loss_m = numeric(),
    production_capped_loss_m = numeric(),
    cap_factor = numeric()
  )

  event_detail <- map_dfr(seq_len(assumptions$n_sims), function(sim_id) {
    n_events <- rpois(1, total_expected_events)
    if (n_events == 0) return(empty_events)

    prod_assignments <- sample(seq_len(n_prods), size = n_events, replace = TRUE)
    raw_sev <- fit$rfun(n_events)

    ev <- tibble(
      sim_id = sim_id,
      production_id = prod_assignments,
      raw_severity_m = raw_sev,
      production_budget_m = budgets[prod_assignments]
    )

    ev %>%
      group_by(production_id) %>%
      mutate(
        production_raw_loss_m = sum(raw_severity_m),
        cap_factor = pmin(1, production_budget_m / production_raw_loss_m),
        severity_m = raw_severity_m * cap_factor,
        production_capped_loss_m = sum(severity_m)
      ) %>%
      ungroup() %>%
      mutate(event_id = row_number()) %>%
      select(sim_id, production_id, event_id, raw_severity_m, severity_m,
             production_budget_m, production_raw_loss_m, production_capped_loss_m, cap_factor)
  })

  sim_ids <- tibble(sim_id = seq_len(assumptions$n_sims))

  cast_loss_by_sim <- event_detail %>%
    group_by(sim_id) %>%
    summarise(
      cast_event_count = n(),
      cast_loss_m = sum(severity_m),
      cast_raw_loss_m = sum(raw_severity_m),
      avg_event_severity_m = mean(severity_m),
      productions_with_events = n_distinct(production_id),
      productions_capped = n_distinct(production_id[production_raw_loss_m > production_budget_m + 1e-10]),
      .groups = "drop"
    )

  attritional <- tibble(
    sim_id = seq_len(assumptions$n_sims),
    attritional_loss_m = r_lognormal_from_mean_cv(assumptions$n_sims, assumptions$avg_attritional_m, assumptions$attritional_cv)
  )

  simulation_summary <- sim_ids %>%
    left_join(cast_loss_by_sim, by = "sim_id") %>%
    left_join(attritional, by = "sim_id") %>%
    mutate(
      cast_event_count = coalesce(cast_event_count, 0L),
      cast_loss_m = coalesce(cast_loss_m, 0),
      cast_raw_loss_m = coalesce(cast_raw_loss_m, 0),
      avg_event_severity_m = coalesce(avg_event_severity_m, 0),
      productions_with_events = coalesce(productions_with_events, 0L),
      productions_capped = coalesce(productions_capped, 0L),
      ground_up_m = cast_loss_m + attritional_loss_m
    )

  layer_out     <- apply_layers_flex(simulation_summary$ground_up_m, event_detail, layers)
  layer_summary <- layer_out$layer_summary

  simulation_summary <- bind_cols(simulation_summary, layer_out$sim_losses) %>%
    select(
      sim_id, cast_event_count, productions_with_events, productions_capped,
      cast_raw_loss_m, cast_loss_m, attritional_loss_m, ground_up_m,
      sir_eroded_m, primary_loss_m, our_layer_loss_m, avg_event_severity_m
    )

  summary <- list(
    expected_ground_up_m = mean(simulation_summary$ground_up_m),
    expected_primary_loss_m = mean(simulation_summary$primary_loss_m),
    expected_our_layer_loss_m = mean(simulation_summary$our_layer_loss_m),
    expected_sir_erosion_m = mean(simulation_summary$sir_eroded_m),
    avg_cast_events = mean(simulation_summary$cast_event_count),
    avg_event_severity_m = if (nrow(event_detail) > 0) mean(event_detail$severity_m) else 0,
    prob_our_layer_hit = mean(simulation_summary$our_layer_loss_m > 0),
    avg_productions_capped = mean(simulation_summary$productions_capped),
    annual_claim_frequency = assumptions$annual_claim_freq,
    total_expected_events = total_expected_events
  )

  summary_table <- tibble(
    metric = c(
      "Expected ground-up loss (USD m)",
      "Expected SIR erosion (USD m)",
      "Expected primary loss (USD m)",
      "Expected our layer loss (USD m)",
      "Probability our layer is hit",
      "Average cast events per simulation",
      "Average event severity after budget cap (USD m)",
      "Average productions capped at budget",
      "Annual claim frequency (input)",
      "Total expected events over exposure",
      "Selected severity curve"
    ),
    value = c(
      summary$expected_ground_up_m,
      summary$expected_sir_erosion_m,
      summary$expected_primary_loss_m,
      summary$expected_our_layer_loss_m,
      summary$prob_our_layer_hit,
      summary$avg_cast_events,
      summary$avg_event_severity_m,
      summary$avg_productions_capped,
      summary$annual_claim_frequency,
      summary$total_expected_events,
      fit$distribution
    )
  )

  list(
    assumptions = assumptions,
    production_schedule = production_schedule,
    severity_input = severity_tbl,
    severity_fit_comparison = fit$comparison_table,
    simulation_summary = simulation_summary,
    event_detail = event_detail,
    summary = summary,
    summary_table = summary_table,
    layer_summary = layer_summary,
    claims_history = claims_history
  )
}
