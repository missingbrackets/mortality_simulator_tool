library(dplyr)
library(tidyr)
library(purrr)

simulate_portfolio_losses <- function(assumptions, mortality_tbl, severity_tbl, fit, claims_history = NULL) {
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

  probs <- per_actor_event_probs(
    age = assumptions$key_actor_age,
    mortality_tbl = mortality_tbl,
    production_duration_days = assumptions$production_duration_days,
    injury_load = assumptions$injury_load
  )

  exposure_tbl <- production_schedule %>%
    mutate(
      n_key_actors = assumptions$avg_key_actors,
      death_prob_per_actor = probs$death_prob,
      injury_prob_per_actor = probs$injury_prob
    )

  empty_events <- tibble(
    sim_id = integer(),
    production_id = integer(),
    event_id = integer(),
    event_type = character(),
    raw_severity_m = numeric(),
    severity_m = numeric(),
    production_budget_m = numeric(),
    production_raw_loss_m = numeric(),
    production_capped_loss_m = numeric(),
    cap_factor = numeric()
  )

  event_detail <- map_dfr(seq_len(assumptions$n_sims), function(sim_id) {
    death_counts <- rbinom(nrow(exposure_tbl), size = exposure_tbl$n_key_actors, prob = exposure_tbl$death_prob_per_actor)
    injury_counts <- rbinom(nrow(exposure_tbl), size = exposure_tbl$n_key_actors, prob = exposure_tbl$injury_prob_per_actor)
    total_counts <- death_counts + injury_counts

    if (sum(total_counts) == 0) return(empty_events)

    map_dfr(seq_along(total_counts), function(prod_idx) {
      n_ev <- total_counts[[prod_idx]]
      if (n_ev == 0) return(NULL)

      raw_sev <- fit$rfun(n_ev)
      budget <- exposure_tbl$budget_m[[prod_idx]]
      raw_total <- sum(raw_sev)
      cap_factor <- min(1, budget / raw_total)
      capped_sev <- raw_sev * cap_factor
      n_death <- death_counts[[prod_idx]]

      tibble(
        sim_id = sim_id,
        production_id = prod_idx,
        event_id = seq_len(n_ev),
        event_type = c(rep("death", n_death), rep("injury", n_ev - n_death)),
        raw_severity_m = raw_sev,
        severity_m = capped_sev,
        production_budget_m = budget,
        production_raw_loss_m = raw_total,
        production_capped_loss_m = sum(capped_sev),
        cap_factor = cap_factor
      )
    })
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

  layers <- apply_layers(
    ground_up_m = simulation_summary$ground_up_m,
    sir_m = assumptions$sir_m,
    primary_limit_m = assumptions$primary_limit_m,
    our_limit_m = assumptions$our_limit_m
  )

  simulation_summary <- bind_cols(simulation_summary, layers) %>%
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
    annual_claim_frequency = expected_annual_claim_frequency(assumptions, mortality_tbl),
    qx_used = probs$qx_death_annual
  )

  summary_table <- tibble(
    metric = c(
      "Expected ground-up loss (USD m)",
      "Expected SIR erosion (USD m)",
      "Expected primary loss (USD m)",
      "Expected our layer loss (USD m)",
      "Probability our layer is hit",
      "Average cast death/injury events",
      "Average cast-event severity after budget cap (USD m)",
      "Average productions capped at budget",
      "Expected annual cast-event frequency",
      "Mortality qx used",
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
      summary$qx_used,
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
    claims_history = claims_history
  )
}
