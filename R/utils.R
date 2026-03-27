library(dplyr)
library(tibble)
library(lubridate)
library(DT)

default_mortality_table <- function() {
  ages <- 16:85
  qx <- pmax(0.00015, pmin(0.20, 0.00012 * exp(ages / 18)))
  tibble(
    `Birth Year` = 2026 - ages,
    Age = ages,
    qx = qx,
    lx = round(100000 * cumprod(c(1, 1 - head(qx, -1))))
  )
}

default_severity_table <- function() {
  data.frame(
    loss_m = c(5, 10, 17),
    return_period_years = c(2, 5, 10)
  )
}

clean_risk_schedule <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  names(df) <- gsub("[[:space:]]+", "_", names(df))
  names(df) <- gsub("[^a-z0-9_]", "", names(df))

  # Limit column aliases
  for (alias in c("limit", "sum_insured", "sum_insured_m", "budget_m", "budget", "tsi_m", "tsi")) {
    if (alias %in% names(df) && !"limit_m" %in% names(df))
      names(df)[names(df) == alias] <- "limit_m"
  }
  if (!"limit_m" %in% names(df))
    stop("Exposure schedule must contain a 'limit_m' column (aliases: limit, sum_insured, budget_m, tsi_m).")

  df$limit_m <- as.numeric(df$limit_m)
  if (!"name" %in% names(df)) df$name <- paste0("Risk ", seq_len(nrow(df)))

  out <- df %>%
    filter(is.finite(limit_m), limit_m > 0) %>%
    mutate(name = as.character(name))

  has_dates <- all(c("start_date", "end_date") %in% names(out))
  if (has_dates) {
    out$start_date <- as.Date(out$start_date)
    out$end_date   <- as.Date(out$end_date)
    out %>% select(name, limit_m, start_date, end_date)
  } else {
    out %>% select(name, limit_m)
  }
}

clean_mortality_table <- function(df) {
  names(df) <- trimws(names(df))
  required <- c("Birth Year", "Age", "qx", "lx")
  if (!all(required %in% names(df))) {
    stop("Mortality table must contain columns: Birth Year, Age, qx, lx")
  }

  df %>%
    transmute(
      birth_year = as.integer(`Birth Year`),
      age = as.integer(Age),
      qx = as.numeric(qx),
      lx = as.numeric(lx)
    ) %>%
    filter(is.finite(age), is.finite(qx), is.finite(lx)) %>%
    arrange(age)
}

clean_severity_table <- function(df) {
  names(df) <- tolower(trimws(names(df)))
  names(df) <- gsub("[[:space:]]+", "_", names(df))
  names(df) <- gsub("[^a-z0-9_]", "", names(df))

  # Loss column aliases
  if (!"loss_m" %in% names(df)) {
    if ("loss" %in% names(df)) names(df)[names(df) == "loss"] <- "loss_m"
    if ("severity_m" %in% names(df)) names(df)[names(df) == "severity_m"] <- "loss_m"
    if ("loss_millions" %in% names(df)) names(df)[names(df) == "loss_millions"] <- "loss_m"
  }

  # Return period aliases
  if ("returnperiod" %in% names(df)) names(df)[names(df) == "returnperiod"] <- "return_period"
  if ("return_period_year" %in% names(df)) names(df)[names(df) == "return_period_year"] <- "return_period_years"
  if ("returnperiodyears" %in% names(df)) names(df)[names(df) == "returnperiodyears"] <- "return_period_years"
  if ("rp_years" %in% names(df)) names(df)[names(df) == "rp_years"] <- "return_period_years"
  if ("returnperiodclaims" %in% names(df)) names(df)[names(df) == "returnperiodclaims"] <- "return_period_claims"
  if ("rp_claims" %in% names(df)) names(df)[names(df) == "rp_claims"] <- "return_period_claims"

  if (!"loss_m" %in% names(df)) {
    stop("Severity table must contain a loss column, e.g. loss_m")
  }

  rp_col <- intersect(c("return_period", "return_period_years", "return_period_claims"), names(df))
  if (length(rp_col) == 0) {
    stop("Severity table must contain one of: return_period, return_period_years, return_period_claims")
  }

  keep_cols <- unique(c("loss_m", rp_col[1]))
  out <- df[, keep_cols, drop = FALSE]

  names(out)[names(out) == rp_col[1]] <- "input_return_period"

  out$loss_m <- suppressWarnings(as.numeric(out$loss_m))
  out$input_return_period <- suppressWarnings(as.numeric(out$input_return_period))

  out <- out[is.finite(out$loss_m) & out$loss_m > 0, , drop = FALSE]
  out <- out[is.finite(out$input_return_period) & out$input_return_period > 0, , drop = FALSE]

  if (nrow(out) < 3) {
    stop("Severity table must contain at least 3 valid rows.")
  }

  out
}

standardize_severity_table <- function(severity_tbl_raw,
                                       input_type = c("years", "claims"),
                                       annual_claim_frequency) {
  input_type <- match.arg(input_type)

  df <- clean_severity_table(severity_tbl_raw)
  annual_claim_frequency <- max(as.numeric(annual_claim_frequency), 1e-8)

  if (input_type == "years") {
    df <- df |>
      dplyr::mutate(
        return_period_years = input_return_period,
        return_period_claims = input_return_period * annual_claim_frequency,
        fit_return_period = input_return_period
      )
  } else {
    df <- df |>
      dplyr::mutate(
        return_period_claims = input_return_period,
        return_period_years = input_return_period / annual_claim_frequency,
        fit_return_period = input_return_period
      )
  }

  df |>
    dplyr::mutate(
      input_type = input_type,
      annual_claim_frequency = annual_claim_frequency,
      exceed_prob_fit = pmin(pmax(1 / fit_return_period, 1e-8), 0.999999),
      percentile = 1 - exceed_prob_fit
    ) |>
    dplyr::arrange(loss_m)
}

lookup_qx <- function(age, mortality_tbl) {
  idx <- which.min(abs(mortality_tbl$age - age))
  mortality_tbl$qx[idx]
}

per_actor_event_probs <- function(age, mortality_tbl, production_duration_days, injury_load) {
  qx_death_annual <- pmin(pmax(lookup_qx(age, mortality_tbl), 0), 0.999999)
  qx_injury_annual <- pmin(qx_death_annual * injury_load, 0.999999)
  years <- production_duration_days / 365.25

  death_prob <- 1 - (1 - qx_death_annual)^years
  injury_prob <- 1 - (1 - qx_injury_annual)^years

  list(
    death_prob = pmin(pmax(death_prob, 0), 0.999999),
    injury_prob = pmin(pmax(injury_prob, 0), 0.999999),
    qx_death_annual = qx_death_annual,
    qx_injury_annual = qx_injury_annual
  )
}

expected_annual_claim_frequency <- function(assumptions, mortality_tbl) {
  probs <- per_actor_event_probs(
    age = assumptions$key_actor_age,
    mortality_tbl = mortality_tbl,
    production_duration_days = assumptions$production_duration_days,
    injury_load = assumptions$injury_load
  )

  expected_term_claims <- assumptions$n_productions * assumptions$avg_key_actors *
    (probs$death_prob + probs$injury_prob)

  pmax(expected_term_claims / assumptions$exposure_years, 1e-8)
}

make_production_schedule <- function(n_productions, inception_date, exposure_years, production_duration_days) {
  total_days <- round(365.25 * exposure_years)
  start_offsets <- round(seq(0, total_days, length.out = n_productions + 1))[-(n_productions + 1)]
  start_dates <- as.Date(inception_date) + start_offsets

  tibble(
    production_id = seq_len(n_productions),
    start_date = start_dates,
    end_date = start_dates + production_duration_days,
    exposure_days = production_duration_days,
    exposure_years = production_duration_days / 365.25
  )
}

assign_production_budgets <- function(n, total_budget_m, min_budget_m, max_budget_m) {
  total_budget_m <- as.numeric(total_budget_m)
  min_budget_m <- as.numeric(min_budget_m)
  max_budget_m <- as.numeric(max_budget_m)

  if (min_budget_m <= 0 || max_budget_m <= 0 || total_budget_m <= 0) {
    stop("Budget inputs must all be positive")
  }
  if (min_budget_m > max_budget_m) {
    stop("Min individual film budget cannot exceed max individual film budget")
  }
  if (total_budget_m < n * min_budget_m || total_budget_m > n * max_budget_m) {
    stop(sprintf(
      "Total budget must lie between %.2f and %.2f for %s productions",
      n * min_budget_m,
      n * max_budget_m,
      n
    ))
  }

  budgets <- numeric(n)
  remaining_total <- total_budget_m

  for (i in seq_len(n - 1)) {
    remaining_n <- n - i
    lower <- max(min_budget_m, remaining_total - remaining_n * max_budget_m)
    upper <- min(max_budget_m, remaining_total - remaining_n * min_budget_m)
    budgets[i] <- runif(1, min = lower, max = upper)
    remaining_total <- remaining_total - budgets[i]
  }
  budgets[n] <- remaining_total

  sample(budgets, size = n, replace = FALSE)
}

r_lognormal_from_mean_cv <- function(n, mean, cv) {
  if (mean <= 0) return(rep(0, n))
  if (cv <= 0) return(rep(mean, n))
  sdlog <- sqrt(log(1 + cv^2))
  meanlog <- log(mean) - 0.5 * sdlog^2
  rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}

apply_layers <- function(ground_up_m, sir_m, primary_limit_m, our_limit_m) {
  sir_eroded_m <- pmin(ground_up_m, sir_m)
  excess_over_sir_m <- pmax(ground_up_m - sir_m, 0)
  primary_loss_m <- pmin(excess_over_sir_m, primary_limit_m)
  our_layer_loss_m <- pmin(pmax(ground_up_m - sir_m - primary_limit_m, 0), our_limit_m)

  tibble(
    sir_eroded_m = sir_eroded_m,
    primary_loss_m = primary_loss_m,
    our_layer_loss_m = our_layer_loss_m
  )
}

# Flexible N-layer version.
# layers_tbl columns: attachment_m, limit_m, is_our_layer, is_aggregate
# event_detail columns: sim_id, severity_m  (large-loss events only)
#
# Occurrence layers (is_aggregate = FALSE):
#   Each event is tested independently — per-event loss summed per sim.
#   Attritional losses do NOT contribute (not in event_detail).
#
# Aggregate layers (is_aggregate = TRUE):
#   Total ground_up_m (large-loss + attritional) erodes the layer once.
apply_layers_flex <- function(ground_up_m, event_detail, layers_tbl) {
  n        <- length(ground_up_m)
  n_layers <- nrow(layers_tbl)

  layer_mat <- matrix(0, nrow = n, ncol = n_layers)

  for (j in seq_len(n_layers)) {
    lyr <- layers_tbl[j, ]

    if (isTRUE(lyr$is_aggregate)) {
      layer_mat[, j] <- pmin(pmax(ground_up_m - lyr$attachment_m, 0), lyr$limit_m)

    } else {
      if (nrow(event_detail) > 0) {
        ev_loss <- pmin(pmax(event_detail$severity_m - lyr$attachment_m, 0), lyr$limit_m)
        ev_df   <- tibble(sim_id = event_detail$sim_id, ev_loss = ev_loss) %>%
          group_by(sim_id) %>%
          summarise(total = sum(ev_loss), .groups = "drop")
        layer_mat[, j] <- left_join(tibble(sim_id = seq_len(n)), ev_df, by = "sim_id") %>%
          mutate(total = coalesce(total, 0)) %>%
          pull(total)
      }
    }
  }

  our_idx   <- which(layers_tbl$is_our_layer)
  other_idx <- setdiff(seq_len(n_layers)[-1], our_idx)

  # Per-layer summary table (one row per layer)
  layer_summary <- tibble(
    layer        = layers_tbl$name,
    basis        = ifelse(layers_tbl$is_aggregate, "Aggregate", "Occurrence"),
    our_layer    = layers_tbl$is_our_layer,
    attachment_m = layers_tbl$attachment_m,
    limit_m      = layers_tbl$limit_m,
    expected_loss_m = colMeans(layer_mat),
    prob_hit        = colMeans(layer_mat > 0)
  )

  list(
    sim_losses = tibble(
      sir_eroded_m     = layer_mat[, 1],
      primary_loss_m   = if (length(other_idx) > 0) rowSums(layer_mat[, other_idx, drop = FALSE]) else rep(0, n),
      our_layer_loss_m = if (length(our_idx)   > 0) rowSums(layer_mat[, our_idx,   drop = FALSE]) else rep(0, n)
    ),
    layer_summary = layer_summary
  )
}

style_dt <- function(dt) {
  dt %>%
    formatStyle(
      columns = names(dt$x$data),
      fontSize = "13px"
    )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}
