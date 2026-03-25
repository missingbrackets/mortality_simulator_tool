library(dplyr)

safe_p <- function(p) {
  pmin(pmax(p, 1e-8), 1 - 1e-8)
}

severity_prob_table <- function(severity_tbl) {
  if (!"loss_m" %in% names(severity_tbl)) {
    stop("Severity table must contain loss_m")
  }

  # Use the original fitting basis, not always claim-based
  if ("fit_return_period" %in% names(severity_tbl)) {
    rp <- severity_tbl$fit_return_period
  } else if ("return_period_claims" %in% names(severity_tbl)) {
    rp <- severity_tbl$return_period_claims
  } else if ("return_period_years" %in% names(severity_tbl)) {
    rp <- severity_tbl$return_period_years
  } else {
    stop("Severity table must contain fit_return_period, return_period_claims, or return_period_years")
  }

  severity_tbl %>%
    mutate(
      fit_return_period = as.numeric(rp),
      exceed_prob_fit = pmin(pmax(1 / fit_return_period, 1e-8), 0.999999),
      percentile = safe_p(1 - exceed_prob_fit)
    ) %>%
    arrange(percentile)
}

interp_percentile_from_loss <- function(loss, df) {
  x <- df$loss_m
  y <- df$percentile

  ord <- order(x)
  x <- x[ord]
  y <- y[ord]

  if (length(unique(x)) < 2) {
    return(rep(mean(y), length(loss)))
  }

  approx(x = x, y = y, xout = loss, method = "linear", rule = 2)$y
}

fit_lognormal_quantile <- function(df) {
  z <- qnorm(df$percentile)
  y <- log(df$loss_m)
  model <- lm(y ~ z)
  list(
    meanlog = unname(coef(model)[1]),
    sdlog = max(unname(coef(model)[2]), 1e-6)
  )
}

fit_weibull_quantile <- function(df) {
  z <- log(-log(1 - df$percentile))
  y <- log(df$loss_m)
  model <- lm(y ~ z)
  list(
    shape = max(1 / unname(coef(model)[2]), 1e-6),
    scale = exp(unname(coef(model)[1]))
  )
}

fit_gamma_quantile <- function(df) {
  objective <- function(theta) {
    shape <- exp(theta[1])
    scale <- exp(theta[2])
    fitted <- qgamma(df$percentile, shape = shape, scale = scale)
    sum((log(fitted) - log(df$loss_m))^2)
  }

  opt <- optim(log(c(2, median(df$loss_m) / 2)), objective, method = "Nelder-Mead")
  list(shape = exp(opt$par[1]), scale = exp(opt$par[2]))
}

fit_gpd_quantile <- function(df, threshold = NULL) {
  if (is.null(threshold) || !is.finite(threshold)) {
    threshold <- min(df$loss_m) * 0.8
  }
  threshold <- min(threshold, min(df$loss_m) * 0.95)

  p_thresh <- interp_percentile_from_loss(threshold, df)
  exceed_thresh <- pmin(pmax(1 - p_thresh, 1e-6), 0.999999)

  objective <- function(theta) {
    xi <- theta[1]
    beta <- exp(theta[2])

    qfun <- function(p) {
      p <- safe_p(p)
      tail_prob <- pmin(pmax((1 - p) / exceed_thresh, 1e-12), 1)
      if (abs(xi) < 1e-8) {
        threshold - beta * log(tail_prob)
      } else {
        threshold + (beta / xi) * (tail_prob^(-xi) - 1)
      }
    }

    fitted <- qfun(df$percentile)
    if (any(!is.finite(fitted)) || any(fitted <= 0)) return(1e12)
    sum((log(fitted) - log(df$loss_m))^2)
  }

  opt <- optim(c(0.1, log(max(median(df$loss_m - threshold), 0.1))), objective, method = "Nelder-Mead")
  list(xi = opt$par[1], beta = exp(opt$par[2]), threshold = threshold)
}

build_lognormal_fit <- function(params) {
  meanlog <- as.numeric(params$meanlog)
  sdlog <- max(as.numeric(params$sdlog), 1e-8)
  list(
    params = list(meanlog = meanlog, sdlog = sdlog),
    qfun = function(p) qlnorm(safe_p(p), meanlog = meanlog, sdlog = sdlog),
    pfun = function(x) plnorm(x, meanlog = meanlog, sdlog = sdlog),
    rfun = function(n) rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  )
}

build_weibull_fit <- function(params) {
  shape <- max(as.numeric(params$shape), 1e-8)
  scale <- max(as.numeric(params$scale), 1e-8)
  list(
    params = list(shape = shape, scale = scale),
    qfun = function(p) qweibull(safe_p(p), shape = shape, scale = scale),
    pfun = function(x) pweibull(x, shape = shape, scale = scale),
    rfun = function(n) rweibull(n, shape = shape, scale = scale)
  )
}

build_gamma_fit <- function(params) {
  shape <- max(as.numeric(params$shape), 1e-8)
  scale <- max(as.numeric(params$scale), 1e-8)
  list(
    params = list(shape = shape, scale = scale),
    qfun = function(p) qgamma(safe_p(p), shape = shape, scale = scale),
    pfun = function(x) pgamma(x, shape = shape, scale = scale),
    rfun = function(n) rgamma(n, shape = shape, scale = scale)
  )
}

build_gpd_fit <- function(params, df) {
  xi <- as.numeric(params$xi)
  beta <- max(as.numeric(params$beta), 1e-8)
  threshold <- as.numeric(params$threshold)
  threshold <- min(threshold, min(df$loss_m) * 0.95)

  p_thresh <- interp_percentile_from_loss(threshold, df)
  exceed_thresh <- pmin(pmax(1 - p_thresh, 1e-6), 0.999999)

  qfun <- function(p) {
    p <- safe_p(p)
    tail_prob <- pmin(pmax((1 - p) / exceed_thresh, 1e-12), 1)
    if (abs(xi) < 1e-8) {
      threshold - beta * log(tail_prob)
    } else {
      threshold + (beta / xi) * (tail_prob^(-xi) - 1)
    }
  }

  pfun <- function(x) {
    x <- pmax(x, threshold)
    if (abs(xi) < 1e-8) {
      1 - exceed_thresh * exp(-(x - threshold) / beta)
    } else {
      inner <- pmax(1 + xi * (x - threshold) / beta, 1e-12)
      1 - exceed_thresh * inner^(-1 / xi)
    }
  }

  rfun <- function(n) {
    u <- runif(n, min = p_thresh, max = 1 - 1e-8)
    qfun(u)
  }

  list(
    params = list(xi = xi, beta = beta, threshold = threshold),
    qfun = qfun,
    pfun = pfun,
    rfun = rfun
  )
}

fit_severity_curve <- function(severity_tbl, distribution = c("lognormal", "weibull", "gamma", "gpd"), params_override = list()) {
  distribution <- match.arg(distribution)
  df <- severity_prob_table(severity_tbl)

  base_params <- switch(
    distribution,
    lognormal = fit_lognormal_quantile(df),
    weibull = fit_weibull_quantile(df),
    gamma = fit_gamma_quantile(df),
    gpd = fit_gpd_quantile(df)
  )

  if (length(params_override) > 0) {
    for (nm in names(params_override)) {
      if (!is.null(params_override[[nm]]) && is.finite(params_override[[nm]])) {
        base_params[[nm]] <- as.numeric(params_override[[nm]])
      }
    }
  }

  built <- switch(
    distribution,
    lognormal = build_lognormal_fit(base_params),
    weibull = build_weibull_fit(base_params),
    gamma = build_gamma_fit(base_params),
    gpd = build_gpd_fit(base_params, df)
  )

  comparison_tbl <- compare_fit_to_input(
    fit = list(params = built$params, qfun = built$qfun, pfun = built$pfun),
    severity_tbl = df
  )

  list(
    distribution = distribution,
    params = built$params,
    qfun = built$qfun,
    pfun = built$pfun,
    rfun = built$rfun,
    comparison_table = comparison_tbl
  )
}

compare_fit_to_input <- function(fit, severity_tbl) {
  input_df <- severity_prob_table(severity_tbl) %>%
    mutate(source = "Input")

  extra_percentiles <- c(0.25, 0.50, 0.60, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99, 0.995, 0.999, 0.9995)
  all_percentiles <- sort(unique(c(input_df$percentile, extra_percentiles)))

  annual_freq <- if ("annual_claim_frequency" %in% names(input_df)) input_df$annual_claim_frequency[[1]] else NA_real_

  padded_df <- data.frame(percentile = all_percentiles, stringsAsFactors = FALSE) %>%
    mutate(
      source = ifelse(percentile %in% input_df$percentile, "Input", "Padded"),
      return_period_claims = 1 / pmax(1 - percentile, 1e-10),
      return_period_years = if (is.na(annual_freq)) NA_real_ else return_period_claims / annual_freq,
      input_exceed_prob = 1 - percentile,
      loss_m = fit$qfun(percentile),
      fitted_percentile = percentile,
      fitted_exceed_prob = 1 - percentile,
      fitted_loss_at_input_percentile_m = fit$qfun(percentile),
      loss_error_pct = NA_real_
    )

  match_idx <- match(input_df$percentile, padded_df$percentile)
  padded_df$loss_m[match_idx] <- input_df$loss_m

  if ("return_period_claims" %in% names(input_df)) {
    padded_df$return_period_claims[match_idx] <- input_df$return_period_claims
  }
  if ("return_period_years" %in% names(input_df)) {
    padded_df$return_period_years[match_idx] <- input_df$return_period_years
  }

  padded_df$input_exceed_prob[match_idx] <- input_df$exceed_prob_fit
  padded_df$fitted_percentile[match_idx] <- fit$pfun(input_df$loss_m)
  padded_df$fitted_exceed_prob[match_idx] <- 1 - fit$pfun(input_df$loss_m)
  padded_df$fitted_loss_at_input_percentile_m[match_idx] <- fit$qfun(input_df$percentile)
  padded_df$loss_error_pct[match_idx] <- fit$qfun(input_df$percentile) / input_df$loss_m - 1

  padded_df %>%
    arrange(percentile)
}