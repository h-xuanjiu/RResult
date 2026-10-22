# Graphics for the first-batch standard result families.

rr_interval_plot <- function(d, title, xlab = "Estimate and stored interval", reference = NULL, show_ci = TRUE) {
  d <- d[is.finite(d$estimate), , drop = FALSE]
  if (!nrow(d)) stop("No finite estimable terms are available for this chart.", call. = FALSE)
  if (anyDuplicated(d$term)) d$term <- paste0(seq_len(nrow(d)), ": ", d$term)
  d$term <- factor(d$term, levels = rev(unique(d$term)))
  p <- ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$term))
  if (length(reference)) p <- p + ggplot2::geom_vline(xintercept = reference, colour = "#9da9b6", linetype = 2)
  if (show_ci) {
    interval <- d[is.finite(d$conf.low) & is.finite(d$conf.high), , drop = FALSE]
    p <- p + ggplot2::geom_segment(data = interval,
      ggplot2::aes(x = .data$conf.low, xend = .data$conf.high, yend = .data$term), colour = "#187f88", linewidth = .9)
  }
  p + ggplot2::geom_point(colour = "#187f88", size = 3) + ggplot2::labs(title = title, x = xlab, y = NULL)
}

rr_saved_p_plot <- function(d, labels, title) {
  keep <- is.finite(d$p.value)
  labels <- labels[keep]; d <- d[keep, , drop = FALSE]
  if (!nrow(d)) stop("This selection has no saved finite P values.", call. = FALSE)
  d$label <- factor(make.unique(rr_text(labels)), levels = rev(make.unique(rr_text(labels))))
  d$score <- -log10(pmax(d$p.value, 1e-16))
  ggplot2::ggplot(d, ggplot2::aes(.data$score, .data$label)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$score, yend = .data$label), colour = "#b2d8db", linewidth = 1) +
    ggplot2::geom_point(colour = "#187f88", size = 3) +
    ggplot2::labs(title = title, x = "-log10(P); displayed scores capped at 16", y = NULL,
      subtitle = "P values and their original adjustment are retained in the result table")
}

rr_step_band <- function(d) {
  pieces <- lapply(split(d, d$stratum), function(s) {
    if (nrow(s) < 2L) return(s)
    idx <- rep(seq_len(nrow(s)), each = 2L)
    out <- s[idx[-length(idx)], , drop = FALSE]
    out$time <- s$time[idx[-1L]]
    out
  })
  do.call(rbind, pieces)
}

rr_first_batch_plot <- function(x, type, limits, include_intercept = FALSE, effect_scale = "link",
                                show_ci = TRUE, show_censor = TRUE) {
  kind <- rr_report_kind(x)
  for (value in list(show_ci, show_censor, include_intercept))
    if (!is.logical(value) || length(value) != 1L || is.na(value)) stop("Chart switches must be TRUE or FALSE.", call. = FALSE)
  allowed <- rr_first_batch_report(x, kind, limits)$plots
  if (!type %in% unname(allowed)) stop("This chart is unavailable for the supplied result. Available: ", paste(unname(allowed), collapse = ", "), call. = FALSE)
  if (kind %in% c("summary.lm", "summary.glm", "coxph", "survreg", "merMod", "lme") && type %in% c("coefficients", "hazard_ratios")) {
    co <- if (kind %in% c("summary.lm", "summary.glm")) rr_summary_coefficients(x, limits) else
      if (kind %in% c("coxph", "survreg")) rr_survival_coefficients(x, kind, limits)$coefficients else
        rr_mixed_result(x, kind, limits)$coefficients
    if (!include_intercept && sum(is.finite(co$estimate)) > 1L) co <- co[co$term != "(Intercept)", , drop = FALSE]
    ratio <- type == "hazard_ratios" || effect_scale == "ratio"
    label <- "Estimate and 95% interval"
    if (ratio) {
      valid_glm <- kind == "summary.glm" && ((x$family$family == "binomial" && x$family$link == "logit") ||
        (x$family$family == "poisson" && x$family$link == "log"))
      if (kind != "coxph" && !valid_glm) stop("Ratio scale requires Cox hazards, binomial/logit or Poisson/log coefficients.", call. = FALSE)
      co[c("estimate", "conf.low", "conf.high")] <- lapply(co[c("estimate", "conf.low", "conf.high")], exp)
      label <- if (kind == "coxph") "Hazard ratio and 95% Wald interval" else "Ratio and 95% Wald interval"
    }
    p <- rr_interval_plot(co, if (ratio) "Relative effects and uncertainty" else "Effect estimates and uncertainty",
      label, if (ratio) 1 else 0, show_ci)
    if (ratio) p <- p + ggplot2::scale_x_log10()
  } else if (kind %in% c("summary.lm", "summary.glm") || type == "linear_predictor") {
    value <- if (type == "linear_predictor") x$linear.predictors else if (kind == "summary.lm") x$residuals else x$deviance.resid
    idx <- rr_sample_indices(length(value), limits$max_plot_points)
    d <- data.frame(value = as.numeric(value[idx])); d <- d[is.finite(d$value), , drop = FALSE]
    if (!nrow(d)) stop("No finite stored values available.", call. = FALSE)
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$value)) +
      ggplot2::geom_histogram(bins = 25, fill = "#187f88", colour = "white") +
      ggplot2::labs(title = if (type == "linear_predictor") "Stored linear predictors" else "Stored residual distribution", x = "Value", y = "Count")
    if (type == "residual_qq") p <- ggplot2::ggplot(d, ggplot2::aes(sample = .data$value)) +
      ggplot2::stat_qq(colour = "#187f88") + ggplot2::stat_qq_line(colour = "#9da9b6") +
      ggplot2::labs(title = "Stored residual Q-Q", x = "Theoretical quantile", y = "Residual")
  } else if (kind == "anova") {
    d <- rr_anova_frame(x, limits)
    if (type == "p_values") {
      col <- names(d)[grepl("^Pr\\(", names(d))][1L]
      d$p.value <- d[[col]]
      p <- rr_saved_p_plot(d, d$term, paste("Saved analysis tests:", col))
    } else {
      col <- intersect(c("Sum Sq", "Sum of Sq", "Deviance"), names(d))[1L]
      d$value <- d[[col]]; d <- d[is.finite(d$value), , drop = FALSE]
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$value, stats::reorder(.data$term, .data$value))) +
        ggplot2::geom_col(fill = "#187f88", width = .65) + ggplot2::labs(title = "Stored analysis quantities", x = col, y = NULL)
    }
  } else if (kind %in% c("TukeyHSD", "pairwise.htest")) {
    d <- rr_comparisons(x, kind, limits)
    labels <- if (kind == "TukeyHSD") paste(d$family, d$contrast, sep = ": ") else paste(d$group1, "vs", d$group2)
    if (type == "p_values") p <- rr_saved_p_plot(d, labels, "Adjusted pairwise P values") else if (type == "contrasts") {
      d$term <- labels
      p <- rr_interval_plot(d, "Tukey simultaneous contrast intervals", "Difference and stored simultaneous interval", 0, show_ci)
    } else {
      if (!nrow(d)) stop("No saved pairwise comparisons in this window.", call. = FALSE)
      d$score <- -log10(pmax(d$p.value, 1e-16))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$group2, .data$group1, fill = .data$score)) +
        ggplot2::geom_tile(colour = "white") +
        ggplot2::geom_text(ggplot2::aes(label = format.pval(.data$p.value, digits = 2)), size = 3) +
        ggplot2::scale_fill_gradient(low = "#edf6f6", high = "#187f88", limits = c(0, 16)) +
        ggplot2::labs(title = "Saved adjusted pairwise tests", subtitle = paste("Adjustment:", x$p.adjust.method),
          x = NULL, y = NULL, fill = "-log10(P)")
    }
  } else if (kind == "power.htest") {
    d <- data.frame(label = "Power", power = x$power)
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$power, .data$label)) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 1, yend = .data$label), colour = "#e6edf1", linewidth = 16) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$power, yend = .data$label), colour = "#187f88", linewidth = 16) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", 100 * .data$power)), vjust = -1.6, size = 5) +
      ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
      ggplot2::labs(title = "Calculated power", subtitle = x$method, x = "Probability", y = NULL)
  } else if (kind == "survfit") {
    d <- rr_survival_data(x, limits, complete = TRUE)
    if (type == "risk_table") {
      snapshots <- lapply(split(d, d$stratum), function(g) g[rr_sample_indices(nrow(g), min(7L, nrow(g))), , drop = FALSE])
      d <- do.call(rbind, snapshots)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$time, .data$stratum, colour = .data$stratum)) +
        ggplot2::geom_text(ggplot2::aes(label = .data$n.risk), size = 4) +
        ggplot2::labs(title = "Number at risk at saved time points", subtitle = "Each label is the stored risk count immediately before its exact time point", x = "Time", y = NULL, colour = NULL)
    } else {
      d$value <- switch(type, survival = d$survival, cumulative_event = 1 - d$survival, cumulative_hazard = d$cumhaz)
      if (type == "cumulative_event") {
        lower <- 1 - d$upper; d$upper <- 1 - d$lower; d$lower <- lower
      }
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$time, .data$value, colour = .data$stratum, group = .data$stratum))
      if (show_ci && type != "cumulative_hazard" && any(is.finite(d$lower) & is.finite(d$upper))) {
        band <- rr_step_band(d)
        band <- band[is.finite(band$lower) & is.finite(band$upper), , drop = FALSE]
        p <- p + ggplot2::geom_ribbon(data = band, ggplot2::aes(ymin = .data$lower, ymax = .data$upper, fill = .data$stratum), colour = NA, alpha = .14)
        p <- p + ggplot2::labs(fill = "Curve")
      }
      p <- p + ggplot2::geom_step(linewidth = .8, na.rm = TRUE)
      if (show_censor) p <- p + ggplot2::geom_point(data = d[!is.na(d$n.censor) & d$n.censor > 0, , drop = FALSE], ggplot2::aes(shape = I(3)), size = 2, na.rm = TRUE)
      p <- p + ggplot2::labs(title = switch(type, survival = "Survival curves", cumulative_event = "Cumulative event probability", cumulative_hazard = "Stored cumulative hazard"),
        subtitle = paste(nrow(d), "saved curve points displayed; censor marks use stored counts"), x = "Time",
        y = if (type == "cumulative_hazard") "Cumulative hazard" else "Probability", colour = "Curve")
      if (type != "cumulative_hazard") p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
    }
  } else if (kind %in% c("merMod", "lme")) {
    r <- rr_mixed_result(x, kind, limits)
    if (type == "random_variance") {
      d <- r$variance[is.na(r$variance$covariance_term), , drop = FALSE]
      d$label <- paste(d$group, ifelse(is.na(d$term), "Residual", d$term), sep = ": ")
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$variance, stats::reorder(.data$label, .data$variance))) +
        ggplot2::geom_col(fill = "#187f88", width = .65) +
        ggplot2::labs(title = "Fitted variance components", subtitle = "Diagonal variances on their fitted scales; covariance entries remain in the table", x = "Variance", y = NULL)
    } else {
      d <- r$diagnostics
      d <- d[is.finite(d$fitted) & is.finite(d$residual), , drop = FALSE]
      p <- if (type == "mixed_qq") ggplot2::ggplot(d, ggplot2::aes(sample = .data$residual)) +
        ggplot2::stat_qq(colour = "#187f88") + ggplot2::stat_qq_line(colour = "#9da9b6") +
        ggplot2::labs(title = "Conditional residual Q-Q", x = "Theoretical quantile", y = "Residual") else
        ggplot2::ggplot(d, ggplot2::aes(.data$fitted, .data$residual)) + ggplot2::geom_point(colour = "#187f88", alpha = .65) +
          ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "#9da9b6") +
          ggplot2::labs(title = "Conditional residuals vs fitted", x = "Conditional fitted value", y = "Residual")
    }
  } else if (kind %in% c("emmGrid", "summary_emm")) {
    r <- rr_emmeans_frame(x, kind, limits)
    if (type == "p_values") p <- rr_saved_p_plot(r$coefficients, r$coefficients$term, "Saved marginal-mean / contrast tests") else
      p <- rr_interval_plot(r$coefficients, "Marginal estimates and uncertainty", paste(r$estimate, "on the saved summary scale"), show_ci = show_ci)
  }
  rr_plot_style(p)
}
