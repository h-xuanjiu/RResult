# Standard result adapters: summaries, comparisons, survival and mixed models.

.rr_first_families <- c("summary.lm", "summary.glm", "anova", "TukeyHSD", "pairwise.htest", "power.htest",
  "survfit", "coxph", "survreg", "merMod", "lme", "emmGrid", "summary_emm")

rr_first_batch_kind <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  for (kind in c("summary.lm", "summary.glm", "pairwise.htest", "power.htest", "coxph", "survreg", "lme"))
    if (identical(cls, kind)) return(kind)
  if (identical(cls, c("anova", "data.frame"))) return("anova")
  if (identical(cls, c("TukeyHSD", "multicomp")) || identical(cls, c("multicomp", "TukeyHSD"))) return("TukeyHSD")
  if (identical(cls, "survfit") || identical(cls, c("survfitcox", "survfit"))) return("survfit")
  if (identical(cls, c("summary_emm", "data.frame"))) return("summary_emm")
  if (isS4(x) && length(cls) == 1L) {
    definition <- methods::getClassDef(as.character(cls))
    if (!is.null(definition)) {
      if (as.character(cls) %in% c("lmerMod", "glmerMod", "lmerModLmerTest") &&
          definition@package %in% c("lme4", "lmerTest")) return("merMod")
      if (as.character(cls) == "emmGrid" && definition@package == "emmeans") return("emmGrid")
    }
  }
  NULL
}

rr_require <- function(package, min_version = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) stop("This report requires the optional package '", package, "'.", call. = FALSE)
  if (!is.null(min_version) && utils::packageVersion(package) < min_version)
    stop("This report requires '", package, "' >= ", min_version, ".", call. = FALSE)
}

rr_coefficient_frame <- function(m, limits, df = Inf, estimate = 1L, se = 2L, statistic = 3L, p = 4L) {
  idx <- seq_len(nrow(m))
  get <- function(j) if (length(j) && !is.na(j) && j <= ncol(m)) as.numeric(m[idx, j]) else rep(NA_real_, length(idx))
  degrees <- rep(df, length.out = nrow(m))[idx]
  critical <- rep(stats::qnorm(.975), length(idx))
  finite <- is.finite(degrees) & degrees > 0
  critical[finite] <- stats::qt(.975, degrees[finite])
  critical[is.na(degrees) | degrees <= 0] <- NA_real_
  value <- get(estimate); error <- get(se)
  data.frame(term = rownames(m)[idx], estimate = value, std.error = error,
    statistic = get(statistic), p.value = get(p), df = degrees,
    conf.low = value - critical * error, conf.high = value + critical * error, stringsAsFactors = FALSE)
}

rr_summary_coefficients <- function(x, limits) {
  is_lm <- identical(rr_report_kind(x), "summary.lm")
  use_t <- is_lm || grepl("t", colnames(x$coefficients)[4L], fixed = TRUE)
  co <- rr_coefficient_frame(x$coefficients, limits, df = if (use_t) x$df[2L] else Inf)
  aliased <- names(x$aliased)[x$aliased]
  if (length(aliased)) {
    missing <- co[rep(NA_integer_, length(aliased)), , drop = FALSE]
    missing$term <- aliased
    co <- rbind(co, missing)
  }
  rownames(co) <- NULL
  co
}

rr_comparisons <- function(x, kind, limits) {
  if (kind == "TukeyHSD") {
    pieces <- rr_plain(x)
    result <- list()
    for (nm in names(pieces)) {
      m <- pieces[[nm]]; idx <- seq_len(nrow(m))
      if (!length(idx)) next
      result[[length(result) + 1L]] <- data.frame(family = nm, contrast = rownames(m)[idx],
        estimate = m[idx, "diff"], conf.low = m[idx, "lwr"], conf.high = m[idx, "upr"],
        p.value = m[idx, "p adj"], row.names = NULL)
    }
    return(if (length(result)) do.call(rbind, result) else data.frame())
  }
  m <- x$p.value
  i <- which(!is.na(m), arr.ind = TRUE)
  data.frame(group1 = rownames(m)[i[, 1L]], group2 = colnames(m)[i[, 2L]],
    p.value = as.numeric(m[i]))
}

rr_anova_frame <- function(x, limits) {
  d <- as.data.frame(rr_plain(x), optional = TRUE, stringsAsFactors = FALSE)
  labels <- attr(x, "row.names", exact = TRUE)
  labels <- if (is.character(labels)) labels[seq_len(nrow(d))] else as.character(seq_len(nrow(d)))
  data.frame(term = labels, d, row.names = NULL, check.names = FALSE)
}

rr_survival_data <- function(x, limits, complete = FALSE) {
  if (!length(x$time)) stop("The survival object has no stored time points.", call. = FALSE)
  sizes <- if (length(x$strata)) as.integer(x$strata) else length(x$time)
  labels <- if (length(x$strata)) names(x$strata) else "All observations"
  if (is.null(labels)) labels <- paste("Stratum", seq_along(sizes))
  starts <- cumsum(c(1, utils::head(sizes, -1L)))
  curves <- if (is.matrix(x$surv)) ncol(x$surv) else 1L
  count <- if (complete) length(sizes) * curves else min(length(sizes) * curves, limits$max_children, limits$max_plot_points)
  budget <- max(1L, floor(limits$max_plot_points / max(1L, count)))
  result <- list()
  for (j in seq_len(curves)) for (g in seq_along(sizes)) {
    if (length(result) >= count) break
    if (!sizes[g]) next
    idx <- starts[g] - 1L + if (complete) seq_len(sizes[g]) else rr_sample_indices(sizes[g], budget)
    pick <- function(v) {
      if (is.null(v)) return(rep(NA_real_, length(idx)))
      if (is.matrix(v)) v[idx, min(j, ncol(v))] else v[idx]
    }
    label <- if (curves == 1L) labels[g] else paste(labels[g], "| curve", j)
    result[[length(result) + 1L]] <- data.frame(time = x$time[idx], survival = pick(x$surv),
      lower = pick(x$lower), upper = pick(x$upper), cumhaz = pick(x$cumhaz),
      n.risk = pick(x$n.risk), n.event = pick(x$n.event), n.censor = pick(x$n.censor),
      stratum = label, stringsAsFactors = FALSE)
  }
  if (!length(result)) stop("No stored survival curves available.", call. = FALSE)
  d <- do.call(rbind, result)
  d$stratum <- factor(d$stratum, levels = unique(d$stratum))
  d
}

rr_survival_coefficients <- function(x, kind, limits) {
  rr_require("survival")
  sm <- if (kind == "coxph") utils::getFromNamespace("summary.coxph", "survival")(x) else
    utils::getFromNamespace("summary.survreg", "survival")(x)
  if (kind == "coxph") {
    m <- sm$coefficients
    if (is.null(m)) m <- matrix(numeric(), 0L, 5L)
    co <- rr_coefficient_frame(m, limits, se = if ("robust se" %in% colnames(m)) match("robust se", colnames(m)) else 3L,
      statistic = match("z", colnames(m)), p = match("Pr(>|z|)", colnames(m)))
    co$hazard_ratio <- exp(co$estimate)
  } else co <- rr_coefficient_frame(sm$table, limits)
  list(summary = sm, coefficients = co)
}

rr_mixed_result <- function(x, kind, limits) {
  if (kind == "merMod") {
    rr_require("lme4")
    sm <- summary(x)
    m <- sm$coefficients
    df <- if ("df" %in% colnames(m)) m[, "df"] else Inf
    co <- rr_coefficient_frame(m, limits, df = df,
      statistic = match(TRUE, grepl("[tz] value", colnames(m))), p = match(TRUE, grepl("^Pr", colnames(m))))
    vc <- as.data.frame(lme4::VarCorr(x))
    variance <- data.frame(group = vc$grp, term = vc$var1, covariance_term = vc$var2, variance = vc$vcov, sd_or_correlation = vc$sdcor)
    groups <- lme4::ngrps(x)
    fitted <- stats::fitted(x); residual <- stats::residuals(x)
    note <- c("Fixed-effect intervals use t quantiles when denominator degrees of freedom are available, otherwise a normal Wald approximation.",
      "A missing p value is retained as unavailable; lme4 linear mixed models do not supply denominator-df p values by default.",
      if (lme4::isSingular(x)) "The fitted random-effects covariance is singular; inspect the model specification.",
      unlist(sm$optinfo$conv$lme4$messages, use.names = FALSE))
  } else {
    rr_require("nlme")
    sm <- summary(x); m <- sm$tTable
    co <- rr_coefficient_frame(m, limits, df = m[, "DF"], statistic = 4L, p = 5L)
    # pdMatrix retains full precision and grouping names, including nested and
    # blocked structures; VarCorr.lme is a rounded character display table.
    vc <- nlme::pdMatrix(x$modelStruct$reStruct)
    components <- lapply(seq_along(vc), function(i) {
      relative <- vc[[i]]
      value <- relative * x$sigma^2
      correlation <- stats::cov2cor(relative)
      n <- nrow(value); nm <- rownames(value); off <- lower.tri(value)
      data.frame(group = names(vc)[i], term = nm[c(seq_len(n), col(value)[off])],
        covariance_term = c(rep(NA_character_, n), nm[row(value)[off]]),
        variance = c(diag(value), value[off]),
        sd_or_correlation = c(sqrt(diag(value)), correlation[off]), stringsAsFactors = FALSE)
    })
    variance <- do.call(rbind, c(components, list(data.frame(group = "Residual", term = NA_character_,
      covariance_term = NA_character_, variance = x$sigma^2, sd_or_correlation = x$sigma))))
    rownames(variance) <- NULL
    groups <- vapply(x$groups, function(v) if (is.factor(v)) nlevels(v) else length(unique(v)), integer(1))
    fitted <- x$fitted[, ncol(x$fitted)]; residual <- x$residuals[, ncol(x$residuals)]
    note <- "Fixed-effect intervals use the saved denominator degrees of freedom from summary.lme. Diagnostics are conditional on the fitted random effects."
  }
  idx <- rr_sample_indices(length(fitted), limits$max_plot_points)
  list(coefficients = co, variance = variance,
    groups = data.frame(group = names(groups), levels = as.numeric(groups)),
    diagnostics = data.frame(fitted = as.numeric(fitted[idx]), residual = as.numeric(residual[idx])),
    n = stats::nobs(x), notes = note)
}

rr_emmeans_frame <- function(x, kind, limits) {
  if (kind == "emmGrid") {
    rr_require("emmeans")
    grid <- methods::slot(x, "grid"); linfct <- methods::slot(x, "linfct")
    if (nrow(grid) > limits$max_plot_points || as.double(nrow(linfct)) * ncol(linfct) > limits$max_cells)
      stop("This emmeans grid exceeds the computation limit. Subset the reference grid in R or increase max_cells/max_plot_points; check the resulting comparison family.", call. = FALSE)
    x <- utils::getFromNamespace("summary.emmGrid", "emmeans")(x, infer = c(TRUE, TRUE))
  }
  idx <- seq_len(rr_df_nrow(x))
  d <- as.data.frame(rr_plain(x), optional = TRUE, stringsAsFactors = FALSE)[idx, , drop = FALSE]
  estimate <- attr(x, "estName", exact = TRUE)
  if (is.null(estimate)) estimate <- intersect(c("emmean", "estimate", "response", "prob", "rate"), names(d))[1L]
  labels <- intersect(c(attr(x, "by.vars", exact = TRUE), attr(x, "pri.vars", exact = TRUE)), names(d))
  if (!length(labels)) labels <- setdiff(names(d), c(estimate, "SE", "df", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL", "t.ratio", "z.ratio", "p.value", "null"))
  term <- if (length(labels)) vapply(seq_len(nrow(d)), function(i) paste(paste(labels, vapply(d[labels], function(v) as.character(v[i]), character(1)), sep = "="), collapse = " | "), character(1)) else as.character(idx)
  get <- function(candidates) { col <- intersect(candidates, names(d)); if (length(col)) as.numeric(d[[col[1L]]]) else rep(NA_real_, nrow(d)) }
  co <- data.frame(term = term, estimate = get(estimate), std.error = get("SE"),
    conf.low = get(c("lower.CL", "asymp.LCL")), conf.high = get(c("upper.CL", "asymp.UCL")), p.value = get("p.value"))
  list(data = d, coefficients = co, estimate = estimate, adjust = attr(x, "adjust", exact = TRUE),
    scale = attr(x, "type", exact = TRUE), notes = attr(x, "mesg", exact = TRUE), n = rr_df_nrow(x))
}

rr_first_batch_report <- function(x, kind, limits) {
  formula <- function() paste(deparse(x$call), collapse = " ")
  if (kind %in% c("summary.lm", "summary.glm")) {
    lm <- kind == "summary.lm"
    residual <- if (lm) x$residuals else x$deviance.resid
    co <- rr_summary_coefficients(x, limits)
    metrics <- if (lm) list("R-squared" = x$r.squared, "Adjusted R-squared" = x$adj.r.squared,
      "Residual standard error" = x$sigma, "Residual df" = x$df[2L]) else
      list(Family = x$family$family, Link = x$family$link, Dispersion = x$dispersion, "Residual df" = x$df.residual)
    return(rr_new_report(kind, "Read a saved model summary", formula(), metrics,
      "Coefficient estimates and reference distributions come from the supplied summary object.",
      list(Coefficients = co), c("Coefficient intervals" = "coefficients", if (length(residual)) c("Stored residual distribution" = "residual_distribution", "Stored residual Q-Q" = "residual_qq")),
      c("A summary does not retain all fitted values or the original data. Fit-versus-response and influence plots require the full model.",
        if (lm) "Stored residuals may be weighted. Intervals use the saved residual degrees of freedom." else "Intervals use the saved t or normal reference distribution on the link scale."), limits))
  }
  if (kind == "anova") {
    d <- rr_anova_frame(x, limits)
    p <- names(d)[grepl("^Pr\\(", names(d))]
    ss <- intersect(c("Sum Sq", "Sum of Sq", "Deviance"), names(d))
    plots <- c(if (length(p)) c("Saved test P values" = "p_values"), if (length(ss)) c("Stored variation / deviance" = "variation"))
    return(rr_new_report(kind, "Inspect the saved analysis table", paste(attr(x, "heading", exact = TRUE), collapse = " "),
      list(Rows = rr_df_nrow(x), Columns = length(rr_plain(x)), "P-value columns" = length(p)),
      "The table retains the original test column names and model-comparison ordering.", list(ANOVA = d), plots,
      "Sequential ANOVA, deviance tests and nested-model comparisons have different meanings. No universal effect size is inferred from an arbitrary anova table.", limits))
  }
  if (kind %in% c("TukeyHSD", "pairwise.htest")) {
    d <- rr_comparisons(x, kind, limits)
    tukey <- kind == "TukeyHSD"
    adjustment <- if (tukey) "Tukey family-wise" else x$p.adjust.method
    return(rr_new_report(kind, if (tukey) "Compare groups with simultaneous intervals" else "Inspect adjusted pairwise tests",
      if (tukey) "Saved Tukey HSD contrasts" else x$method,
      list("Displayed comparisons" = nrow(d), Adjustment = adjustment,
        "Smallest displayed P" = if (nrow(d) && any(is.finite(d$p.value))) min(d$p.value, na.rm = TRUE) else NA_real_),
      "Adjusted P values are taken from the saved result, without a second adjustment.", list(Comparisons = d),
      if (tukey) c("Simultaneous contrast intervals" = "contrasts", "Adjusted P values" = "p_values") else
        c("Adjusted P-value heatmap" = "pairwise_heatmap", "Adjusted P values" = "p_values"),
      c(if (tukey) paste("Stored simultaneous confidence level:", attr(x, "conf.level", exact = TRUE)) else
        "Only stored pairwise tests are shown. Missing cells do not represent non-significant comparisons.",
        "All saved comparisons are retained; the adjustment belongs to the original comparison family."), limits))
  }
  if (kind == "power.htest") {
    params <- rr_plain(x)
    params <- params[vapply(params, function(v) is.atomic(v) && length(v) == 1L, logical(1))]
    d <- data.frame(parameter = names(params), value = vapply(params, as.character, character(1)))
    return(rr_new_report(kind, x$method, "Saved power-analysis parameters",
      list(Power = if (is.null(x$power)) NA_real_ else x$power, "Significance level" = if (is.null(x$sig.level)) NA_real_ else x$sig.level,
        "Sample size parameter" = if (is.null(x$n)) NA_real_ else x$n),
      if (length(x$note)) x$note else "Sample size and effect definitions follow the original method.",
      list(Parameters = d), if (length(x$power)) c("Calculated power" = "power") else character(),
      "This is the saved calculation. A power-versus-sample-size curve is not invented from a single result.", limits))
  }
  if (kind == "survfit") {
    d <- rr_survival_data(x, limits, complete = TRUE)
    return(rr_new_report(kind, "Follow survival over time", paste(deparse(x$call), collapse = " "),
      list(Observations = sum(x$n), "Displayed curves" = nlevels(d$stratum), "Stored time points" = length(x$time),
        "Confidence level" = if (length(x$conf.int)) x$conf.int else NA_real_),
      "Survival probabilities, censor counts and risk counts come from the fitted survival object.",
      list(Curves = d), c("Survival curves" = "survival", "Cumulative event probability" = "cumulative_event",
        if (any(is.finite(d$cumhaz))) c("Stored cumulative hazard" = "cumulative_hazard"), "Risk-count snapshots" = "risk_table"),
      c("Confidence bands use the stored confidence level and transformation. Censor marks indicate stored times with censoring.",
        "Curves and tables retain all saved steps, strata and censor times. Risk snapshots label selected exact time points.",
        "Multi-state survfitms objects require a separate adapter and retain generic exploration."), limits))
  }
  if (kind %in% c("coxph", "survreg")) {
    r <- rr_survival_coefficients(x, kind, limits); sm <- r$summary
    cox <- kind == "coxph"
    metrics <- if (cox) list(Observations = sm$n, Events = sm$nevent, Concordance = unname(sm$concordance[1L]),
      "Likelihood-ratio P" = unname(sm$logtest["pvalue"])) else list(Observations = sm$n, Distribution = x$dist,
      "Log-likelihood" = utils::tail(x$loglik, 1L), Scale = paste(rr_fmt(x$scale), collapse = ", "))
    return(rr_new_report(kind, if (cox) "Read the Cox proportional-hazards model" else "Read a parametric survival model", formula(), metrics,
      if (cox) "Hazard ratios describe conditional relative hazards under the fitted model." else "Coefficients follow the fitted distribution's response transformation.",
      list(Coefficients = r$coefficients),
      c(if (cox) c("Hazard ratios" = "hazard_ratios"), "Coefficient intervals" = "coefficients",
        if (length(x$linear.predictors)) c("Linear predictor distribution" = "linear_predictor")),
      c("Intervals are 95% Wald intervals, using the fitted robust covariance when present.",
        if (cox) "The report does not infer proportional-hazards validity or automatically refit survival curves." else
          "Accelerated-failure-time coefficients are not hazard ratios. Censored times are not treated as fully observed outcomes."), limits))
  }
  if (kind %in% c("merMod", "lme")) {
    r <- rr_mixed_result(x, kind, limits)
    return(rr_new_report(kind, "Inspect fixed and random effects", paste(deparse(stats::formula(x)), collapse = " "),
      list(Observations = r$n, "Fixed terms" = nrow(r$coefficients), "Grouping factors" = nrow(r$groups), "Variance rows" = nrow(r$variance)),
      "Fixed-effect estimates and random-effect variance components have separate views.",
      list(Coefficients = r$coefficients, Variance = r$variance, Groups = r$groups),
      c("Fixed-effect intervals" = "coefficients", "Variance components" = "random_variance", "Conditional residuals" = "mixed_residuals", "Residual Q-Q" = "mixed_qq"),
      c(r$notes, "Random-effect covariance entries are retained in the table; variance charts show diagonal variances only, on their fitted scales."), limits))
  }
  if (kind %in% c("emmGrid", "summary_emm")) {
    r <- rr_emmeans_frame(x, kind, limits)
    return(rr_new_report(kind, "Compare estimated marginal means and contrasts", "Reference-grid estimates on the requested summary scale",
      list("Summary rows" = r$n, "Estimate column" = r$estimate, Adjustment = if (length(r$adjust)) r$adjust else "Saved summary",
        Scale = if (length(r$scale)) r$scale else "Saved summary"),
      "Intervals and tests retain the summary's scale and multiplicity adjustment.",
      list(Estimates = r$data), c("Estimates and intervals" = "marginal_means", if (any(is.finite(r$coefficients$p.value))) c("Saved P values" = "p_values")),
      c(r$notes, "Non-estimable rows remain unavailable. A supplied summary_emm is never re-summarized or assigned new confidence intervals."), limits))
  }
  stop("Unknown standard result family.", call. = FALSE)
}
