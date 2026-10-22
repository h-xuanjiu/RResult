# Structured reports for trusted R classes. Tables stay in R; the UI pages them.

rr_report_kind <- function(x) {
  if (rr_is_ggplot(x)) return("ggplot")
  bio <- rr_bio_kind(x)
  if (!is.null(bio)) return(bio)
  ordination <- rr_ordination_kind(x)
  if (!is.null(ordination)) return(ordination)
  extended <- rr_extended_kind(x)
  if (!is.null(extended)) return(extended)
  additional <- rr_first_batch_kind(x)
  if (!is.null(additional)) return(additional)
  cls <- attr(x, "class", exact = TRUE)
  for (kind in c("lm", "htest", "prcomp", "kmeans", "dist", "hclust", "density", "nls", "table")) if (identical(cls, kind)) return(kind)
  if (identical(cls, c("xtabs", "table"))) return("table")
  if (identical(cls, c("glm", "lm"))) return("glm")
  if (identical(cls, c("aov", "lm"))) return("aov")
  if (identical(cls, "ts") || identical(cls, c("mts", "ts", "matrix", "array")) ||
      identical(cls, c("mts", "ts", "matrix")) || identical(cls, c("mts", "ts"))) return("ts")
  if (identical(cls, "data.frame") || identical(cls, c("tbl_df", "tbl", "data.frame")) ||
      identical(cls, c("data.table", "data.frame")) || identical(cls, c("grouped_df", "tbl_df", "tbl", "data.frame")) ||
      identical(cls, c("rowwise_df", "tbl_df", "tbl", "data.frame"))) return("data_frame")
  NULL
}

rr_sample_indices <- function(n, cap) {
  if (!n) integer() else unique(as.integer(round(seq(1, n, length.out = min(n, cap)))))
}

rr_fmt <- function(x, digits = 4L) {
  if (!length(x)) return("Unavailable")
  if (is.numeric(x)) return(ifelse(is.na(x), "Unavailable", format(signif(x, digits), trim = TRUE)))
  rr_text(as.character(x), 500L)
}

rr_report_table <- function(data, limits) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data
}

rr_new_report <- function(kind, title, subtitle, metrics, highlights, tables, plots, notes, limits) {
  scope <- lapply(tables, function(data) attr(data, "rresult_scope", exact = TRUE))
  tables <- lapply(tables, rr_report_table, limits = limits)
  list(kind = kind, title = rr_text(title, 500L), subtitle = rr_text(subtitle, 1000L),
    metrics = metrics, highlights = rr_text(highlights, 1000L), tables = tables,
    plots = plots, notes = rr_text(notes, 1000L), table_scope = scope)
}

rr_glm <- function(x, limits) {
  # glm.nb estimates theta; MASS inference fixes the remaining GLM dispersion
  # at one rather than estimating a second dispersion from Pearson residuals.
  sm <- if (identical(attr(x, "class", exact = TRUE), c("negbin", "glm", "lm"))) {
    stats::summary.glm(x, dispersion = 1)
  } else stats::summary.glm(x)
  mat <- sm$coefficients
  critical <- if (grepl("t", colnames(mat)[4L], fixed = TRUE)) stats::qt(.975, x$df.residual) else stats::qnorm(.975)
  co <- data.frame(term = rownames(mat), estimate = mat[, 1L], std.error = mat[, 2L],
    statistic = mat[, 3L], p.value = mat[, 4L], conf.low = mat[, 1L] - critical * mat[, 2L],
    conf.high = mat[, 1L] + critical * mat[, 2L], row.names = NULL)
  missing <- names(x$coefficients)[is.na(x$coefficients)]
  if (length(missing)) co <- rbind(co, data.frame(term = missing, estimate = NA_real_, std.error = NA_real_,
    statistic = NA_real_, p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
  list(coefficients = co, dispersion = sm$dispersion,
    deviance_reduction = if (is.finite(x$null.deviance) && x$null.deviance > 0) 1 - x$deviance / x$null.deviance else NA_real_)
}

rr_data_profile <- function(x, limits) {
  plain <- rr_plain(x)
  rows <- rr_sample_indices(rr_df_nrow(x), limits$max_plot_points)
  cols <- seq_along(plain)
  profile <- lapply(cols, function(j) {
    v <- plain[[j]]
    simple <- rr_simple(v) && is.null(attr(v, "dim", exact = TRUE))
    val <- if (simple) rr_plain(v)[rows] else NULL
    data.frame(column = j, variable = names(plain)[j],
      type = paste(utils::head(class(v), 3L), collapse = "/"),
      missing = if (simple) sum(is.na(val)) else NA_integer_,
      missing_percent = if (simple && length(rows)) 100 * mean(is.na(val)) else NA_real_,
      distinct = if (simple) length(unique(val[!is.na(val)])) else NA_integer_,
      mean = if (simple && is.numeric(val) && !is.complex(val) && any(is.finite(val))) mean(val[is.finite(val)]) else NA_real_,
      stringsAsFactors = FALSE)
  })
  list(data = if (length(profile)) do.call(rbind, profile) else data.frame(), rows = rows, cols = cols)
}

rr_report <- function(x, limits, metadata = NULL, sample_id = NULL, assay = NULL, layer = NULL, bio_coef = NULL) {
  kind <- rr_report_kind(x)
  if (is.null(kind)) stop("No built-in report for this exact class. Use the object explorer or register an adapter.", call. = FALSE)
  if (kind %in% .rr_ordination_families) return(rr_ordination_report(x, kind, limits, metadata, sample_id))
  if (!is.null(metadata) || !is.null(sample_id)) stop("Sample metadata is currently supported for ordination reports only.", call. = FALSE)
  if (kind %in% .rr_bio_families) return(rr_bio_report(x, kind, limits, assay, layer, bio_coef))
  if (kind == "ggplot") return(rr_ggplot_report(x, limits))
  if (kind %in% .rr_extended_families) return(rr_extended_report(x, kind, limits))
  if (kind %in% .rr_first_families) return(rr_first_batch_report(x, kind, limits))
  if (kind %in% c("dist", "hclust", "density", "nls", "table")) return(rr_standard_report(x, kind, limits))
  formula <- function() paste(deparse(stats::formula(x), width.cutoff = 100L), collapse = " ")
  if (kind == "lm") {
    r <- rr_lm(x, limits)
    o <- r$overview
    return(rr_new_report(kind, "Your model, at a glance", o$formula,
      list(Observations = o$n, "R-squared" = o$r_squared, "Adjusted R-squared" = o$adjusted_r_squared,
        "Residual standard error" = o$residual_standard_error),
      c(paste0("The fitted model accounts for ", rr_fmt(100 * o$r_squared), "% of ",
          if (attr(x$terms, "intercept") == 0L) "uncentered response variation." else "response variation in the fitted sample."),
        paste(o$total_terms, "coefficient terms;", o$residual_df, "residual degrees of freedom.")),
      list(Coefficients = r$coefficients, Fit = data.frame(metric = c("AIC", "BIC", "F statistic", "Residual df", "Log-likelihood"),
        value = c(o$AIC, o$BIC, o$f_statistic, o$residual_df, o$log_likelihood))),
      c("Coefficient intervals" = "coefficients", "Observed vs fitted" = "fitted", "Residuals vs fitted" = "residuals",
        "Normal Q-Q" = "qq", "Scale-location" = "scale_location", "Cook's distance" = "cooks"), r$interpretation, limits))
  }
  if (kind == "glm") {
    r <- rr_glm(x, limits)
    ratio <- (x$family$family == "binomial" && x$family$link == "logit") ||
      (x$family$family == "poisson" && x$family$link == "log")
    return(rr_new_report(kind, "See the model on its own scale", formula(),
      list(Observations = stats::nobs(x), Family = x$family$family, Link = x$family$link, AIC = x$aic),
      c(paste("Deviance reduction from the intercept-only fit:", rr_fmt(100 * r$deviance_reduction), "percent."),
        paste("Fitting", if (isTRUE(x$converged)) "converged." else "did not converge; inspect the fit before interpreting coefficients.")),
      list(Coefficients = r$coefficients, Fit = data.frame(metric = c("Residual deviance", "Null deviance", "Residual df", "Dispersion", "Iterations"),
        value = c(x$deviance, x$null.deviance, x$df.residual, r$dispersion, x$iter))),
      c("Coefficient intervals" = "coefficients", "Response vs fitted" = "fitted", "Deviance residuals" = "residuals"),
      c("Intervals are 95% Wald intervals on the link scale, using the summary.glm reference distribution; they are not profile-likelihood intervals.",
        if (ratio) "Use Ratio scale to show odds ratios (binomial/logit) or rate ratios (Poisson/log)." else "Coefficients describe the fitted link scale.",
        "Deviance reduction is a descriptive fit measure, not ordinary R-squared. Coefficients describe conditional associations.",
        if (anyNA(x$coefficients)) "Rank deficiency: aliased coefficients are unavailable.",
        if (!is.null(x$na.action)) paste(length(x$na.action), "observations omitted by the fitted NA action.")), limits))
  }
  if (kind == "htest") {
    estimated <- if (length(x$estimate)) data.frame(quantity = names(x$estimate), estimate = as.numeric(x$estimate)) else data.frame(quantity = character(), estimate = numeric())
    interval <- if (length(x$conf.int)) data.frame(confidence = attr(x$conf.int, "conf.level"), lower = x$conf.int[1L], upper = x$conf.int[2L]) else data.frame()
    details <- data.frame(quantity = c(names(x$statistic), names(x$parameter), names(x$null.value)),
      value = as.numeric(c(x$statistic, x$parameter, x$null.value)))
    return(rr_new_report(kind, x$method, x$data.name,
      list("P value" = x$p.value, Statistic = if (length(x$statistic)) unname(x$statistic[1L]) else NA_real_,
        Alternative = x$alternative, "Confidence level" = if (length(x$conf.int)) attr(x$conf.int, "conf.level") else NA_real_),
      c(paste("Alternative hypothesis:", x$alternative),
        if (length(x$estimate)) paste(paste(names(x$estimate), rr_fmt(x$estimate), sep = " = "), collapse = "; ") else "This test does not store an effect estimate."),
      list(Estimates = estimated, Interval = interval, Test = details), c("Estimate and uncertainty" = "test"),
      c("The report uses the saved test result. Raw observations and diagnostic checks cannot be reconstructed from an htest object.",
        "A p value does not measure effect size or the probability that the null hypothesis is true.",
        if (length(x$conf.int) && any(!is.finite(x$conf.int))) "The stored interval is one-sided or unbounded; the graph displays finite endpoints only."), limits))
  }
  if (kind == "kmeans") {
    sizes <- data.frame(cluster = as.character(seq_along(x$size)), size = x$size, within_ss = x$withinss)
    centers <- x$centers
    return(rr_new_report(kind, "Understand the groups you found", "Cluster sizes, within-cluster variation and fitted centers",
      list(Clusters = length(x$size), Observations = sum(x$size), "Between / total SS (%)" = if (x$totss > 0) 100 * x$betweenss / x$totss else NA_real_, Iterations = x$iter),
      c(paste("Cluster sizes range from", min(x$size), "to", max(x$size), "observations."),
        paste("Total within-cluster sum of squares:", rr_fmt(x$tot.withinss))),
      list(Clusters = sizes, Centers = data.frame(cluster = rownames(centers), centers, check.names = FALSE)),
      c("Cluster sizes" = "clusters", "Cluster center profiles" = "centers"),
      c("Cluster centers use the scale on which kmeans was fitted. Variables in different units should be interpreted accordingly.",
        "The saved kmeans result has no raw observation coordinates. Membership scatterplots are not fabricated.",
        if (!is.null(x$ifault) && x$ifault != 0L) paste("The fitting algorithm reported ifault =", x$ifault)), limits))
  }
  if (kind == "aov") {
    table <- as.data.frame(stats::anova(x))
    table$term <- trimws(rownames(table))
    rownames(table) <- NULL
    table <- table[, c(ncol(table), seq_len(ncol(table) - 1L)), drop = FALSE]
    total <- sum(table[["Sum Sq"]])
    table$eta_squared <- if (total > 0) table[["Sum Sq"]] / total else NA_real_
    table$eta_squared[table$term == "Residuals"] <- NA_real_
    return(rr_new_report(kind, "See which terms explain variation", formula(),
      list(Observations = stats::nobs(x), Terms = sum(table$term != "Residuals"), "Residual df" = x$df.residual,
        "Residual standard error" = stats::summary.lm(x)$sigma),
      c("Sequential (Type I) sums of squares follow the fitted formula order.",
        "Compare term contributions together with residual variation and model diagnostics."),
      list(ANOVA = table), c("Variance contributions" = "anova", "Residuals vs fitted" = "residuals", "Normal Q-Q" = "qq"),
      c("Eta-squared is term sum of squares / total sum of squares for this sequential table; it is not partial eta-squared.",
        "Unbalanced designs and interactions require care when interpreting term-order-dependent sums of squares. Multi-stratum aovlist objects use the explorer."), limits))
  }
  if (kind == "ts") {
    m <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else unclass(x)
    idx <- seq_len(nrow(m))
    cols <- seq_len(ncol(m))
    nm <- colnames(m)
    if (is.null(nm)) nm <- paste("Series", seq_len(ncol(m)))
    summary <- do.call(rbind, lapply(cols, function(j) {
      v <- m[idx, j]; finite <- v[is.finite(v)]
      data.frame(series = nm[j], values = length(v), missing = sum(is.na(v)),
        mean = if (length(finite)) mean(finite) else NA_real_, sd = stats::sd(finite))
    }))
    tsp <- attr(x, "tsp", exact = TRUE)
    return(rr_new_report(kind, "Follow the signal through time", "Stored time coordinates, series profiles and autocorrelation",
      list(Observations = nrow(m), Series = ncol(m), Frequency = tsp[3L], "Time span" = paste(rr_fmt(tsp[1L], 8L), "to", rr_fmt(tsp[2L], 8L))),
      c(paste("The series has", tsp[3L], "observations per time unit."),
        paste(length(idx), "stored time positions are used for every series profile and trend display.")),
      list(Series = summary), c("Time trend" = "series", "Autocorrelation" = "acf"),
      c("Trend lines use every stored time position and break at non-finite values.",
        "ACF uses a bounded contiguous leading window of the first displayed series. It is descriptive and is not a stationarity test."), limits))
  }
  profile <- rr_data_profile(x, limits)
  p <- profile$data
  grouped <- attr(x, "groups", exact = TRUE)
  group_table <- NULL
  if (!is.null(grouped) && ".rows" %in% names(grouped)) {
    gp <- rr_plain(grouped)
    rows <- seq_len(rr_df_nrow(grouped))
    group_columns <- setdiff(names(gp), ".rows")
    group_table <- if (length(group_columns)) as.data.frame(lapply(gp[group_columns], function(v) v[rows]), optional = TRUE) else data.frame(group_row = rows)
    group_table$rows <- lengths(gp$.rows[rows])
  }
  missing <- if (nrow(p) && any(is.finite(p$missing_percent))) mean(p$missing_percent, na.rm = TRUE) else NA_real_
  rr_new_report(kind, "Get to know your data", "Data quality, variable profiles and relationships before modeling",
    list(Rows = rr_df_nrow(x), Columns = length(rr_plain(x)), "Profiled columns" = length(profile$cols), "Sample missing (%)" = missing),
    c(paste(length(profile$rows), "evenly spaced rows and", length(profile$cols), "columns profiled."),
      if (is.finite(missing)) paste("Average missingness across profiled atomic columns:", rr_fmt(missing), "percent.") else "No atomic columns available for a missingness profile."),
    c(list(Variables = p), if (!is.null(group_table)) list(Groups = group_table)), c(rr_data_choices(x, limits), if (length(rr_numeric_columns(x, limits)) >= 2L) c("Numeric correlations" = "correlation"), "Missingness by variable" = "missingness"),
    c("Profiles use a deterministic row sample. Non-atomic/custom columns are left uncomputed; distinct counts and means refer to the sample.",
      if (!is.null(group_table)) paste(rr_df_nrow(grouped), "saved groups; grouping variables:", paste(setdiff(names(grouped), ".rows"), collapse = ", ")),
      "Pearson correlations use pairwise complete finite sample values. Relationships describe associations, not causation."), limits)
}

#' Create a structured report from a supported R object
#'
#' @param x A supported standard result; see \code{rresult_supported}.
#' @param limits Named list overriding the finite limits in \code{rresult}.
#' @param metadata,sample_id Optional ordination sample annotations and ID
#'   matching, as in \code{rresult}.
#' @param assay Optional expression assay name/index for a bioinformatics
#'   container. NULL selects the native default assay or first stored assay.
#' @param layer Optional exact Seurat layer name. Split layers remain separate;
#'   they are not joined. Bioconductor expression sources are selected by assay.
#' @param bio_coef Optional saved coefficient/contrast name or index for a
#'   limma MArrayLM result; the default is its first coefficient.
#' @details Bioinformatics reports use stored expression, coordinates, metadata
#'   and statistics. Any BH FDR adjustment covers all stored gene rows before
#'   preview sampling and is disabled if that family exceeds max_scan_rows.
#'   SpatialExperiment reports display saved coordinates without image overlays.
#'   Large sparse/delayed expression matrices are sliced before dense conversion.
#' @return A list of metrics, highlights, tables, plot choices and notes.
#'   Saved result tables retain all rows and columns; browser limits apply to
#'   pages, not CSV exports. Named expression/distance windows and ggplot source
#'   previews remain bounded and are identified in the report.
#'   Results are taken from the supplied object; models are never refitted.
#' @examples
#' rresult_report(t.test(mpg ~ am, data = mtcars))
#' rresult_report(prcomp(USArrests, scale. = TRUE))
#' @export
rresult_report <- function(x, limits = list(), metadata = NULL, sample_id = NULL, assay = NULL, layer = NULL, bio_coef = NULL) {
  rr_report(x, do.call(rr_limits, limits), metadata, sample_id, assay, layer, bio_coef)
}



