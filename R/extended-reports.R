# Additional saved ecological, nonlinear, diagnostic and genomic results.

.rr_extended_families <- c("anova.cca", "negbin", "gam", "betadisper", "envfit", "roc", "DESeqResults")
.rr_constrained_families <- c("rda", "cca")

rr_extended_kind <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  if (identical(cls, c("anova.cca", "anova", "data.frame"))) return("anova.cca")
  if (identical(cls, c("negbin", "glm", "lm"))) return("negbin")
  if (identical(cls, c("gam", "glm", "lm")) || identical(cls, c("bam", "gam", "glm", "lm"))) return("gam")
  for (kind in c("betadisper", "envfit", "roc")) if (identical(cls, kind)) return(kind)
  if (isS4(x) && length(cls) == 1L && as.character(cls) == "DESeqResults") {
    definition <- methods::getClassDef("DESeqResults")
    if (!is.null(definition) && definition@package == "DESeq2") return("DESeqResults")
  }
  NULL
}

rr_gam_summary <- function(x, limits) {
  rr_require("mgcv")
  sm <- utils::getFromNamespace("summary.gam", "mgcv")(x)
  co <- if (length(sm$p.table)) rr_coefficient_frame(sm$p.table, limits,
    df = if (grepl("t", colnames(sm$p.table)[4L], fixed = TRUE)) sm$residual.df else Inf) else data.frame()
  st <- sm$s.table
  smooths <- if (length(st)) data.frame(term = rownames(st), st, row.names = NULL, check.names = FALSE) else data.frame()
  list(summary = sm, coefficients = co, smooths = smooths)
}

rr_gam_smooth_choices <- function(x) {
  index <- which(vapply(x$smooth, function(s) {
    s$dim == 1L && identical(s$by, "NA") && length(s$term) == 1L &&
      !inherits(s, "random.effect") && !is.null(x$model[[s$term]]) &&
      is.numeric(x$model[[s$term]]) && is.null(dim(x$model[[s$term]]))
  }, logical(1)))
  stats::setNames(index, vapply(x$smooth[index], function(s) s$label, character(1)))
}

rr_vegan_anova_choices <- function(x) {
  columns <- names(rr_plain(x))
  c(if (any(grepl("^Pr\\(", columns))) c("Saved permutation P values" = "p_values"),
    if ("R2" %in% columns) c("Stored R-squared" = "r_squared"),
    if ("F" %in% columns) c("Saved F statistics" = "f_statistics"),
    if (length(intersect(c("SumOfSqs", "Variance", "ChiSquare"), columns))) c("Stored inertia / sums of squares" = "variation"))
}

rr_envfit_tables <- function(x, limits) {
  result <- list()
  for (key in c("vectors", "factors")) {
    part <- x[[key]]
    if (is.null(part) || !length(part$r)) next
    idx <- seq_along(part$r)
    nm <- names(part$r)
    if (is.null(nm) && key == "vectors") nm <- rownames(part$arrows)
    if (is.null(nm)) nm <- as.character(seq_along(part$r))
    d <- data.frame(variable = nm[idx], r_squared = as.numeric(part$r[idx]),
      p.value = if (length(part$pvals)) as.numeric(part$pvals[idx]) else NA_real_)
    result[[if (key == "vectors") "Vectors" else "Factors"]] <- d
  }
  if (!is.null(x$vectors$arrows)) result$Directions <- rr_ordination_table(x$vectors$arrows, limits, "variable")
  if (!is.null(x$factors$centroids)) result$Centroids <- rr_ordination_table(x$factors$centroids, limits, "level")
  result
}

rr_roc_data <- function(x, limits, cap = limits$max_plot_points, complete = FALSE) {
  n <- length(x$sensitivities)
  if (!n || length(x$specificities) != n || length(x$thresholds) != n) stop("The ROC object has incompatible saved coordinates.", call. = FALSE)
  idx <- if (complete) seq_len(n) else rr_sample_indices(n, min(cap, limits$max_plot_points, limits$max_cells %/% 3L))
  data.frame(threshold = x$thresholds[idx], sensitivity = x$sensitivities[idx], specificity = x$specificities[idx])
}

rr_deseq_data <- function(x, limits, cap = limits$max_plot_points, complete = FALSE) {
  rr_require("DESeq2")
  columns <- intersect(c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj"), names(x))
  if (!all(c("log2FoldChange", "padj") %in% columns)) stop("DESeqResults requires saved log2FoldChange and padj columns.", call. = FALSE)
  nc <- if (complete) length(columns) else min(length(columns), max(0L, limits$max_cols - 1L))
  idx <- if (complete) seq_len(nrow(x)) else rr_sample_indices(nrow(x), min(cap, limits$max_plot_points, limits$max_cells %/% (length(columns) + 1L)))
  ids <- rownames(x)
  d <- as.data.frame(x[idx, columns, drop = FALSE])
  names(d) <- columns
  d <- data.frame(feature = if (is.null(ids)) as.character(idx) else ids[idx], d, row.names = NULL)
  list(data = d, table = d[, c("feature", columns[seq_len(nc)]), drop = FALSE],
    population = nrow(x), metadata = S4Vectors::metadata(x))
}

rr_extended_report <- function(x, kind, limits) {
  call <- function() paste(deparse(x$call), collapse = " ")
  if (kind == "anova.cca") {
    d <- rr_anova_frame(x, limits)
    permutations <- attr(x, "F.perm", exact = TRUE)
    return(rr_new_report(kind, "Read the saved permutation analysis", paste(attr(x, "heading", exact = TRUE), collapse = " "),
      list(Rows = rr_df_nrow(x), "Stored permutations" = if (is.null(permutations)) NA_integer_ else NROW(permutations)),
      "Original term ordering, test statistics and P values are retained.", list(ANOVA = d), rr_vegan_anova_choices(x),
      c("The object may be a PERMANOVA or an ordination permutation analysis; its saved heading identifies the test and permutation scheme.",
        "R-squared is shown only when saved. Marginal or sequential term quantities need not sum to one. No new permutations are run.",
        "For PERMANOVA, group dispersion can influence interpretation; a betadisper result provides a separate view of dispersion."), limits))
  }
  if (kind == "negbin") {
    rr_require("MASS")
    r <- rr_glm(x, limits)
    return(rr_new_report(kind, "Inspect an overdispersed count model", paste(deparse(stats::formula(x)), collapse = " "),
      list(Observations = stats::nobs(x), Theta = x$theta, "Theta SE" = x$SE.theta, AIC = x$aic),
      paste("Negative binomial model with", x$family$link, "link; theta is the fitted dispersion parameter."),
      list(Coefficients = r$coefficients, Fit = data.frame(metric = c("Deviance", "Null deviance", "Residual df", "Theta", "Theta SE"),
        value = c(x$deviance, x$null.deviance, x$df.residual, x$theta, x$SE.theta))),
      c("Coefficient intervals" = "coefficients", if (x$family$link == "log") c("Rate ratios" = "rate_ratios"),
        "Response vs fitted" = "fitted", "Deviance residuals" = "residuals"),
      c("Coefficient intervals are 95% normal Wald intervals conditional on the fitted dispersion; theta and its standard error are saved estimates.",
        "For the log link, exponentiated coefficients are conditional mean/count ratios. No model is refitted.",
        if (!isTRUE(x$converged)) "The saved fit did not converge; inspect the original fit.",
        if (length(x$th.warn)) paste("Saved warning while fitting theta:", paste(x$th.warn, collapse = "; "),
          "Inspect the original fit and theta estimate before interpreting model uncertainty.")), limits))
  }
  if (kind == "gam") {
    r <- rr_gam_summary(x, limits); sm <- r$summary
    return(rr_new_report(kind, "Explore linear and smooth effects", paste(deparse(stats::formula(x)), collapse = " "),
      list(Observations = sm$n, "Deviance explained (%)" = 100 * sm$dev.expl, "Adjusted R-squared" = sm$r.sq,
        "Effective df" = sum(x$edf)),
      c(paste(length(x$smooth), "smooth terms; fitted family:", x$family$family),
        "Parametric coefficients and smooth-term tests have separate tables."),
      list(Coefficients = r$coefficients, Smooths = r$smooths),
      c(if (length(rr_gam_smooth_choices(x))) c("One-dimensional smooth effects" = "smooths"),
        if (nrow(r$coefficients)) c("Parametric coefficients" = "coefficients"), "Response vs fitted" = "fitted", "Deviance residuals" = "residuals"),
      c("Smooth-term tests are approximate tests from summary.gam, not tests of individual spline coefficients.",
        "Smooth curves are centered term effects on the link scale. Bands are approximate 95% pointwise intervals conditional on smoothing parameters.",
        "Curves are available for saved, numeric, one-dimensional smooths without a by modifier. Other smooths remain in the summary table.",
        "Native summary calculations can still depend on model size; only plotted prediction grids are bounded."), limits))
  }
  if (kind == "betadisper") {
    n <- length(x$distances); idx <- seq_len(n)
    counts <- table(x$group)
    groups <- names(x$group.distances)
    if (is.null(groups)) groups <- dimnames(x$group.distances)[[1L]]
    shown <- seq_along(groups)
    groups <- groups[shown]
    return(rr_new_report(kind, "Inspect within-group dispersion", call(),
      list(Observations = n, Groups = length(x$group.distances), Center = attr(x, "type", exact = TRUE), Distance = attr(x, "method", exact = TRUE)),
      "Distances and group mean distances come from the saved dispersion analysis.",
      list(Groups = data.frame(group = groups, mean_distance = as.numeric(x$group.distances[shown]), n = as.numeric(counts[groups])),
        Distances = data.frame(sample = if (is.null(names(x$distances))) as.character(idx) else names(x$distances)[idx],
          group = as.character(x$group[idx]), distance = as.numeric(x$distances[idx]))),
      c("Distances by group" = "dispersion", "Saved group mean distances" = "group_distances"),
      c("Group means retain the full saved analysis. Box plots use bounded sampled distances; their quartiles may differ from the full data.",
        "The center is the saved centroid or spatial median. This object alone does not store a permutation-test P value; pass the separately saved test table for inference.",
        paste("Saved bias adjustment:", isTRUE(attr(x, "bias.adjust", exact = TRUE)))), limits))
  }
  if (kind == "envfit") {
    tables <- rr_envfit_tables(x, limits)
    fits <- tables[intersect(c("Vectors", "Factors"), names(tables))]
    finite_p <- any(vapply(fits, function(d) any(is.finite(d$p.value)), logical(1)))
    return(rr_new_report(kind, "Read environmental fits to an ordination", "Saved vector and factor fits",
      list("Numeric variables" = length(x$vectors$r), "Factor variables" = length(x$factors$r),
        "Vector permutations" = if (length(x$vectors$permutations)) x$vectors$permutations else NA_integer_),
      "Fit strengths and available permutation P values retain the original ordination fit.", tables,
      c("Saved fit strengths" = "fit_strength", if (finite_p) c("Saved P values" = "p_values"),
        if (!is.null(x$vectors$arrows) && ncol(x$vectors$arrows) >= 2L) c("Fitted vector directions" = "env_vectors")),
      c("Vector lengths use the square root of the saved fit R-squared, as in vegan scores.envfit. No new permutations are run.",
        "The envfit object does not contain the original sample coordinates. This standalone vector diagram is not a reconstruction of the sample ordination.",
        "Factor centroids are stored separately in the result tables. Missing P values remain unavailable."), limits))
  }
  if (kind == "roc") {
    d <- rr_roc_data(x, limits, complete = TRUE)
    ci <- x$ci
    interval <- if (inherits(ci, "ci.auc") && length(ci) == 3L) data.frame(confidence = attr(ci, "conf.level", exact = TRUE),
      lower = as.numeric(ci[1L]), estimate = as.numeric(ci[2L]), upper = as.numeric(ci[3L])) else data.frame()
    return(rr_new_report(kind, "Inspect discrimination and thresholds", "Saved binary ROC analysis",
      list(AUC = if (length(x$auc)) as.numeric(x$auc) else NA_real_, Cases = length(x$cases), Controls = length(x$controls),
        Scale = if (isTRUE(x$percent)) "Percent" else "Proportion"),
      paste("Saved comparison direction:", x$direction, "| control:", x$levels[1L], "| case:", x$levels[2L]),
      list(Thresholds = d, "AUC interval" = interval), c("ROC curve" = "roc", "Threshold trade-offs" = "thresholds"),
      c("The stored curve, direction and response levels are preserved. AUC and its confidence interval are shown only when saved; no bootstrap is run.",
        if (is.numeric(attr(x$auc, "partial.auc", exact = TRUE))) "The saved AUC is a partial AUC with its original correction and range; it is not the full-curve AUC.",
        "AUC intervals do not define pointwise bands around the ROC curve. A good cutoff depends on the intended decision and is not selected automatically.",
        "ROC charts and threshold tables retain all saved coordinates."), limits))
  }
  # This is the same saved, narrow gene-result table already read by the
  # complete volcano/MA views; it never accesses an expression assay.
  r <- rr_deseq_data(x, limits, complete = TRUE); d <- r$data
  column_metadata <- S4Vectors::mcols(x)
  descriptions <- if ("description" %in% names(column_metadata)) data.frame(column = names(x), description = column_metadata$description) else data.frame()
  setting_names <- intersect(c("alpha", "lfcThreshold", "filterTheta", "filterThreshold"), names(r$metadata))
  settings <- lapply(setting_names, function(nm) data.frame(setting = nm, value = paste(rr_fmt(r$metadata[[nm]]), collapse = ", ")))
  rr_new_report(kind, "Explore a saved differential-expression result", "Original contrast, test and adjusted P values",
    list(Features = r$population, "Reported features" = nrow(d), "Available padj" = sum(is.finite(d$padj)),
      "Saved FDR threshold" = if (length(r$metadata$alpha)) r$metadata$alpha else NA_real_),
    "Effect estimates and multiple-testing results come from the supplied DESeqResults.",
    list(Results = r$table, "Column descriptions" = descriptions,
      "Saved settings" = if (length(settings)) do.call(rbind, settings) else data.frame()),
    c("Volcano plot (adjusted P)" = "volcano", if ("baseMean" %in% names(d)) c("MA plot" = "ma")),
    c("Plots and result tables use all saved gene rows. Display cutoffs do not recompute P values, shrinkage or multiple-testing correction.",
      "Missing adjusted P values remain unavailable and are not classified as nonsignificant. Inspect the original result for independent filtering and outlier handling.",
      "For likelihood-ratio tests, the P value can test several coefficients and need not test the displayed fold change alone.",
      "Only the saved result is inspected; this object does not contain the full expression matrix.",
      if (length(r$metadata$lfcThreshold)) paste("Saved test fold-change threshold:", r$metadata$lfcThreshold)), limits)
}

rr_constrained_report <- function(x, kind, limits, metadata, sample_id) {
  o <- rr_ordination(x, kind)
  components <- c(Conditioned = if (length(x$pCCA$tot.chi)) x$pCCA$tot.chi else 0,
    Constrained = if (length(x$CCA$tot.chi)) x$CCA$tot.chi else 0,
    Residual = if (length(x$CA$tot.chi)) x$CA$tot.chi else 0)
  tables <- list(Inertia = data.frame(component = names(components), inertia = unname(components),
    percent_total = if (x$tot.chi > 0) 100 * unname(components) / x$tot.chi else NA_real_),
    Samples = rr_ordination_table(o$scores, limits), Species = rr_ordination_table(o$variables, limits, "species"),
    Axes = data.frame(axis = names(o$eigen), eigenvalue = unname(o$eigen), percent_total = unname(o$percent)))
  if (!is.null(o$environment)) tables$Environment <- rr_ordination_table(o$environment, limits, "variable")
  if (!is.null(x$CCA$centroids)) tables$Centroids <- rr_ordination_table(x$CCA$centroids, limits, "level")
  notes <- c(paste("Coordinates are", if (!is.null(x$CCA) && length(x$CCA$eig)) "saved weighted-average site and species scores" else "saved unconstrained site and species scores",
      "with vegan scaling = 0 and const = 1. Maps have equal axis units."),
    "Axis percentages use total saved inertia, including any conditioned and residual components; they are not percentages of the constrained component alone.",
    "The selected coordinate block is stated in the report. Partial models retain conditioned inertia separately.",
    "Arrow overlays use one reported visual multiplier; sample, species and environment arrow lengths should not be interpreted as sharing units.",
    "No permutation test is run automatically. Pass the saved anova.cca table to inspect its inference.")
  if (!is.null(metadata)) {
    idx <- seq_len(nrow(o$scores))
    matched <- rr_ordination_metadata(metadata, sample_id, o$scores, idx, limits, preview = FALSE)
    tables$Metadata <- cbind(tables$Samples["sample"], matched$data)
    notes <- c(notes, matched$note)
  }
  if (is.null(metadata) && !is.null(sample_id)) stop("sample_id requires metadata.", call. = FALSE)
  rr_new_report(kind, "Relate community variation to explanatory variables", paste(o$method, "|", o$coordinate_set, "| scaling 0"),
    list(Method = o$method, Samples = o$observations, "Total inertia" = x$tot.chi,
      "Constrained inertia (%)" = if (x$tot.chi > 0) 100 * components[["Constrained"]] / x$tot.chi else NA_real_),
    paste("Coordinate block:", o$coordinate_set, "| constrained rank:", if (length(x$CCA$rank)) x$CCA$rank else 0),
    tables, rr_ordination_choices(o), notes, limits)
}
