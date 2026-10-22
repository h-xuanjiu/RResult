# Trusted adapters ------------------------------------------------------------

.rr_adapters <- new.env(parent = emptyenv())
.rr_lm_modules <- list(
  overview = function(x, limits) rr_lm(x, limits)$overview,
  coefficients = function(x, limits) rr_lm(x, limits)$coefficients,
  interpretation = function(x, limits) rr_lm(x, limits)$interpretation
)

#' Register a trusted object adapter
#'
#' @description
#' Add an explicitly trusted adapter without changing the generic explorer.
#' Adapters can expose arbitrary named modules; coefficients and diagnostics
#' are not required. Registration lasts for the current R process.
#'
#' @param class One exact class name matched against an object's class attribute.
#' @param modules Named list of functions, each accepting \code{x} and
#'   \code{limits} and returning a bounded R result or ggplot.
#' @param overwrite Logical; replace an existing registration.
#'
#' @details Adapter code is trusted executable R code and is responsible for
#'   safe access and bounded computation. Do not register untrusted functions.
#'   Built-in reports recognize exact standard classes for lm, glm, aov, htest,
#'   prcomp, kmeans, ts/mts, data.frame and tibble. Unknown subclasses fall back
#'   to the generic explorer. Built-in adapter names cannot be replaced.
#'   Additional standard reports cover table/xtabs, dist, hclust, density and nls.
#'   Use \code{rresult_supported()} for an inventory.
#'   Standard ggplot objects have an original-chart and inspection report.
#'   Standard model summaries, ANOVA tables, Tukey/pairwise/power results,
#'   survival models, mixed models and emmeans objects also have built-in
#'   reports. Their modelling packages are optional suggested dependencies.
#'   Further explicit adapters cover vegan permutation tables, RDA/CCA,
#'   environmental fits and dispersion, MASS negative-binomial models,
#'   mgcv GAM/bam models, pROC binary ROC results and DESeqResults.
#'   Bioinformatics adapters cover Seurat/Assay/Assay5, Bioconductor experiment
#'   containers, edgeR/limma results and saved enrichment-result classes.
#' @return Invisibly, the registered module list.
#' @examples
#' rresult_register_adapter("example_result", list(
#'   overview = function(x, limits) list(status = "ready")
#' ))
#' @export
rresult_register_adapter <- function(class, modules, overwrite = FALSE) {
  if (!is.character(class) || length(class) != 1L || is.na(class) || !nzchar(class)) stop("class must be one nonempty string.", call. = FALSE)
  if (!is.list(modules) || !length(modules) || is.null(names(modules)) ||
      anyNA(names(modules)) || any(!nzchar(names(modules))) || anyDuplicated(names(modules)) ||
      !all(vapply(modules, is.function, logical(1)))) stop("modules must be a uniquely named list of functions.", call. = FALSE)
  if (class %in% c("lm", "glm", "aov", "htest", "prcomp", "kmeans", "ts", "mts", "data.frame", "tbl_df", "table", "xtabs", "dist", "hclust", "density", "nls", "gg", "ggplot", "ggplot2::ggplot",
      .rr_first_families, .rr_ordination_families, .rr_extended_families, .rr_bio_families, "bam", "data.table", "grouped_df", "rowwise_df", "lmerMod", "glmerMod", "lmerModLmerTest")) stop("The built-in adapter cannot be replaced.", call. = FALSE)
  if (exists(class, envir = .rr_adapters, inherits = FALSE) && !isTRUE(overwrite)) stop("Adapter already registered; set overwrite = TRUE.", call. = FALSE)
  assign(class, modules, envir = .rr_adapters)
  invisible(modules)
}

rr_adapter <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  if (identical(cls, "lm")) return(list(name = "Linear model", modules = .rr_lm_modules, kind = "lm"))
  kind <- rr_report_kind(x)
  if (!is.null(kind)) {
    titles <- c(glm = "Generalized linear model", aov = "Analysis of variance", htest = "Hypothesis test",
      prcomp = "Principal components", kmeans = "K-means clustering", ts = "Time series", data_frame = "Data overview",
      table = "Frequency table", dist = "Pairwise distances", hclust = "Hierarchical clustering", density = "Density estimate", nls = "Nonlinear model", ggplot = "Ggplot workspace")
    titles <- c(titles, "summary.lm" = "Linear model summary", "summary.glm" = "GLM summary", anova = "Analysis table",
      TukeyHSD = "Tukey comparisons", "pairwise.htest" = "Pairwise comparisons", "power.htest" = "Power analysis",
      survfit = "Survival curves", coxph = "Cox survival model", survreg = "Parametric survival model",
      merMod = "Mixed-effects model", lme = "Mixed-effects model", emmGrid = "Marginal means", summary_emm = "Marginal means summary")
    titles <- c(titles, princomp = "PCA", PCA = "PCA (FactoMineR)", metaMDS = "NMDS ordination",
      monoMDS = "MDS ordination", pcoa = "Principal coordinates", wcmdscale = "Principal coordinates",
      rda = "Redundancy analysis", cca = "Constrained correspondence analysis", "anova.cca" = "Permutation analysis",
      negbin = "Negative binomial model", gam = "Generalized additive model", betadisper = "Group dispersion",
      envfit = "Environmental fits", roc = "ROC analysis", DESeqResults = "Differential expression")
    titles <- c(titles, stats::setNames(paste("Bioinformatics:", .rr_bio_families), .rr_bio_families))
    return(list(name = unname(titles[kind]), kind = kind,
      modules = stats::setNames(rep(list(function(x, limits) rr_report(x, limits)), 3L), c("overview", "results", "interpretation"))))
  }
  for (cl in cls) if (exists(cl, envir = .rr_adapters, inherits = FALSE)) {
    return(list(name = cl, modules = get(cl, envir = .rr_adapters, inherits = FALSE)))
  }
  NULL
}

rr_lm <- function(x, limits) {
  if (!identical(attr(x, "class", exact = TRUE), "lm")) stop("The built-in report requires exact class lm.", call. = FALSE)
  # Call the known lm method explicitly; do not dispatch unknown subclasses.
  sm <- stats::summary.lm(x)
  coef <- stats::coef(x)
  # Reuse this summary for every result. Calling tidy(), confint() and
  # glance() separately repeats the same QR-based summary several times.
  tidy <- data.frame(term = names(coef), estimate = unname(coef),
    std.error = rep(NA_real_, length(coef)), statistic = rep(NA_real_, length(coef)),
    p.value = rep(NA_real_, length(coef)),
    stringsAsFactors = FALSE)
  estimable <- match(rownames(sm$coefficients), tidy$term)
  tidy[estimable, c("std.error", "statistic", "p.value")] <- sm$coefficients[, 2:4, drop = FALSE]
  critical <- suppressWarnings(stats::qt(c(.025, .975), x$df.residual))
  tidy$conf.low <- tidy$estimate + critical[1L] * tidy$std.error
  tidy$conf.high <- tidy$estimate + critical[2L] * tidy$std.error
  # Retain aliased terms once, with explicitly unavailable uncertainty.
  aliased <- names(coef)[is.na(coef)]
  total_terms <- nrow(tidy)
  f <- sm$fstatistic
  log_likelihood <- stats::logLik(x)
  likelihood_df <- attr(log_likelihood, "df")
  aic <- -2 * as.numeric(log_likelihood) + 2 * likelihood_df
  bic <- -2 * as.numeric(log_likelihood) + log(attr(log_likelihood, "nobs")) * likelihood_df
  n <- stats::nobs(x)
  # Keep the existing broom_metrics names for saved reports and consumers.
  glance <- list(r.squared = sm$r.squared, adj.r.squared = sm$adj.r.squared,
    sigma = sm$sigma, statistic = if (is.null(f)) NA_real_ else f["value"],
    p.value = if (is.null(f)) NA_real_ else stats::pf(f["value"], f["numdf"], f["dendf"], lower.tail = FALSE),
    df = if (is.null(f)) NA_real_ else f["numdf"], logLik = as.numeric(log_likelihood),
    AIC = aic, BIC = bic, deviance = stats::deviance(x), df.residual = x$df.residual, nobs = n)
  formula <- paste(deparse(stats::formula(x), width.cutoff = 100L), collapse = " ")
  overview <- list(formula = rr_text(formula, 1000L), n = n,
    r_squared = sm$r.squared, adjusted_r_squared = sm$adj.r.squared,
    residual_standard_error = sm$sigma, residual_df = x$df.residual,
    f_statistic = if (is.null(f)) NA_real_ else unname(f[[1L]]),
    f_num_df = if (is.null(f)) NA_real_ else unname(f[[2L]]),
    f_den_df = if (is.null(f)) NA_real_ else unname(f[[3L]]),
    AIC = aic, BIC = bic,
    log_likelihood = as.numeric(log_likelihood), rank = x$rank,
    total_terms = total_terms, terms_shown = nrow(tidy),
    broom_metrics = glance)
  caution <- c("Coefficients describe conditional associations in the fitted model, not causal effects.",
               "P > 0.05 is not evidence that a null hypothesis is true; uncertainty is represented by confidence intervals.")
  terms <- x$terms
  if (length(aliased)) caution <- c(caution, "Rank deficiency: aliased coefficients and their uncertainty are unavailable.")
  if (!is.null(terms) && attr(terms, "intercept") == 0L) caution <- c(caution, "No intercept: R-squared is the uncentered R definition; comparisons with intercept models need care.")
  if (!is.null(terms) && any(attr(terms, "order") > 1L)) caution <- c(caution, "Interaction terms: main effects depend on the other predictors and their reference values.")
  if (length(x$xlevels)) caution <- c(caution, "Factor coefficients depend on the fitted contrasts and reference levels.")
  if (x$df.residual <= 0) caution <- c(caution, "No residual degrees of freedom: standard errors, tests and intervals can be undefined.")
  if (!is.null(x$na.action)) caution <- c(caution, paste(length(x$na.action), "observations were omitted by the fitted model's NA action."))
  term_notes <- vapply(seq_len(min(nrow(tidy), limits$max_children)), function(i) {
    term <- tidy[i, ]
    if (!is.finite(term$estimate)) return(paste0("Term '", term$term, "': coefficient is not estimable."))
    direction <- if (term$estimate > 0) "positive" else if (term$estimate < 0) "negative" else "zero"
    interval <- if (all(is.finite(c(term$conf.low, term$conf.high)))) {
      includes_zero <- term$conf.low <= 0 && term$conf.high >= 0
      paste0("95% CI [", signif(term$conf.low, 4), ", ", signif(term$conf.high, 4), "] ",
        if (includes_zero) "includes zero; the estimate remains uncertain." else "excludes zero under the fitted model's assumptions.")
    } else "Confidence interval is unavailable."
    paste0("Term '", term$term, "': estimate ", signif(term$estimate, 4), " (", direction, "). ", interval)
  }, character(1))
  caution <- c(caution, term_notes)
  list(overview = overview, coefficients = tidy, interpretation = caution)
}

rr_lm_diagnostics <- function(x, limits) {
  if (!identical(attr(x, "class", exact = TRUE), "lm")) stop("Diagnostics require exact class lm.", call. = FALSE)
  # Known lm diagnostic methods can inspect the model, but only bounded points
  # are passed to ggplot/browser. These are real fitted-model diagnostics.
  residual <- x$residuals
  fitted <- x$fitted.values
  n <- length(residual)
  index <- if (!n) integer() else unique(as.integer(seq(1, n, length.out = min(n, limits$max_plot_points))))
  diagnostic_model <- x
  diagnostic_model$na.action <- NULL
  standard <- suppressWarnings(stats::rstandard(diagnostic_model))[index]
  cook <- suppressWarnings(stats::cooks.distance(diagnostic_model))[index]
  list(data = data.frame(index = index, fitted = fitted[index], residual = residual[index],
                         standard = standard, scale_location = sqrt(abs(standard)), cook = cook),
       sampled = n > length(index), population = n)
}


