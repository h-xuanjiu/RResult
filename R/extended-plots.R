# Plots from explicit saved-result adapters; prediction grids are bounded.

rr_gam_smooth_data <- function(x, limits, smooth_term = NULL) {
  rr_require("mgcv")
  choices <- rr_gam_smooth_choices(x)
  if (!length(choices)) stop("No saved one-dimensional numeric smooth is available for a curve.", call. = FALSE)
  selected <- if (is.null(smooth_term)) unname(choices[1L]) else rr_integer(as.numeric(smooth_term), "smooth_term", upper = length(x$smooth))
  if (!selected %in% choices) stop("Select an available one-dimensional smooth without a by modifier.", call. = FALSE)
  s <- x$smooth[[selected]]
  parameters <- seq.int(s$first.para, s$last.para)
  width <- length(parameters)
  if (width > limits$max_cols || as.double(width)^2 > limits$max_cells)
    stop("Smooth covariance exceeds max_cols/max_cells; increase the explicit limits.", call. = FALSE)
  n <- min(100L, limits$max_plot_points, limits$max_cells %/% width)
  if (n < 2L) stop("The smooth grid requires at least two points within max_cells.", call. = FALSE)
  values <- x$model[[s$term]]
  sample <- values[rr_sample_indices(length(values), limits$max_plot_points)]
  sample <- sample[is.finite(sample)]
  if (length(unique(sample)) < 2L) stop("The saved smooth covariate has fewer than two distinct finite sampled values.", call. = FALSE)
  grid <- seq(min(sample), max(sample), length.out = n)
  data <- stats::setNames(data.frame(grid), s$term)
  basis <- mgcv::PredictMat(s, data)
  fit <- as.numeric(basis %*% x$coefficients[parameters])
  covariance <- x$Vp[parameters, parameters, drop = FALSE]
  se <- sqrt(pmax(0, rowSums((basis %*% covariance) * basis)))
  list(data = data.frame(x = grid, fit = fit, se = se, lower = fit - stats::qnorm(.975) * se,
    upper = fit + stats::qnorm(.975) * se), term = s$label, variable = s$term)
}

rr_extended_model_plot <- function(x, kind, type, limits, include_intercept, effect_scale, show_ci, smooth_term) {
  if (type == "smooths") {
    r <- rr_gam_smooth_data(x, limits, smooth_term); d <- r$data
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$fit))
    if (show_ci) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), fill = "#b2d8db", alpha = .45)
    return(p + ggplot2::geom_line(colour = "#187f88", linewidth = .9) + ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "#9da9b6") +
      ggplot2::labs(title = r$term, x = r$variable, y = "Centered smooth effect (link scale)",
        subtitle = "Approximate 95% pointwise intervals; conditional on smoothing parameters",
        caption = "Bounded grid over the sampled saved covariate range; no refitting"))
  }
  if (type %in% c("coefficients", "rate_ratios")) {
    d <- if (kind == "gam") rr_gam_summary(x, limits)$coefficients else rr_glm(x, limits)$coefficients
    if (!include_intercept && nrow(d) > 1L) d <- d[d$term != "(Intercept)", , drop = FALSE]
    d <- utils::head(d, limits$max_plot_points)
    ratio <- type == "rate_ratios" || effect_scale == "ratio"
    if (ratio) {
      if (kind != "negbin" || x$family$link != "log") stop("Rate ratios require a negative-binomial model with log link.", call. = FALSE)
      d[c("estimate", "conf.low", "conf.high")] <- lapply(d[c("estimate", "conf.low", "conf.high")], exp)
    }
    p <- rr_interval_plot(d, if (ratio) "Conditional count ratios" else "Parametric coefficients",
      if (ratio) "Ratio and 95% Wald interval" else "Link-scale estimate and 95% interval", if (ratio) 1 else 0, show_ci)
    if (ratio) p <- p + ggplot2::scale_x_log10()
    return(p)
  }
  compact <- x; compact$na.action <- NULL
  residual <- if (kind == "gam") utils::getFromNamespace("residuals.gam", "mgcv")(compact, type = "deviance") else
    stats::residuals.glm(compact, type = "deviance")
  fitted <- x$fitted.values
  if (!is.numeric(fitted) || !is.null(dim(fitted)) || length(residual) != length(fitted))
    stop("This diagnostic requires one saved fitted response per observation.", call. = FALSE)
  idx <- rr_sample_indices(length(fitted), min(limits$max_plot_points, limits$max_cells %/% 2L))
  if (type == "fitted") {
    response <- x$y
    if (is.null(response) && !is.null(x$model)) response <- stats::model.response(x$model)
    if (is.null(response) || !is.null(dim(response)) || length(response) != length(fitted))
      stop("A scalar observed response was not saved; use a different chart.", call. = FALSE)
    d <- data.frame(fitted = fitted[idx], observed = response[idx])
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$fitted, .data$observed)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "#9da9b6") +
      ggplot2::labs(title = "Observed response vs fitted", y = "Observed response")
  } else {
    d <- data.frame(fitted = fitted[idx], residual = residual[idx])
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$fitted, .data$residual)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "#9da9b6") +
      ggplot2::labs(title = "Deviance residuals vs fitted", y = "Deviance residual")
  }
  p + ggplot2::geom_point(colour = "#187f88", alpha = .65) + ggplot2::labs(x = "Fitted response",
    subtitle = paste(length(idx), "displayed fitted observations of", length(fitted)))
}

rr_extended_plot <- function(x, type, limits, axis_x = NULL, axis_y = NULL, include_intercept = FALSE,
                             effect_scale = "link", show_ci = TRUE, smooth_term = NULL,
                             padj_cutoff = NULL, lfc_cutoff = 0) {
  kind <- rr_extended_kind(x)
  if (!type %in% rr_extended_report(x, kind, limits)$plots) stop("This chart is unavailable for the saved result.", call. = FALSE)
  for (flag in list(include_intercept, show_ci)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) stop("Chart switches must be TRUE or FALSE.", call. = FALSE)
  if (kind %in% c("gam", "negbin")) return(rr_plot_style(rr_extended_model_plot(x, kind, type, limits,
    include_intercept, effect_scale, show_ci, smooth_term)))
  if (kind == "anova.cca") {
    d <- rr_anova_frame(x, limits)
    if (type == "p_values") {
      d$p.value <- d[[names(d)[grepl("^Pr\\(", names(d))][1L]]]
      return(rr_plot_style(rr_saved_p_plot(d, d$term, "Saved permutation P values")))
    }
    column <- if (type == "r_squared") "R2" else if (type == "f_statistics") "F" else
      intersect(c("SumOfSqs", "Variance", "ChiSquare"), names(d))[1L]
    d$value <- d[[column]]
    d <- d[is.finite(d$value) & !d$term %in% c("Total", "Residual", "Residuals"), , drop = FALSE]
    if (!nrow(d)) stop("No finite term quantities are saved for this chart.", call. = FALSE)
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$value, stats::reorder(.data$term, .data$value))) +
      ggplot2::geom_col(fill = "#187f88") + ggplot2::labs(title = paste("Saved", column), x = column, y = NULL,
        subtitle = "Original term quantities; total and residual rows remain in the table")
  } else if (kind == "betadisper") {
    if (type == "group_distances") {
      idx <- seq_len(min(length(x$group.distances), limits$max_children, limits$max_plot_points, limits$max_cells %/% 2L))
      labels <- names(x$group.distances)
      if (is.null(labels)) labels <- dimnames(x$group.distances)[[1L]]
      d <- data.frame(group = make.unique(rr_text(labels[idx], 80L)), distance = as.numeric(x$group.distances[idx]))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$group, .data$distance)) + ggplot2::geom_col(fill = "#187f88") +
        ggplot2::labs(title = "Saved group mean distances", y = "Mean distance to saved group center")
    } else {
      idx <- rr_sample_indices(length(x$distances), min(limits$max_plot_points, limits$max_cells %/% 2L))
      d <- data.frame(group = rr_plot_values(x$group, idx), distance = as.numeric(x$distances[idx]))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$group, .data$distance)) +
        ggplot2::geom_boxplot(fill = "#b2d8db", outlier.shape = NA) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = .12, height = 0, seed = 1), colour = "#187f88", alpha = .6) +
        ggplot2::labs(title = "Within-group dispersion", y = "Distance to saved group center", subtitle = paste(length(idx), "sampled distances of", length(x$distances)))
    }
    p <- p + ggplot2::labs(x = "Group")
  } else if (kind == "envfit") {
    if (type == "env_vectors") {
      m <- x$vectors$arrows
      axes <- rr_ordination_axes(NULL, axis_x, axis_y, m)
      idx <- seq_len(min(nrow(m), limits$max_children, limits$max_plot_points, limits$max_cells %/% 3L))
      multiplier <- sqrt(pmax(0, x$vectors$r[idx]))
      d <- data.frame(x = m[idx, axes[1L]] * multiplier, y = m[idx, axes[2L]] * multiplier, variable = rr_text(rownames(m)[idx], 80L))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)) +
        ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y), colour = "#187f88", arrow = grid::arrow(length = grid::unit(.15, "cm"))) +
        rr_ordination_labels(d, ggplot2::aes(.data$x, .data$y, label = .data$variable), colour = "#187f88") + ggplot2::coord_equal() +
        ggplot2::labs(title = "Saved fitted environmental vectors", x = colnames(m)[axes[1L]], y = colnames(m)[axes[2L]],
          subtitle = "Directions scaled by sqrt(saved R-squared); sample coordinates are not included")
    } else {
      tables <- rr_envfit_tables(x, limits)
      parts <- tables[intersect(c("Vectors", "Factors"), names(tables))]
      if (!length(parts)) stop("No saved environmental fits are available.", call. = FALSE)
      d <- do.call(rbind, lapply(names(parts), function(nm) { p <- parts[[nm]]; p$variable <- paste(nm, p$variable, sep = ": "); p }))
      d <- utils::head(d, min(limits$max_plot_points, limits$max_cells %/% 3L))
      if (type == "p_values") return(rr_plot_style(rr_saved_p_plot(d, d$variable, "Saved environmental-fit P values")))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$r_squared, stats::reorder(.data$variable, .data$r_squared))) +
        ggplot2::geom_point(colour = "#187f88", size = 3) + ggplot2::labs(title = "Saved environmental fit strengths", x = "R-squared", y = NULL)
    }
  } else if (kind == "roc") {
    d <- rr_roc_data(x, limits, complete = TRUE)
    maximum <- if (isTRUE(x$percent)) 100 else 1
    if (type == "roc") {
      d$false_positive <- maximum - d$specificity
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$false_positive, .data$sensitivity)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "#9da9b6") +
        ggplot2::geom_path(colour = "#187f88", linewidth = .9) + ggplot2::coord_equal(xlim = c(0, maximum), ylim = c(0, maximum)) +
        ggplot2::labs(title = "Saved ROC curve", x = if (isTRUE(x$percent)) "False-positive rate (100 - specificity)" else "False-positive rate (1 - specificity)", y = "Sensitivity")
    } else {
      d <- d[is.finite(d$threshold), , drop = FALSE]
      if (!nrow(d)) stop("No finite saved thresholds available.", call. = FALSE)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$threshold)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$sensitivity, colour = "Sensitivity"), linewidth = .9) +
        ggplot2::geom_line(ggplot2::aes(y = .data$specificity, colour = "Specificity"), linewidth = .9) +
        ggplot2::labs(title = "Saved threshold trade-offs", x = "Threshold", y = if (isTRUE(x$percent)) "Percent" else "Proportion", colour = NULL)
    }
    p <- p + ggplot2::labs(subtitle = paste(nrow(d), "saved coordinate points displayed |", if (isTRUE(x$percent)) "percent scale" else "proportion scale"))
  } else {
    r <- rr_deseq_data(x, limits, complete = TRUE); d <- r$data
    if (is.null(padj_cutoff)) padj_cutoff <- if (length(r$metadata$alpha)) r$metadata$alpha else .05
    if (!is.numeric(padj_cutoff) || length(padj_cutoff) != 1L || !is.finite(padj_cutoff) || padj_cutoff <= 0 || padj_cutoff > 1)
      stop("padj_cutoff must be a number greater than zero and at most one.", call. = FALSE)
    if (!is.numeric(lfc_cutoff) || length(lfc_cutoff) != 1L || !is.finite(lfc_cutoff) || lfc_cutoff < 0)
      stop("lfc_cutoff must be one finite nonnegative number.", call. = FALSE)
    d$status <- ifelse(!is.finite(d$padj), "Adjusted P unavailable", ifelse(d$padj < padj_cutoff & abs(d$log2FoldChange) >= lfc_cutoff, "Meets display cutoffs", "Other saved results"))
    if (type == "volcano") {
      d <- d[is.finite(d$log2FoldChange) & is.finite(d$padj) & d$padj >= 0 & d$padj <= 1, , drop = FALSE]
      d$score <- -log10(pmax(d$padj, .Machine$double.xmin))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$log2FoldChange, .data$score, colour = .data$status)) +
        ggplot2::geom_hline(yintercept = -log10(padj_cutoff), linetype = 2, colour = "#9da9b6") +
        ggplot2::labs(title = "Saved differential-expression volcano plot", x = "Saved log2 fold change", y = "-log10(adjusted P)")
    } else {
      d <- d[is.finite(d$baseMean) & d$baseMean > 0 & is.finite(d$log2FoldChange), , drop = FALSE]
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$baseMean, .data$log2FoldChange, colour = .data$status)) +
        ggplot2::scale_x_log10() + ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "#9da9b6") +
        ggplot2::labs(title = "Saved differential-expression MA plot", x = "Saved mean normalized count (log scale)", y = "Saved log2 fold change")
    }
    if (!nrow(d)) stop("No finite saved features are available for this chart.", call. = FALSE)
    p <- p + ggplot2::geom_point(alpha = .65, size = 2) +
      ggplot2::scale_colour_manual(values = c("Meets display cutoffs" = "#cc7852", "Other saved results" = "#187f88", "Adjusted P unavailable" = "#9da9b6")) +
      ggplot2::labs(colour = NULL, subtitle = paste(nrow(d), "displayed features of", r$population, "| padj <", padj_cutoff, "and |log2FC| >=", lfc_cutoff),
        caption = "Saved adjusted P values retained; unavailable values excluded from volcano plots; zero values use a finite display floor")
  }
  rr_plot_style(p)
}
