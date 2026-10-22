# Plots from stored, trusted results ------------------------------------------

rr_plot_style <- function(plot) {
  custom <- plot$theme
  plot + ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot", plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.margin = ggplot2::margin(16, 20, 16, 16), legend.position = "bottom") + custom
}

rr_numeric_columns <- function(x, limits) {
  p <- rr_plain(x)
  index <- seq_along(p)
  index[vapply(index, function(j) rr_simple(p[[j]]) && is.numeric(p[[j]]) && !is.complex(p[[j]]) &&
    is.null(attr(p[[j]], "dim", exact = TRUE)) && is.null(attr(p[[j]], "class", exact = TRUE)), logical(1))]
}

rr_glm_plot_response <- function(x) {
  observed <- x$y
  from_model <- is.null(observed)
  if (from_model && !is.null(x$model)) observed <- stats::model.response(x$model)
  if (is.null(observed)) stop("The fitted object does not store the response; use a different plot.", call. = FALSE)

  if (x$family$family %in% c("binomial", "quasibinomial")) {
    if (is.matrix(observed) && ncol(observed) == 2L) {
      # Match the family initialization, including rows with no trials.
      trials <- observed[, 1L] + observed[, 2L]
      observed <- observed[, 1L] / trials
      observed[trials == 0] <- 0
    } else {
      if (is.factor(observed)) observed <- observed != levels(observed)[1L]
      # Saved x$y has already been initialized. Only a raw, single-column
      # model response needs the family's zero-weight adjustment.
      if (from_model && length(x$prior.weights) == length(observed))
        observed[x$prior.weights == 0] <- 0
    }
  }
  if (is.matrix(observed) && ncol(observed) == 1L) observed <- observed[, 1L]
  if ((!is.numeric(observed) && !is.logical(observed)) || !is.null(dim(observed)) ||
      length(observed) != length(x$fitted.values))
    stop("The fitted object does not store a response aligned with its fitted values; use a different plot.", call. = FALSE)
  as.numeric(observed)
}

rr_effect_plot <- function(x, limits, include_intercept = FALSE, effect_scale = "link", show_ci = TRUE) {
  kind <- rr_report_kind(x)
  co <- if (kind == "glm") rr_glm(x, limits)$coefficients else rr_lm(x, limits)$coefficients
  if (!include_intercept) co <- co[co$term != "(Intercept)", , drop = FALSE]
  co <- co[is.finite(co$estimate), , drop = FALSE]
  if (!nrow(co)) stop("No estimable coefficient terms in this selection. Include the intercept to inspect an intercept-only fit.", call. = FALSE)
  null <- 0
  label <- "Estimate and 95% confidence interval"
  if (effect_scale == "ratio") {
    valid <- kind == "glm" && ((x$family$family == "binomial" && x$family$link == "logit") ||
      (x$family$family == "poisson" && x$family$link == "log"))
    if (!valid) stop("Ratio scale requires binomial/logit or Poisson/log.", call. = FALSE)
    co[c("estimate", "conf.low", "conf.high")] <- lapply(co[c("estimate", "conf.low", "conf.high")], exp)
    null <- 1
    label <- paste(if (x$family$family == "binomial") "Odds ratio" else "Rate ratio", "and 95% Wald interval")
  }
  co$direction <- ifelse(co$estimate >= null, "Above reference", "Below reference")
  p <- ggplot2::ggplot(co, ggplot2::aes(.data$estimate, stats::reorder(.data$term, .data$estimate), colour = .data$direction)) +
    ggplot2::geom_vline(xintercept = null, colour = "#9da9b6", linetype = 2)
  if (show_ci) p <- p + ggplot2::geom_segment(ggplot2::aes(x = .data$conf.low, xend = .data$conf.high,
      yend = stats::reorder(.data$term, .data$estimate)), linewidth = 1, na.rm = TRUE)
  p <- p +
    ggplot2::geom_point(size = 3, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c("Above reference" = "#187f88", "Below reference" = "#cc7852")) +
    ggplot2::labs(title = "Effect estimates and uncertainty", subtitle = if (kind == "glm") "95% Wald intervals; coefficients on the selected scale" else "95% model-based intervals; conditional associations",
      x = label, y = NULL, colour = NULL) + ggplot2::theme(legend.position = "bottom")
  if (effect_scale == "ratio") p <- p + ggplot2::scale_x_log10()
  rr_plot_style(p)
}

rr_report_plot <- function(x, type, limits, axis_x = NULL, axis_y = NULL, include_intercept = FALSE, effect_scale = "link",
                           aggregation = "mean", bins = 30L, show_points = TRUE, clusters = NULL,
                           show_ci = TRUE, show_censor = TRUE, colour_by = NULL, facet_by = NULL,
                           x_as_category = FALSE, y_as_category = FALSE, facet_scales = "fixed",
                           metadata = NULL, sample_id = NULL, shape_by = NULL, sample_labels = FALSE,
                           max_labels = 20L, top_variables = 12L, group_hulls = FALSE,
                           smooth_term = NULL, padj_cutoff = NULL, lfc_cutoff = 0,
                           assay = NULL, layer = NULL, reduction = NULL, feature = NULL, bio_coef = NULL, top_terms = 20L, bio_source = NULL, bio_result = NULL) {
  kind <- rr_report_kind(x)
  if (kind %in% .rr_bio_containers) return(rr_bio_container_plot(x, type, limits, assay, layer, reduction, feature, colour_by, shape_by, facet_by, axis_x, axis_y, source = bio_source))
  if (kind %in% .rr_bio_results) {
    if (is.null(bio_result) && kind %in% c("DGEExact", "DGELRT", "MArrayLM"))
      bio_result <- rr_bio_result_data(x, kind, limits, bio_coef, complete = TRUE)
    if (!type %in% rr_bio_result_choices(x, kind, limits, bio_coef, result = bio_result)) stop("This chart is unavailable for the saved bioinformatics result.", call. = FALSE)
    return(rr_bio_result_plot(x, kind, type, limits, bio_coef, padj_cutoff, lfc_cutoff, top_terms, result = bio_result))
  }
  if (kind %in% .rr_ordination_families) return(rr_ordination_plot(x, type, limits, axis_x, axis_y,
    metadata, sample_id, colour_by, shape_by, facet_by, sample_labels, max_labels, top_variables, group_hulls))
  if (kind %in% .rr_extended_families) return(rr_extended_plot(x, type, limits, axis_x, axis_y,
    include_intercept, effect_scale, show_ci, smooth_term, padj_cutoff, lfc_cutoff))
  if (kind == "ggplot") {
    if (!identical(type, "original")) stop("Existing ggplots support the original chart view.", call. = FALSE)
    return(x)
  }
  if (kind %in% .rr_first_families) return(rr_first_batch_plot(x, type, limits,
    include_intercept = include_intercept, effect_scale = effect_scale, show_ci = show_ci, show_censor = show_censor))
  if (kind %in% c("table", "dist", "hclust", "density", "nls")) return(rr_standard_plot(x, type, limits, clusters))
  if (kind == "data_frame" && type %in% c("auto", "scatter", "relationship", "line", "histogram", "density", "boxplot", "violin", "ecdf", "bar", "proportion", "jitter", "summary_bar", "count_heatmap", "freqpoly", "smooth", "bin2d", "mean_se", "count_bubble"))
    return(rr_data_plot(x, type, limits, axis_x, axis_y, aggregation, bins, show_points,
      colour_by, facet_by, x_as_category, y_as_category, facet_scales, show_ci))
  if (is.null(axis_x)) axis_x <- 1L
  if (is.null(axis_y)) axis_y <- 2L
  allowed <- switch(kind, lm = c("coefficients", "fitted", "residuals", "qq", "scale_location", "cooks"),
    glm = c("coefficients", "fitted", "residuals"), aov = c("anova", "residuals", "qq"), htest = "test",
    prcomp = c("scree", "scores", "loadings"), kmeans = c("clusters", "centers"),
    ts = c("series", "acf"), data_frame = c("missingness", "correlation", "relationship"))
  if (!type %in% allowed) stop("This plot is not available for the supplied object.", call. = FALSE)
  if (type == "coefficients") return(rr_effect_plot(x, limits, include_intercept, effect_scale, show_ci))
  if (kind == "lm" && type != "fitted") return(rr_plot(x, type, limits))
  if (kind == "aov" && type %in% c("residuals", "qq")) {
    class(x) <- "lm"
    return(rr_plot(x, type, limits))
  }
  if (kind %in% c("lm", "glm")) {
    idx <- seq_along(x$fitted.values)
    fitted <- x$fitted.values[idx]
    if (type == "fitted") {
      observed <- if (kind == "lm") (x$fitted.values + x$residuals)[idx] else rr_glm_plot_response(x)[idx]
      dat <- data.frame(fitted = fitted, observed = observed)
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$fitted, .data$observed)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#b2bec8", linetype = 2) +
        ggplot2::geom_point(colour = "#187f88", alpha = .65, size = 2) +
        ggplot2::labs(title = "Observed response vs fitted value", x = "Fitted response", y = "Observed response")
    } else {
      # Keep residuals in fitted-observation order: na.exclude would otherwise
      # restore omitted rows before the compact fitted-value indices are used.
      diagnostic_model <- x
      diagnostic_model$na.action <- NULL
      residual <- stats::residuals.glm(diagnostic_model, type = "deviance")
      dat <- data.frame(fitted = fitted, residual = residual[idx])
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$fitted, .data$residual)) +
        ggplot2::geom_hline(yintercept = 0, colour = "#b2bec8", linetype = 2) + ggplot2::geom_point(colour = "#187f88", alpha = .65) +
        ggplot2::labs(title = "Deviance residuals vs fitted", x = "Fitted response", y = "Deviance residual")
    }
    p <- p + ggplot2::labs(subtitle = paste("All", length(idx), "fitted observations"))
  } else if (kind == "htest") {
    estimate <- x$estimate
    if (length(estimate) == 2L && (grepl("t-test", x$method, fixed = TRUE) || grepl("proportions", x$method, fixed = TRUE))) {
      estimate <- stats::setNames(estimate[1L] - estimate[2L], "Difference (first - second)")
    }
    if (length(estimate) == 1L && length(x$conf.int) == 2L) {
      dat <- data.frame(estimate = as.numeric(estimate), lower = if (is.finite(x$conf.int[1L])) x$conf.int[1L] else NA_real_,
        upper = if (is.finite(x$conf.int[2L])) x$conf.int[2L] else NA_real_, label = names(estimate))
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$estimate, .data$label)) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$lower, xend = .data$upper, yend = .data$label), linewidth = 1, colour = "#187f88", na.rm = TRUE) +
        ggplot2::geom_point(size = 4, colour = "#187f88") +
        ggplot2::labs(title = "Estimate and stored confidence interval", x = "Effect estimate", y = NULL,
          subtitle = paste(rr_fmt(100 * attr(x$conf.int, "conf.level")), "% confidence; alternative:", x$alternative,
            if (any(!is.finite(x$conf.int))) "| Unbounded endpoint; finite interval is not drawn" else ""))
      if (length(x$null.value) == 1L && is.finite(x$null.value)) p <- p + ggplot2::geom_vline(xintercept = x$null.value, linetype = 2, colour = "#9da9b6")
    } else {
      val <- if (length(estimate)) estimate else x$statistic
      if (!length(val)) stop("This test stores no numeric estimate or statistic to plot.", call. = FALSE)
      dat <- data.frame(quantity = names(val), value = as.numeric(val))
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$value, .data$quantity)) + ggplot2::geom_point(size = 4, colour = "#187f88") +
        ggplot2::labs(title = "Saved test quantities", subtitle = "This result does not store a plottable confidence interval", x = "Value", y = NULL)
    }
  } else if (kind == "kmeans") {
    if (type == "clusters") {
      idx <- seq_along(x$size)
      dat <- data.frame(cluster = factor(idx), size = x$size[idx])
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$cluster, .data$size)) + ggplot2::geom_col(fill = "#187f88", width = .65) +
        ggplot2::labs(title = "How balanced are the clusters?", x = "Cluster", y = "Observations")
    } else {
      m <- x$centers
      nr <- nrow(m); nc <- ncol(m)
      labels <- colnames(m)
      if (is.null(labels)) labels <- paste("Variable", seq_len(nc))
      labels <- make.unique(labels)
      dat <- data.frame(cluster = rep(as.character(seq_len(nr)), nc), variable = rep(labels, each = nr), value = as.vector(m))
      dat$cluster <- factor(dat$cluster, levels = rev(as.character(seq_len(nr))))
      dat$variable <- factor(dat$variable, levels = labels)
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$variable, .data$cluster, fill = .data$value)) + ggplot2::geom_tile(colour = "white") +
        ggplot2::scale_fill_gradient2(low = "#cc7852", mid = "#f4f7fa", high = "#187f88") +
        ggplot2::labs(title = "Cluster center profiles", subtitle = "Values use the original fitted scale", x = NULL, y = "Cluster", fill = "Center") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
    }
  } else if (kind == "aov") {
    full <- as.data.frame(stats::anova(x))
    full$term <- trimws(rownames(full))
    full$share <- full[["Sum Sq"]] / sum(full[["Sum Sq"]]) * 100
    dat <- full
    p <- ggplot2::ggplot(dat, ggplot2::aes(.data$share, stats::reorder(.data$term, .data$share))) +
      ggplot2::geom_col(fill = "#187f88", width = .65) + ggplot2::labs(title = "Where does the variation go?",
        subtitle = "Sequential (Type I) sums of squares, including residual variation", x = "Share of total sum of squares (%)", y = NULL)
  } else if (kind == "ts") {
    m <- if (is.null(dim(x))) matrix(as.numeric(x), ncol = 1L) else unclass(x)
    if (type == "series") {
      nc <- ncol(m)
      idx <- seq_len(nrow(m))
      tsp <- attr(x, "tsp", exact = TRUE)
      nm <- colnames(m)
      if (is.null(nm)) nm <- paste("Series", seq_len(ncol(m)))
      nm <- make.unique(nm)
      value <- as.vector(m[idx, seq_len(nc), drop = FALSE]); value[!is.finite(value)] <- NA_real_
      dat <- data.frame(time = rep(tsp[1L] + (idx - 1) / tsp[3L], nc), value = value, series = rep(nm, each = length(idx)))
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$time, .data$value)) + ggplot2::geom_line(colour = "#187f88", linewidth = .7) +
        ggplot2::facet_wrap(~series, scales = "free_y") + ggplot2::labs(title = "Time trend", x = "Time", y = "Value",
          subtitle = paste("All", length(idx), "stored time positions | All", nc, "series"))
    } else {
      v <- m[seq_len(min(nrow(m), limits$max_plot_points)), 1L]; v[!is.finite(v)] <- NA_real_
      if (sum(is.finite(v)) < 3L || length(unique(v[is.finite(v)])) < 2L) stop("ACF needs at least three finite values and nonzero variation in the leading window.", call. = FALSE)
      a <- stats::acf(v, plot = FALSE, na.action = stats::na.pass, lag.max = min(40L, length(v) - 1L))
      dat <- data.frame(lag = as.numeric(a$lag)[-1L], correlation = as.numeric(a$acf)[-1L])
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$lag, .data$correlation)) + ggplot2::geom_hline(yintercept = 0, colour = "#9da9b6") +
        ggplot2::geom_segment(ggplot2::aes(xend = .data$lag, yend = 0), colour = "#187f88", linewidth = .8, na.rm = TRUE) +
        ggplot2::labs(title = "Autocorrelation", subtitle = paste("First series |", length(v), "contiguous leading observations; non-finite values treated as missing"), x = "Lag (observations)", y = "ACF")
    }
  } else if (kind == "data_frame") {
    profile <- rr_data_profile(x, limits)
    plain <- rr_plain(x)
    if (type == "missingness") {
      dat <- profile$data
      if (!nrow(dat) || !any(is.finite(dat$missing_percent))) stop("No atomic variables available for a missingness plot.", call. = FALSE)
      dat$variable <- paste0(dat$column, ": ", dat$variable)
      p <- ggplot2::ggplot(dat, ggplot2::aes(.data$missing_percent, stats::reorder(.data$variable, .data$missing_percent))) +
        ggplot2::geom_col(fill = "#187f88", width = .65, na.rm = TRUE) + ggplot2::scale_x_continuous(limits = c(0, 100)) +
        ggplot2::labs(title = "Check completeness before analysis", subtitle = paste(length(profile$rows), "sampled rows | unsupported columns omitted from bars"),
          x = "Missing values (%)", y = NULL)
    } else {
      numeric <- rr_numeric_columns(x, limits)
      if (length(numeric) < 2L) stop("Choose a data frame with at least two plain numeric variables in the profiled columns.", call. = FALSE)
      if (type == "correlation") {
        numeric <- utils::head(numeric, min(12L, floor(sqrt(min(limits$max_cells, limits$max_plot_points)))))
        if (length(numeric) < 2L) stop("Increase max_cells and max_plot_points to at least four for a correlation view.", call. = FALSE)
        sampled <- lapply(plain[numeric], function(v) { v <- v[profile$rows]; v[!is.finite(v)] <- NA_real_; v })
        co <- suppressWarnings(stats::cor(as.data.frame(sampled), use = "pairwise.complete.obs"))
        nm <- paste0(numeric, ": ", rr_text(names(plain)[numeric], 40L))
        dat <- data.frame(x = rep(nm, each = length(nm)), y = rep(nm, length(nm)), value = as.vector(co))
        dat$x <- factor(dat$x, levels = nm)
        dat$y <- factor(dat$y, levels = rev(nm))
        p <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$y, fill = .data$value)) + ggplot2::geom_tile(colour = "white") +
          ggplot2::scale_fill_gradient2(low = "#cc7852", mid = "#f4f7fa", high = "#187f88", limits = c(-1, 1), na.value = "#aeb9c2") +
          ggplot2::labs(title = "Numeric relationships at a glance", subtitle = paste(length(numeric), "leading numeric columns |", length(profile$rows), "sampled rows | Pearson r, pairwise complete | grey: undefined"),
            x = NULL, y = NULL, fill = "r") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
      } else {
        ax <- rr_integer(as.numeric(axis_x), "X column", upper = length(plain)); ay <- rr_integer(as.numeric(axis_y), "Y column", upper = length(plain))
        if (!all(c(ax, ay) %in% numeric)) stop("Select two plain numeric columns from the profiled range.", call. = FALSE)
        if (ax == ay) stop("Choose two different variables.", call. = FALSE)
        dat <- data.frame(x = plain[[ax]][profile$rows], y = plain[[ay]][profile$rows])
        dat <- dat[is.finite(dat$x) & is.finite(dat$y), , drop = FALSE]
        p <- ggplot2::ggplot(dat, ggplot2::aes(.data$x, .data$y)) + ggplot2::geom_point(colour = "#187f88", alpha = .6, size = 2) +
          ggplot2::labs(title = "Inspect a variable relationship", subtitle = paste(nrow(dat), "finite pairs from", length(profile$rows), "sampled rows"),
            x = rr_text(names(plain)[ax]), y = rr_text(names(plain)[ay]))
      }
    }
  }
  rr_plot_style(p)
}


