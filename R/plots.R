# Statistical plots -----------------------------------------------------------

rr_plot <- function(x, type, limits, ...) {
  kind_report <- rr_report_kind(x)
  if (!is.null(kind_report) && kind_report %in% .rr_bio_families) {
    args <- list(...)
    if (type == "auto") {
      if (kind_report %in% .rr_bio_containers) {
        if (is.null(args$bio_source)) args$bio_source <- rr_bio_source(x, limits, args$assay, args$layer, lazy = TRUE)
        choices <- rr_bio_container_choices(args$bio_source)
      } else {
        if (kind_report %in% c("DGEExact", "DGELRT", "MArrayLM"))
          args$bio_result <- rr_bio_result_data(x, kind_report, limits, args$bio_coef, complete = TRUE)
        choices <- rr_bio_result_choices(x, kind_report, limits, args$bio_coef, result = args$bio_result)
      }
      if (!length(choices)) stop("This saved object has no available numeric chart.", call. = FALSE)
      type <- unname(choices[1L])
    }
    return(do.call(rr_report_plot, c(list(x = x, type = type, limits = limits), args)))
  }
  if (is.null(kind_report) && rr_kind(x) %in% c("vector", "array") && !type %in% c("heatmap", "coefficients", "residuals", "qq", "scale_location", "cooks")) {
    if (rr_kind(x) == "array") { x <- rr_plain(x); attributes(x) <- NULL }
    if (type %in% c("density", "histogram") && !rr_variable_type(x) %in% c("numeric", "time")) stop("Distribution requires a real numeric vector.", call. = FALSE)
    if (type == "bar" && rr_variable_type(x) != "category") stop("Bar plot requires logical, factor or character data.", call. = FALSE)
    if (type == "density" && !rr_violin_ready(as.numeric(x))) stop("Density requires at least two distinct finite values.", call. = FALSE)
    return(rr_data_plot(x, type, limits, ...))
  }
  if (identical(kind_report, "lm") && type %in% c("auto", "coefficients")) return(rr_effect_plot(x, limits, ...))
  if (identical(kind_report, "data_frame")) {
    if (type == "auto" && !length(rr_plot_columns(x, limits))) type <- "missingness"
    return(rr_report_plot(x, type, limits, ...))
  }
  if (!is.null(kind_report) && (kind_report != "lm" || type == "fitted")) {
    if (type == "auto") type <- if (kind_report %in% .rr_ordination_families)
      unname(rr_ordination_choices(rr_ordination(x))[1L]) else unname(rr_report(x, limits)$plots[1L])
    return(rr_report_plot(x, type, limits, ...))
  }
  type <- match.arg(type, c("auto", "histogram", "density", "bar", "heatmap", "coefficients",
                            "residuals", "qq", "scale_location", "cooks"))
  kind <- rr_kind(x)
  if (type == "auto") type <- if (identical(attr(x, "class", exact = TRUE), "lm")) "coefficients" else
    if (kind %in% c("matrix", "sparse_matrix")) "heatmap" else
    if (any(attr(x, "class", exact = TRUE) %in% c("factor", "ordered")) || typeof(x) %in% c("logical", "character")) "bar" else "histogram"
  if (type == "heatmap") {
    args <- list(...)
    range <- args$color_range
    args$color_range <- NULL
    slice <- do.call(rr_matrix, c(list(x = x, limits = limits), args))
    m <- slice$data
    if (!is.numeric(m) || is.complex(m)) stop("Heatmaps require a real numeric matrix.", call. = FALSE)
    if (nrow(m) > 200L || ncol(m) > 200L || length(m) > min(40000L, limits$max_cells)) stop("Heatmap limited to 200 rows/columns and 40,000 cells.", call. = FALSE)
    if (!length(m)) stop("No matching matrix cells to plot.", call. = FALSE)
    if (!is.null(range) && (!is.numeric(range) || length(range) != 2L || any(!is.finite(range)) || range[1L] >= range[2L])) stop("color_range must contain two finite increasing values.", call. = FALSE)
    dat <- data.frame(column = rep(seq_len(ncol(m)), each = nrow(m)), row = rep(seq_len(nrow(m)), ncol(m)), value = as.vector(m))
    dat$value[!is.finite(dat$value)] <- NA_real_
    plot <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$column, y = .data$row, fill = .data$value)) +
      ggplot2::geom_tile() + ggplot2::scale_y_reverse() +
      ggplot2::scale_fill_gradientn(colours = c("#174b78", "#e7f1f2", "#db713b"), limits = range, na.value = "#929aaa") +
      ggplot2::labs(x = "Column within selected window", y = "Row within selected window", fill = "Value",
        title = "Selected matrix window", subtitle = "Grey = NA, NaN, Inf or values outside the colour range")
  } else if (type %in% c("coefficients", "residuals", "qq", "scale_location", "cooks")) {
    if (type == "coefficients") {
      dat <- rr_lm(x, limits)$coefficients
      plot <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = stats::reorder(.data$term, .data$estimate))) +
        ggplot2::geom_vline(xintercept = 0, colour = "#929aaa", linetype = 2) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$conf.low, xend = .data$conf.high, yend = stats::reorder(.data$term, .data$estimate)), na.rm = TRUE, colour = "#477984") +
        ggplot2::geom_point(na.rm = TRUE, colour = "#1a7e8c", size = 3) +
        ggplot2::labs(x = "Estimate and 95% confidence interval", y = NULL, title = "Model coefficients")
    } else {
      d <- rr_lm_diagnostics(x, limits)
      dat <- d$data
      subtitle <- if (d$sampled) paste(nrow(dat), "evenly spaced observations of", d$population) else "All fitted observations"
      plot <- switch(type,
        residuals = ggplot2::ggplot(dat, ggplot2::aes(.data$fitted, .data$residual)) + ggplot2::geom_point(colour = "#1a7e8c") + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::labs(x = "Fitted", y = "Residual", title = "Residuals vs fitted"),
        qq = ggplot2::ggplot(dat, ggplot2::aes(sample = .data$standard)) + ggplot2::stat_qq(na.rm = TRUE, colour = "#1a7e8c") + ggplot2::stat_qq_line(na.rm = TRUE) + ggplot2::labs(x = "Theoretical quantile", y = "Standardized residual", title = "Normal Q-Q"),
        scale_location = ggplot2::ggplot(dat, ggplot2::aes(.data$fitted, .data$scale_location)) + ggplot2::geom_point(na.rm = TRUE, colour = "#1a7e8c") + ggplot2::labs(x = "Fitted", y = "sqrt(|standardized residual|)", title = "Scale-Location"),
        cooks = ggplot2::ggplot(dat, ggplot2::aes(.data$index, .data$cook)) + ggplot2::geom_col(na.rm = TRUE, fill = "#1a7e8c") + ggplot2::labs(x = "Observation", y = "Cook's distance", title = "Influence"))
      plot <- plot + ggplot2::labs(subtitle = subtitle)
    }
  } else {
    s <- rr_sample(x, limits)
    v <- s$value
    subtitle <- paste(s$used, if (s$sampled) "sampled values of" else "values of", s$population,
                      "| evenly spaced sampling; missing/non-finite values excluded")
    if (type %in% c("histogram", "density")) {
      if (!is.numeric(v) || is.complex(v)) stop("Distribution requires a real numeric vector.", call. = FALSE)
      v <- v[is.finite(v)]
      if (!length(v)) stop("No finite values to plot.", call. = FALSE)
      if (type == "density" && (length(v) < 2L || length(unique(v)) < 2L)) stop("Density requires at least two distinct finite values.", call. = FALSE)
      plot <- ggplot2::ggplot(data.frame(value = v), ggplot2::aes(.data$value)) +
        (if (type == "histogram") ggplot2::geom_histogram(bins = min(30L, max(1L, length(unique(v)))), fill = "#1a7e8c", colour = "white") else ggplot2::geom_density(fill = "#b2d8db", colour = "#1a7e8c")) +
        ggplot2::labs(x = "Value", y = if (type == "histogram") "Count" else "Density", title = "Distribution", subtitle = subtitle)
    } else {
      freq <- rr_summary(x, limits)$frequencies
      if (is.null(freq) || !length(freq)) stop("Bar plot requires logical, factor or character data.", call. = FALSE)
      dat <- data.frame(category = names(freq), count = as.numeric(freq))
      subtitle <- paste(s$used, if (s$sampled) "sampled values of" else "values of", s$population,
                        "| evenly spaced sampling; missing categories included")
      plot <- ggplot2::ggplot(dat, ggplot2::aes(stats::reorder(.data$category, .data$count), .data$count)) +
        ggplot2::geom_col(fill = "#1a7e8c") + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Sample count", title = "Most frequent categories", subtitle = subtitle)
    }
  }
  plot + ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))
}

# ggplot2 resolves .data in its data mask; this declaration informs R CMD check.
utils::globalVariables(".data")


