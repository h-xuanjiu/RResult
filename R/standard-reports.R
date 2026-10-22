# Standard stats/base object formats ------------------------------------------

rr_nls_result <- function(x, limits) {
  sm <- utils::getFromNamespace("summary.nls", "stats")(x)
  m <- sm$coefficients
  q <- if (sm$df[2L] > 0) stats::qt(.975, sm$df[2L]) else NA_real_
  co <- data.frame(term = rownames(m), estimate = m[, 1L], std.error = m[, 2L], statistic = m[, 3L], p.value = m[, 4L],
    conf.low = m[, 1L] - q * m[, 2L], conf.high = m[, 1L] + q * m[, 2L], row.names = NULL)
  list(summary = sm, coefficients = co)
}

rr_dist_window <- function(x, limits) {
  n <- attr(x, "Size", exact = TRUE)
  cap <- min(n, limits$max_rows, limits$max_cols, floor(sqrt(min(limits$max_cells, limits$max_plot_points))))
  idx <- seq_len(cap)
  g <- expand.grid(row = idx, column = idx)
  i <- pmax(g$row, g$column); j <- pmin(g$row, g$column)
  offset <- as.double(n) * (j - 1) - as.double(j) * (j - 1) / 2 + i - j
  value <- numeric(nrow(g))
  value[i != j] <- rr_plain(x)[offset[i != j]]
  labels <- attr(x, "Labels", exact = TRUE)
  labels <- if (is.null(labels)) as.character(idx) else paste0(idx, ": ", rr_text(labels[idx], 40L))
  g$from <- factor(labels[g$column], levels = labels)
  g$to <- factor(labels[g$row], levels = rev(labels))
  g$distance <- value
  g
}

rr_table_cells <- function(x, limits) {
  d <- dim(x)
  idx <- seq_along(x)
  positions <- arrayInd(idx, .dim = d)
  dn <- dimnames(x)
  labels <- vapply(seq_along(idx), function(i) paste(vapply(seq_along(d), function(j) {
    if (is.null(dn[[j]])) as.character(positions[i, j]) else dn[[j]][positions[i, j]]
  }, character(1)), collapse = " / "), character(1))
  data.frame(cell = paste0(idx, ": ", labels), value = as.numeric(rr_plain(x)[idx]))
}

rr_standard_report <- function(x, kind, limits) {
  if (kind == "density") {
    idx <- seq_along(x$x)
    mode <- which.max(x$y)
    return(rr_new_report(kind, "Inspect a saved density estimate", paste(deparse(x$call), collapse = " "),
      list(Observations = x$n, Bandwidth = x$bw, "Grid points" = length(x$x), "Grid peak at" = x$x[mode]),
      c("The curve uses the stored density grid and bandwidth; it is not re-estimated from the grid heights.",
        paste(length(idx), "evenly spaced saved grid points displayed.")),
      list(Density = data.frame(x = x$x[idx], density = x$y[idx])), c("Saved density curve" = "density_curve"),
      "Density heights are not frequencies. The grid peak is a numerical approximation to the mode.", limits))
  }
  if (kind == "dist") {
    n <- attr(x, "Size", exact = TRUE)
    idx <- rr_sample_indices(length(x), limits$max_plot_points)
    v <- rr_plain(x)[idx]; finite <- v[is.finite(v)]
    window <- rr_dist_window(x, limits)
    window_table <- window[c("from", "to", "distance")]
    attr(window_table, "rresult_scope") <- paste("Distance preview: first", nlevels(window$from), "of", n,
      "observations. CSV exports this window, not every stored pair.")
    return(rr_new_report(kind, "Inspect pairwise distances", "Bounded distance window and stored-pair distribution",
      list(Observations = n, Pairs = length(x), Method = attr(x, "method", exact = TRUE), "Sample mean distance" = if (length(finite)) mean(finite) else NA_real_),
      c(paste(length(idx), "evenly spaced stored pairs sampled for the distribution."),
        paste(length(unique(window$from)), "leading observations in the heatmap; original condensed storage is retained.")),
      list(Window = window_table, Summary = data.frame(metric = c("Minimum", "Median", "Maximum"),
        value = if (length(finite)) c(min(finite), stats::median(finite), max(finite)) else rep(NA_real_, 3L))),
      c("Distance window" = "distance_heatmap", "Stored-pair distribution" = "distance_distribution"),
      "Distances are dependent pairwise quantities. The histogram describes stored distances, not independent observations.", limits))
  }
  if (kind == "hclust") {
    n <- length(x$order)
    idx <- seq_len(nrow(x$merge))
    tree_cap <- min(limits$max_rows, floor(limits$max_plot_points / 3))
    return(rr_new_report(kind, "Read the fitted hierarchy", paste("Linkage:", x$method, "| Distance:", x$dist.method),
      list(Observations = n, Merges = nrow(x$merge), Linkage = x$method, "Final merge height" = utils::tail(x$height, 1L)),
      c("The hierarchy and merge heights are taken directly from the saved hclust result.",
        if (n <= tree_cap) "The dendrogram displays every original leaf." else paste("Dendrogram leaf limit:", tree_cap, "; use merge heights or cluster sizes for this object.")),
      list(Merges = data.frame(step = idx, left = x$merge[idx, 1L], right = x$merge[idx, 2L], height = x$height[idx])),
      c(if (n <= tree_cap) c("Dendrogram" = "dendrogram"), "Merge heights" = "merge_heights", "Cut-tree cluster sizes" = "cut_clusters"),
      c("A negative merge index denotes an original observation; a positive index denotes an earlier merge.",
        "Height is the linkage criterion, not necessarily an ordinary distance. Centroid/median linkage may have height reversals."), limits))
  }
  if (kind == "table") {
    values <- rr_plain(x)
    negative <- any(values < 0, na.rm = TRUE)
    return(rr_new_report(kind, "Inspect a frequency or weighted table", paste(length(dim(x)), "dimensions |", paste(dim(x), collapse = " x ")),
      list(Dimensions = length(dim(x)), Cells = length(x), "Stored total" = sum(values, na.rm = TRUE), "Missing cells" = sum(is.na(values))),
      c("Cells retain their stored values, including zero cells; counts are not reconstructed as raw observations.",
        if (length(dim(x)) > 2L) "For a multiway table, cell bars retain every stored cell with all dimensions in the label." else "Two-way proportions use the full row total, including columns outside the displayed window."),
      list(Cells = rr_table_cells(x, limits)), c("Stored cell values" = "table_bar", if (length(dim(x)) == 2L) c("Count / weight heatmap" = "table_heatmap"),
        if (length(dim(x)) <= 2L && !negative && sum(values, na.rm = TRUE) > 0) c("Within-row proportions" = "table_proportion")),
      c("Fractional xtabs weights remain fractional. Negative stored weights do not have a proportion view.",
        "Cell bars and result tables retain every stored cell; two-way heatmaps use an explicitly labelled leading window. Multiway tables are not silently collapsed over other dimensions."), limits))
  }
  r <- rr_nls_result(x, limits)
  sm <- r$summary
  rr_new_report(kind, "Inspect a nonlinear least-squares fit", paste(deparse(sm$formula), collapse = " "),
    list(Observations = sum(sm$df), Parameters = sm$df[1L], "Residual df" = sm$df[2L], "Residual standard error" = sm$sigma),
    c(paste("Fitting", if (isTRUE(x$convInfo$isConv)) "converged." else "did not converge; check the saved convergence information."),
      "Parameters and residuals come from the saved fit; the model is never refitted."),
    list(Parameters = r$coefficients), c("Parameter intervals" = "parameters", "Observed vs fitted" = "nls_fitted", "Residuals vs fitted" = "nls_residuals"),
    c("Intervals use a local linear approximation with a t reference distribution. They are not profile-likelihood confidence intervals.",
      "Response residuals are plotted on the original response scale, including for weighted fits."), limits)
}

rr_standard_plot <- function(x, type, limits, clusters = NULL) {
  kind <- rr_report_kind(x)
  allowed <- rr_standard_report(x, kind, limits)$plots
  if (!type %in% allowed) stop("This plot is unavailable for the object or exceeds its display limits.", call. = FALSE)
  if (kind == "density") {
    idx <- seq_along(x$x)
    d <- data.frame(x = x$x[idx], density = x$y[idx])
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$density)) + ggplot2::geom_area(fill = "#187f88", alpha = .2) +
      ggplot2::geom_line(colour = "#187f88", linewidth = .9) + ggplot2::labs(title = "Saved density estimate", subtitle = paste("Stored bandwidth:", rr_fmt(x$bw)), x = "Value", y = "Density")
  } else if (kind == "dist") {
    if (type == "distance_heatmap") {
      d <- rr_dist_window(x, limits)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$from, .data$to, fill = .data$distance)) + ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "#eef6f6", high = "#187f88", na.value = "#a7b3bd") +
        ggplot2::labs(title = "Leading distance window", subtitle = paste("First", nlevels(d$from), "of", attr(x, "Size", exact = TRUE), "observations"),
          x = NULL, y = NULL, fill = "Distance") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      idx <- rr_sample_indices(length(x), limits$max_plot_points)
      value <- rr_plain(x)[idx]; value <- value[is.finite(value)]
      if (!length(value)) stop("No finite stored distances to plot.", call. = FALSE)
      p <- ggplot2::ggplot(data.frame(distance = value), ggplot2::aes(.data$distance)) + ggplot2::geom_histogram(bins = 25, fill = "#187f88", colour = "white") +
        ggplot2::labs(title = "Stored-pair distance distribution", subtitle = paste(length(idx), "evenly spaced stored pairs of", length(x)), x = "Distance", y = "Sampled pairs")
    }
  } else if (kind == "hclust") {
    n <- length(x$order)
    if (type == "dendrogram") {
      positions <- numeric(n); positions[x$order] <- seq_len(n)
      centers <- numeric(n - 1L)
      rows <- vector("list", n - 1L)
      for (i in seq_len(n - 1L)) {
        children <- x$merge[i, ]
        xx <- vapply(children, function(j) if (j < 0) positions[-j] else centers[j], numeric(1))
        yy <- vapply(children, function(j) if (j < 0) 0 else x$height[j], numeric(1))
        centers[i] <- mean(xx)
        rows[[i]] <- data.frame(x = c(xx, xx[1L]), xend = c(xx, xx[2L]), y = c(yy, x$height[i]), yend = rep(x$height[i], 3L))
      }
      d <- do.call(rbind, rows)
      labels <- if (is.null(x$labels)) as.character(x$order) else rr_text(x$labels[x$order], 35L)
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y, xend = .data$xend, yend = .data$yend)) + ggplot2::geom_segment(colour = "#187f88", linewidth = .7) +
        ggplot2::scale_x_continuous(breaks = seq_len(n), labels = labels) + ggplot2::labs(title = "Hierarchical clustering", subtitle = paste("All", n, "leaves |", x$method), x = NULL, y = "Merge height") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
    } else if (type == "cut_clusters") {
      if (is.null(clusters)) clusters <- min(3L, n)
      k <- rr_integer(as.numeric(clusters), "clusters", upper = n)
      counts <- table(stats::cutree(x, k = k))
      d <- data.frame(cluster = factor(names(counts)), size = as.numeric(counts))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$cluster, .data$size, fill = .data$cluster)) + ggplot2::geom_col(width = .65) +
        ggplot2::guides(fill = "none") + ggplot2::labs(title = paste("Cut the saved tree into", k, "clusters"), x = "Cluster", y = "Observations")
    } else {
      idx <- seq_along(x$height)
      d <- data.frame(step = idx, height = x$height[idx])
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$step, .data$height)) + ggplot2::geom_line(colour = "#187f88") +
        ggplot2::labs(title = "Merge height progression", subtitle = paste("All", length(idx), "stored merges"), x = "Merge step", y = "Height")
    }
  } else if (kind == "table") {
    if (!length(x)) stop("No stored cells to plot.", call. = FALSE)
    if (type == "table_bar" || length(dim(x)) != 2L) {
      d <- rr_table_cells(x, limits)
      if (type == "table_proportion") d$value <- if (sum(x, na.rm = TRUE) > 0) d$value / sum(x, na.rm = TRUE) else NA_real_
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$value, stats::reorder(.data$cell, .data$value))) + ggplot2::geom_col(fill = "#187f88") +
        ggplot2::labs(title = "Stored table cells", x = if (type == "table_proportion") "Proportion of full total" else "Count / weight", y = NULL)
    } else {
      nr <- min(nrow(x), limits$max_rows, limits$max_plot_points, limits$max_cells)
      nc <- min(ncol(x), limits$max_cols, floor(min(limits$max_cells, limits$max_plot_points) / nr))
      m <- unclass(x)[seq_len(nr), seq_len(nc), drop = FALSE]
      if (type == "table_proportion") {
        denom <- rowSums(unclass(x), na.rm = TRUE)[seq_len(nr)]
        m <- m / ifelse(denom > 0, denom, NA_real_)
      }
      dn <- dimnames(x)
      labels <- lapply(seq_len(2L), function(i) { n <- c(nr, nc)[i]; if (is.null(dn[[i]])) as.character(seq_len(n)) else paste0(seq_len(n), ": ", dn[[i]][seq_len(n)]) })
      d <- data.frame(row = factor(rep(labels[[1L]], nc), levels = rev(labels[[1L]])), column = factor(rep(labels[[2L]], each = nr), levels = labels[[2L]]), value = as.vector(m))
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$column, .data$row, fill = .data$value)) + ggplot2::geom_tile(colour = "white") +
        ggplot2::scale_fill_gradient(low = "#eef6f6", high = "#187f88", na.value = "#a7b3bd") +
        ggplot2::labs(title = if (type == "table_proportion") "Proportions within the full row" else "Stored table counts / weights",
          subtitle = paste("Displayed window: first", nr, "of", nrow(x), "rows | first", nc, "of", ncol(x), "columns"),
          x = names(dn)[2L], y = names(dn)[1L], fill = if (type == "table_proportion") "Proportion" else "Value")
    }
  } else {
    if (type == "parameters") {
      d <- rr_nls_result(x, limits)$coefficients
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$term)) + ggplot2::geom_vline(xintercept = 0, colour = "#a7b3bd", linetype = 2) +
        ggplot2::geom_segment(ggplot2::aes(x = .data$conf.low, xend = .data$conf.high, yend = .data$term), colour = "#187f88", na.rm = TRUE) +
        ggplot2::geom_point(colour = "#187f88") + ggplot2::labs(title = "Nonlinear model parameters", subtitle = "95% local-linear t intervals", x = "Estimate", y = NULL)
    } else {
      fitted <- x$m$fitted(); observed <- x$m$lhs()
      idx <- seq_along(fitted)
      d <- data.frame(fitted = fitted[idx], observed = observed[idx], residual = (observed - fitted)[idx])
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$fitted, if (type == "nls_fitted") .data$observed else .data$residual)) + ggplot2::geom_point(colour = "#187f88") +
        ggplot2::labs(title = if (type == "nls_fitted") "Observed vs fitted" else "Response residuals vs fitted", subtitle = paste("All", length(idx), "fitted observations"), x = "Fitted response", y = if (type == "nls_fitted") "Observed response" else "Response residual")
      p <- p + if (type == "nls_fitted") ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#a7b3bd", linetype = 2) else ggplot2::geom_hline(yintercept = 0, colour = "#a7b3bd", linetype = 2)
    }
  }
  rr_plot_style(p)
}
