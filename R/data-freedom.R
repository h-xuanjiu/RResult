# Explicit grouping and faceting, with complete data and statistical summaries.

rr_flexible_data_plot <- function(x, type, limits, axis_x, axis_y, aggregation, bins, show_points,
                                  colour_by, facet_by, x_as_category, y_as_category, facet_scales, show_ci) {
  facet_scales <- match.arg(facet_scales, c("fixed", "free_y", "free_x", "free"))
  for (flag in list(show_points, show_ci)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag))
    stop("Chart switches must be TRUE or FALSE.", call. = FALSE)
  s <- rr_data_selection(x, limits, axis_x, axis_y, colour_by, facet_by, x_as_category, y_as_category)
  choices <- rr_data_selection_choices(s)
  if (type == "relationship") type <- "scatter"
  if (type == "auto") type <- unname(choices[1L])
  if (!length(type) || is.na(type) || !type %in% unname(choices)) stop("This chart is incompatible with the selected variables and grouping.", call. = FALSE)
  d <- s$data
  keep <- if (s$tx == "category") !is.na(d$x) else is.finite(as.numeric(d$x))
  if (!is.null(s$ty)) keep <- keep & if (s$ty == "category") !is.na(d$y) else is.finite(d$y)
  for (key in intersect(c("colour", "facet"), names(d))) keep <- keep & !is.na(d[[key]])
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) stop("No finite complete observations for the selected variables and grouping.", call. = FALSE)
  horizontal <- identical(s$tx, "numeric") && identical(s$ty, "category")
  if (horizontal) {
    value <- d$x; d$x <- d$y; d$y <- value
    label <- s$xlab; s$xlab <- s$ylab; s$ylab <- label
    s$tx <- "category"; s$ty <- "numeric"
  }
  d <- droplevels(d)
  coloured <- "colour" %in% names(d)
  if (coloured && identical(s$tx, "category") && identical(s$ty, "category"))
    stop("Two-category charts already use Y as their group. Use facet_by for an additional grouping variable.", call. = FALSE)
  caption <- paste(nrow(d), "complete observations of", s$population,
    "| All rows used; missing / non-finite selected values excluded")
  colour_map <- function(p, fill = FALSE) {
    if (!coloured) return(p)
    if (fill) p + ggplot2::aes(colour = .data$colour, fill = .data$colour) else p + ggplot2::aes(colour = .data$colour)
  }
  if (type %in% c("scatter", "line", "smooth", "bin2d")) {
    if (type == "line") d <- d[order(d$x), , drop = FALSE]
    p <- colour_map(ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)))
    point <- if (coloured) ggplot2::geom_point(alpha = .65, size = 2.4) else ggplot2::geom_point(colour = "#187f88", alpha = .65, size = 2.4)
    if (type == "scatter") p <- p + point
    if (type == "line") p <- p + if (coloured) ggplot2::geom_line(linewidth = .8) else ggplot2::geom_line(colour = "#187f88", linewidth = .8)
    if (type == "smooth") {
      if (show_points) p <- p + point
      p <- p + if (coloured) ggplot2::geom_smooth(ggplot2::aes(fill = .data$colour), method = "lm", formula = y ~ x, se = show_ci, alpha = .14) else
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = show_ci, colour = "#187f88", fill = "#b2d8db", alpha = .25)
      caption <- paste(caption, "| Linear trends and optional 95% pointwise confidence bands fitted to displayed observations")
    }
    if (type == "bin2d") p <- p + ggplot2::geom_bin_2d(bins = rr_integer(as.numeric(bins), "bins", 2, 100))
    p <- p + ggplot2::labs(title = switch(type, scatter = "Variable relationship", line = "Values ordered by X", smooth = "Relationship with a fitted linear trend", bin2d = "Observation density by two-dimensional bins"), x = s$xlab, y = s$ylab)
  } else if (type %in% c("histogram", "freqpoly", "density", "ecdf")) {
    p <- colour_map(ggplot2::ggplot(d, ggplot2::aes(.data$x)), fill = type %in% c("histogram", "density"))
    b <- rr_integer(as.numeric(bins), "bins", 2, 100)
    layer <- switch(type,
      histogram = if (coloured) ggplot2::geom_histogram(bins = b, position = "identity", alpha = .35) else ggplot2::geom_histogram(bins = b, fill = "#187f88", colour = "white"),
      freqpoly = if (coloured) ggplot2::geom_freqpoly(bins = b, linewidth = .9) else ggplot2::geom_freqpoly(bins = b, colour = "#187f88", linewidth = .9),
      density = if (coloured) ggplot2::geom_density(alpha = .18, linewidth = .9) else ggplot2::geom_density(fill = "#b2d8db", colour = "#187f88", alpha = .4),
      ecdf = if (coloured) ggplot2::stat_ecdf(geom = "step") else ggplot2::stat_ecdf(geom = "step", colour = "#187f88"))
    p <- p + layer + ggplot2::labs(title = switch(type, histogram = "Distribution", freqpoly = "Frequency polygons", density = "Density curves", ecdf = "Cumulative distribution"),
      x = s$xlab, y = if (type == "density") "Density" else if (type == "ecdf") "Cumulative proportion" else "Count")
  } else if (type %in% c("boxplot", "violin", "jitter", "summary_bar", "mean_se")) {
    single <- is.null(s$ty)
    if (single) { d$y <- d$x; d$x <- factor("All values") }
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y))
    p <- if (coloured) colour_map(p, TRUE) else p + ggplot2::aes(fill = .data$x)
    if (type %in% c("summary_bar", "mean_se")) {
      aggregation <- match.arg(aggregation, c("mean", "median", "sum"))
      fun <- if (type == "mean_se") mean else switch(aggregation, mean = mean, median = stats::median, sum = sum)
      keys <- intersect(c("x", "colour", "facet"), names(d))
      a <- stats::aggregate(list(value = d$y), d[keys], function(v) c(y = fun(v), n = length(v), se = stats::sd(v) / sqrt(length(v))))
      summary <- a[keys]; summary$y <- a$value[, "y"]; summary$n <- a$value[, "n"]; summary$se <- a$value[, "se"]
      summary$se[!is.finite(summary$se)] <- NA_real_
      p <- ggplot2::ggplot(summary, ggplot2::aes(.data$x, .data$y))
      p <- if (coloured) colour_map(p, TRUE) else p + ggplot2::aes(fill = .data$x)
      dodge <- ggplot2::position_dodge(width = .65)
      if (type == "summary_bar") p <- p + ggplot2::geom_col(position = dodge, width = .6) else {
        # Keep every group in both layers while computing their dodge offsets.
        # Missing intervals are omitted when drawing, after positions align.
        p <- p + ggplot2::geom_point(position = dodge, size = 3) +
          ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$y - .data$se, ymax = .data$y + .data$se),
            position = dodge, width = .15, na.rm = TRUE)
        caption <- paste(caption, "| Mean +/- one standard error of displayed observations; SE assumes independent observations and is unavailable when n < 2")
      }
    } else {
      groups <- rr_data_groups(d[intersect(c("x", "colour", "facet"), names(d))])
      if (type == "violin") {
        if (!rr_violin_ready(d$y, groups)) stop("Violin plots require at least two distinct finite values in every displayed group.", call. = FALSE)
        p <- p + ggplot2::geom_violin(alpha = .35, linewidth = .5)
      }
      if (type %in% c("boxplot", "violin")) p <- p + ggplot2::geom_boxplot(width = if (type == "violin") .16 else .55, alpha = .65,
        outlier.shape = if (show_points) NA else 16)
      if (type == "jitter" || show_points) p <- p + if (coloured)
        ggplot2::geom_point(position = ggplot2::position_jitterdodge(jitter.width = .13, seed = 1), alpha = .55, size = 1.7) else
          ggplot2::geom_point(position = ggplot2::position_jitter(width = .13, height = 0, seed = 1), colour = "#187f88", alpha = .55, size = 1.7)
    }
    if (!coloured) p <- p + ggplot2::guides(fill = "none")
    p <- p + ggplot2::labs(title = switch(type, boxplot = "Distribution by group", violin = "Density and quartiles by group", jitter = "Individual observations", summary_bar = paste("Group", aggregation), mean_se = "Group means and standard errors"),
      x = if (single) NULL else s$xlab, y = if (single) s$xlab else s$ylab)
  } else {
    keys <- intersect(c("x", "y", "colour", "facet"), names(d))
    counts <- stats::aggregate(list(count = rep(1L, nrow(d))), d[keys], sum)
    two <- !is.null(s$ty)
    if (type == "proportion") {
      denominator_keys <- intersect(c(if (two || coloured) "x", "facet"), names(counts))
      denominator <- rr_data_groups(counts[denominator_keys])
      counts$count <- counts$count / stats::ave(counts$count, denominator, FUN = sum)
    }
    p <- ggplot2::ggplot(counts, ggplot2::aes(.data$x, .data$count))
    if (type %in% c("count_heatmap", "count_bubble")) {
      caption <- paste(caption, "| Unobserved category combinations have zero count")
      p <- ggplot2::ggplot(counts, ggplot2::aes(.data$x, .data$y))
      p <- if (type == "count_heatmap") p + ggplot2::geom_tile(ggplot2::aes(fill = .data$count), colour = "white") else
        p + ggplot2::geom_point(ggplot2::aes(size = .data$count, colour = .data$count)) + ggplot2::scale_size_area(max_size = 14)
      p <- p + ggplot2::labs(y = s$ylab)
      p <- if (type == "count_heatmap") p + ggplot2::labs(fill = "Count") else p + ggplot2::labs(colour = "Count", size = "Count")
    } else {
      if (two) p <- p + ggplot2::aes(fill = .data$y) else if (coloured) p <- p + ggplot2::aes(fill = .data$colour) else p <- p + ggplot2::aes(fill = .data$x)
      p <- p + ggplot2::geom_col(position = if (type == "proportion") "stack" else "dodge", width = .7)
      if (!two && !coloured) p <- p + ggplot2::guides(fill = "none")
      if (type == "proportion") p <- p + ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"))
      p <- p + ggplot2::labs(y = if (type == "proportion") "Proportion" else "Count", fill = if (two) s$ylab else NULL)
    }
    p <- p + ggplot2::labs(title = if (type == "proportion") "Category proportions" else "Observed category counts", x = s$xlab)
  }
  if ("facet" %in% names(d)) p <- p + ggplot2::facet_wrap(~facet, scales = facet_scales, ncol = min(3L, nlevels(d$facet)))
  if (coloured) {
    label <- names(rr_plain(x))[as.integer(colour_by)]
    mappings <- union(names(p$mapping), unlist(lapply(p$layers, function(layer) names(layer$mapping))))
    labels <- stats::setNames(rep(list(label), length(intersect(c("colour", "fill"), mappings))), intersect(c("colour", "fill"), mappings))
    if (length(labels)) p <- p + do.call(ggplot2::labs, labels)
  }
  if (horizontal) p <- p + ggplot2::coord_flip()
  rr_plot_style(p + ggplot2::labs(subtitle = caption))
}
