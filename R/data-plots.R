# Data semantics and compatible chart choices --------------------------------

rr_variable_type <- function(x) {
  if (!rr_simple(x) || !is.null(attr(x, "dim", exact = TRUE))) return("unsupported")
  cls <- attr(x, "class", exact = TRUE)
  if (any(cls %in% c("Date", "POSIXct"))) return("time")
  if (any(cls %in% c("factor", "ordered")) || is.character(x) || is.logical(x)) return("category")
  if (is.numeric(x) && !is.complex(x)) return("numeric")
  "unsupported"
}

rr_plot_columns <- function(x, limits) {
  p <- rr_plain(x)
  idx <- seq_along(p)
  types <- vapply(p[idx], rr_variable_type, character(1))
  stats::setNames(types[types != "unsupported"], idx[types != "unsupported"])
}

rr_default_axes <- function(x, limits) {
  types <- rr_plot_columns(x, limits)
  numeric <- as.integer(names(types)[types == "numeric"])
  time <- as.integer(names(types)[types == "time"])
  category <- as.integer(names(types)[types == "category"])
  if (length(time) && length(numeric)) return(c(time[1L], numeric[1L]))
  if (length(numeric) >= 2L) return(numeric[1:2])
  if (length(category) && length(numeric)) return(c(category[1L], numeric[1L]))
  c(if (length(types)) as.integer(names(types)[1L]) else NA_integer_, 0L)
}

rr_plot_values <- function(v, idx) {
  type <- rr_variable_type(v)
  if (type == "category") {
    if (is.factor(v)) {
      code <- rr_plain(v)[idx]
      observed <- sort(unique(code[!is.na(code)]))
      labels <- make.unique(rr_text(attr(v, "levels", exact = TRUE)[observed], 80L))
      return(factor(labels[match(code, observed)], levels = labels, ordered = is.ordered(v)))
    }
    value <- as.character(rr_plain(v)[idx])
    observed <- unique(value[!is.na(value)])
    labels <- make.unique(rr_text(observed, 80L))
    return(factor(labels[match(value, observed)], levels = labels))
  }
  v[idx]
}

rr_data_selection <- function(x, limits, axis_x = NULL, axis_y = NULL,
                              colour_by = NULL, facet_by = NULL, x_as_category = FALSE, y_as_category = FALSE) {
  for (flag in list(x_as_category, y_as_category)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag))
    stop("Category switches must be TRUE or FALSE.", call. = FALSE)
  frame <- identical(rr_report_kind(x), "data_frame")
  if (!frame) {
    type <- rr_variable_type(x)
    if (type == "unsupported") stop("This data type has no statistical chart; use its value preview.", call. = FALSE)
    idx <- seq_along(x)
    value <- rr_plot_values(x, idx)
    if (x_as_category) { value <- factor(value); type <- "category" }
    return(list(data = data.frame(x = value), tx = type, ty = NULL,
      xlab = "Value", ylab = NULL, population = length(x), sampled = length(idx)))
  }
  p <- rr_plain(x)
  if (is.null(axis_x) || is.null(axis_y)) {
    defaults <- rr_default_axes(x, limits)
    if (is.null(axis_x)) axis_x <- defaults[1L]
    if (is.null(axis_y)) axis_y <- defaults[2L]
  }
  if (!length(p) || is.na(axis_x)) stop("No supported columns available for plotting.", call. = FALSE)
  ax <- rr_integer(as.numeric(axis_x), "X column", upper = length(p))
  ay <- rr_integer(as.numeric(axis_y), "Y column", lower = 0, upper = length(p))
  if (ay == ax) stop("Choose two different variables.", call. = FALSE)
  tx <- rr_variable_type(p[[ax]])
  ty <- if (ay) rr_variable_type(p[[ay]]) else NULL
  if (tx == "unsupported" || identical(ty, "unsupported")) stop("Select known numeric, categorical or time columns.", call. = FALSE)
  idx <- seq_len(rr_df_nrow(x))
  dat <- data.frame(x = rr_plot_values(p[[ax]], idx))
  if (ay) dat$y <- rr_plot_values(p[[ay]], idx)
  if (x_as_category) { dat$x <- factor(dat$x); tx <- "category" }
  if (ay && y_as_category) { dat$y <- factor(dat$y); ty <- "category" }
  for (key in c("colour", "facet")) {
    column <- if (key == "colour") colour_by else facet_by
    if (is.null(column) || identical(as.numeric(column), 0)) next
    column <- rr_integer(as.numeric(column), paste(key, "column"), upper = length(p))
    if (rr_variable_type(p[[column]]) != "category") stop("Colour and facet grouping require a categorical column. Convert numeric group codes to factors in R.", call. = FALSE)
    dat[[key]] <- rr_plot_values(p[[column]], idx)
  }
  list(data = dat, tx = tx, ty = ty, xlab = rr_text(names(p)[ax]), ylab = if (ay) rr_text(names(p)[ay]) else NULL,
    population = rr_df_nrow(x), sampled = length(idx))
}

rr_data_groups <- function(data) {
  if (!ncol(data)) return(factor(rep("All", nrow(data))))
  # Group observed combinations by integer codes. This avoids expanding the
  # category cross-product and keeps labels containing separators distinct.
  codes <- lapply(data, function(v) as.integer(factor(v)))
  keys <- do.call(paste, c(codes, list(sep = "/")))
  keys[!stats::complete.cases(data)] <- NA_character_
  factor(keys)
}

rr_violin_ready <- function(value, group = NULL) {
  if (is.null(group)) group <- rep("All", length(value))
  keep <- is.finite(value) & !is.na(group)
  groups <- split(value[keep], droplevels(factor(group[keep])))
  length(groups) > 0L && all(vapply(groups, function(v) length(v) >= 2L && length(unique(v)) >= 2L, logical(1)))
}

rr_data_choices <- function(x, limits, axis_x = NULL, axis_y = NULL,
                            colour_by = NULL, facet_by = NULL, x_as_category = FALSE, y_as_category = FALSE) {
  s <- tryCatch(rr_data_selection(x, limits, axis_x, axis_y, colour_by, facet_by, x_as_category, y_as_category), error = function(e) NULL)
  if (is.null(s)) return(character())
  rr_data_selection_choices(s)
}

# Plot validation and the choice list share the same selected observations.
# Selecting full columns twice needlessly repeats factorisation and copying.
rr_data_selection_choices <- function(s) {
  tx <- s$tx; ty <- s$ty; d <- s$data
  group_keys <- intersect(c("colour", "facet"), names(d))
  if (is.null(ty)) {
    if (tx == "category") return(c("Category counts" = "bar", "Category proportions" = "proportion"))
    if (tx == "time") return(c("Time distribution" = "histogram", "Cumulative distribution" = "ecdf"))
    distribution_groups <- if (length(group_keys)) rr_data_groups(d[group_keys]) else NULL
    varied <- rr_violin_ready(d$x, distribution_groups)
    return(c("Histogram" = "histogram", "Frequency polygon" = "freqpoly", if (varied) c("Density" = "density"), "Box plot" = "boxplot",
      if (varied) c("Violin + box plot" = "violin"), "Cumulative distribution" = "ecdf"))
  }
  if (tx %in% c("numeric", "time") && ty == "numeric") {
    finite <- is.finite(as.numeric(d$x)) & is.finite(d$y)
    groups <- rr_data_groups(d[group_keys])
    grouped_x <- split(d$x[finite], groups[finite], drop = TRUE)
    finite_n <- sum(finite)
    unique_x <- finite_n >= 2L && all(vapply(grouped_x, function(v) length(v) >= 2L && !anyDuplicated(v), logical(1)))
    smooth_ready <- tx == "numeric" && finite_n >= 3L && all(vapply(grouped_x, function(v) length(unique(v)) >= 3L, logical(1)))
    return(c(if (tx == "time" && unique_x) c("Time trend" = "line"), "Scatter plot" = "scatter",
      if (tx == "numeric" && unique_x) c("Ordered line" = "line"),
      if (tx == "numeric" && smooth_ready) c("Fitted linear trend" = "smooth"),
      if (tx == "numeric" && !"colour" %in% names(d)) c("Binned observation counts" = "bin2d")))
  }
  if ((tx == "category" && ty == "numeric") || (tx == "numeric" && ty == "category")) return(c("Grouped box plot" = "boxplot",
    if (if (tx == "category") rr_violin_ready(d$y, rr_data_groups(d[c("x", group_keys)])) else
      rr_violin_ready(d$x, rr_data_groups(d[c("y", group_keys)]))) c("Grouped violin + box plot" = "violin"),
    "Individual observations" = "jitter", "Group mean and standard error" = "mean_se", "Aggregated values (choose statistic)" = "summary_bar"))
  if (tx == "category" && ty == "category") return(c("Grouped counts" = "bar", "Within-X proportions" = "proportion", "Count heatmap" = "count_heatmap", "Count bubbles" = "count_bubble"))
  character()
}

rr_data_plot <- function(x, type, limits, axis_x = NULL, axis_y = NULL, aggregation = "mean", bins = 30L, show_points = TRUE,
                         colour_by = NULL, facet_by = NULL, x_as_category = FALSE, y_as_category = FALSE,
                         facet_scales = "fixed", show_ci = TRUE) {
  for (flag in list(x_as_category, y_as_category)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag))
    stop("Category switches must be TRUE or FALSE.", call. = FALSE)
  if ((!is.null(colour_by) && any(as.numeric(colour_by) != 0)) || (!is.null(facet_by) && any(as.numeric(facet_by) != 0)) ||
      isTRUE(x_as_category) || isTRUE(y_as_category) || type %in% c("freqpoly", "smooth", "bin2d", "mean_se", "count_bubble"))
    return(rr_flexible_data_plot(x, type, limits, axis_x, axis_y, aggregation, bins, show_points,
      colour_by, facet_by, x_as_category, y_as_category, facet_scales, show_ci))
  s <- rr_data_selection(x, limits, axis_x, axis_y)
  if (type %in% c("scatter", "relationship") && (!s$tx %in% c("numeric", "time") || !identical(s$ty, "numeric")))
    stop("Scatter plots require two numeric columns, or a time X and numeric Y column.", call. = FALSE)
  choices <- rr_data_selection_choices(s)
  if (type == "relationship") type <- "scatter"
  if (type == "auto") type <- unname(choices[1L])
  if (!length(type) || is.na(type) || !type %in% choices) stop("This chart is incompatible with the selected data. Available: ", paste(unname(choices), collapse = ", "), ". Violin plots require at least two distinct finite values in every group.", call. = FALSE)
  d <- s$data
  if (s$tx == "category") d <- d[!is.na(d$x), , drop = FALSE] else d <- d[is.finite(as.numeric(d$x)), , drop = FALSE]
  if (!is.null(s$ty)) d <- if (s$ty == "category") d[!is.na(d$y), , drop = FALSE] else d[is.finite(d$y), , drop = FALSE]
  if (!nrow(d)) stop("No finite or nonmissing observations to plot.", call. = FALSE)
  horizontal <- identical(s$tx, "numeric") && identical(s$ty, "category")
  if (horizontal) {
    d <- data.frame(x = d$y, y = d$x)
    label <- s$xlab; s$xlab <- s$ylab; s$ylab <- label
    s$tx <- "category"; s$ty <- "numeric"
  }
  d <- droplevels(d)
  caption <- paste(nrow(d), "complete observations of", s$population,
    "| All rows used; missing/non-finite selected values excluded")
  if (type %in% c("histogram", "density", "ecdf")) {
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x))
    p <- p + switch(type,
      histogram = ggplot2::geom_histogram(bins = rr_integer(as.numeric(bins), "bins", 2, 100), fill = "#187f88", colour = "white", linewidth = .3),
      density = ggplot2::geom_density(fill = "#187f88", alpha = .25, colour = "#187f88", linewidth = .9),
      ecdf = ggplot2::stat_ecdf(geom = "step", colour = "#187f88", linewidth = .9)) +
      ggplot2::labs(title = switch(type, histogram = "Distribution", density = "Density", ecdf = "Cumulative distribution"),
        x = s$xlab, y = switch(type, histogram = "Count", density = "Density", ecdf = "Cumulative proportion"))
  } else if (type %in% c("scatter", "line")) {
    if (type == "line") d <- d[order(d$x), , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y)) +
      (if (type == "scatter") ggplot2::geom_point(colour = "#187f88", alpha = .7, size = 2.5) else ggplot2::geom_line(colour = "#187f88", linewidth = .8)) +
      ggplot2::labs(title = if (type == "scatter") "Variable relationship" else "Values in X order", x = s$xlab, y = s$ylab)
    if (type == "line") caption <- paste(caption, "| Ordered by X; displayed points are connected")
  } else if (type %in% c("boxplot", "violin", "jitter", "summary_bar")) {
    grouped <- !is.null(s$ty)
    if (!grouped) d <- data.frame(x = factor("All values"), y = d$x)
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y, fill = .data$x))
    if (type == "summary_bar") {
      aggregation <- match.arg(aggregation, c("mean", "median", "sum"))
      values <- split(d$y, d$x)
      agg <- switch(aggregation, mean = mean, median = stats::median, sum = sum)
      summary <- data.frame(x = factor(names(values), levels = levels(d$x)), y = vapply(values, agg, numeric(1)), n = lengths(values))
      p <- ggplot2::ggplot(summary, ggplot2::aes(.data$x, .data$y, fill = .data$x)) + ggplot2::geom_col(width = .65) +
        ggplot2::labs(title = paste("Group", aggregation), y = paste(aggregation, "of", s$ylab), x = s$xlab)
      caption <- paste(caption, "| Bars show", aggregation, "of complete values; no uncertainty interval is inferred")
    } else {
      if (type == "violin") p <- p + ggplot2::geom_violin(trim = TRUE, alpha = .55, colour = "#187f88", linewidth = .5)
      if (type %in% c("violin", "boxplot")) p <- p + ggplot2::geom_boxplot(width = if (type == "violin") .14 else .5,
        alpha = .75, outlier.shape = if (isTRUE(show_points)) NA else 16, colour = "#304b58", linewidth = .55)
      if (type == "jitter" || isTRUE(show_points)) p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = .12, height = 0, seed = 1),
        colour = "#187f88", alpha = .5, size = 1.7)
      p <- p + ggplot2::labs(title = switch(type, boxplot = "Distribution by group", violin = "Density and quartiles by group", jitter = "Individual observations"),
        x = if (grouped) s$xlab else NULL, y = if (grouped) s$ylab else s$xlab)
    }
    p <- p + ggplot2::guides(fill = "none")
  } else {
    two <- !is.null(s$ty)
    keys <- if (two) c("x", "y") else "x"
    counts <- stats::aggregate(list(count = rep(1L, nrow(d))), d[keys], sum)
    if (two) caption <- paste(caption, "| Unobserved category combinations have zero count")
    if (two && type == "count_heatmap") {
      p <- ggplot2::ggplot(counts, ggplot2::aes(.data$x, .data$y, fill = .data$count)) + ggplot2::geom_tile(colour = "white") +
        ggplot2::scale_fill_gradient(low = "#edf6f6", high = "#187f88") + ggplot2::labs(title = "Category co-occurrence counts", x = s$xlab, y = s$ylab, fill = "Count")
    } else {
      if (type == "proportion") counts$count <- if (two) counts$count / stats::ave(counts$count, counts$x, FUN = sum) else counts$count / sum(counts$count)
      if (two) {
        p <- ggplot2::ggplot(counts, ggplot2::aes(.data$x, .data$count, fill = .data$y)) +
          ggplot2::geom_col(position = if (type == "proportion") "stack" else "dodge", width = .7) + ggplot2::labs(fill = s$ylab)
      } else p <- ggplot2::ggplot(counts, ggplot2::aes(.data$x, .data$count, fill = .data$x)) + ggplot2::geom_col(width = .7) + ggplot2::guides(fill = "none")
      p <- p + ggplot2::labs(title = if (type == "proportion") "Category proportions" else "Category counts", x = s$xlab,
        y = if (type == "proportion") if (two) "Proportion within X" else "Proportion" else "Count")
      if (type == "proportion") p <- p + ggplot2::scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"))
    }
  }
  if (horizontal) p <- p + ggplot2::coord_flip()
  rr_plot_style(p + ggplot2::labs(subtitle = caption))
}

#' List compatible plots for the selected data
#' @param x A vector, data frame, or supported report object.
#' @param axis_x,axis_y Data-frame column indices. Use zero for no Y variable.
#'   Defaults select known numeric/time columns without inferring categories.
#' @param limits Named finite provider limits. Data charts retain all rows and
#'   observed categories of the selected columns; preview limits do not sample
#'   observations or restrict which data-frame columns can be selected.
#' @param colour_by,facet_by Categorical column indices for colour groups and
#'   panels; NULL or zero disables the grouping.
#' @param x_as_category,y_as_category Logical; treat selected axes as categorical
#'   for this chart without modifying the source object.
#' @param assay,layer Optional expression source selectors for bioinformatics
#'   containers, as in \code{rresult_report}.
#' @param bio_coef Optional saved limma coefficient/contrast name or index.
#' @return A named character vector: display labels and plot type identifiers.
#' @examples
#' rresult_plot_choices(iris, axis_x = 5, axis_y = 1)
#' @export
rresult_plot_choices <- function(x, axis_x = NULL, axis_y = NULL, limits = list(), colour_by = NULL,
                                 facet_by = NULL, x_as_category = FALSE, y_as_category = FALSE,
                                 assay = NULL, layer = NULL, bio_coef = NULL) {
  l <- do.call(rr_limits, limits)
  kind <- rr_report_kind(x)
  generic <- rr_kind(x)
  if (generic == "matrix") return(if (typeof(x) %in% c("integer", "double")) c("Selected numeric window" = "heatmap") else character())
  if (generic == "sparse_matrix") {
    numeric <- "x" %in% methods::slotNames(x) && typeof(methods::slot(x, "x")) %in% c("integer", "double")
    return(if (numeric) c("Selected numeric window" = "heatmap") else character())
  }
  if (generic == "array") { x <- rr_plain(x); attributes(x) <- NULL }
  if (is.null(kind) || kind == "data_frame") return(rr_data_choices(x, l, axis_x, axis_y,
    colour_by, facet_by, x_as_category, y_as_category))
  if (kind %in% .rr_bio_containers) return(rr_bio_container_choices(rr_bio_source(x, l, assay, layer, lazy = TRUE)))
  if (kind %in% .rr_bio_families) return(rr_bio_result_choices(x, kind, l, bio_coef))
  if (kind %in% .rr_ordination_families) return(rr_ordination_choices(rr_ordination(x, kind)))
  rr_report(x, l, assay = assay, layer = layer, bio_coef = bio_coef)$plots
}


