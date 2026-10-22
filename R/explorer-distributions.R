# Full-column distributions for the live data-frame explorer -----------------

rr_explorer_columns <- function(x, cols, limits) {
  p <- rr_plain(x)
  cols <- unique(rr_indices(cols, length(p), limits$max_cols, "Column"))
  types <- vapply(p[cols], rr_variable_type, character(1))
  supported <- types != "unsupported"
  labels <- names(p)[cols]
  labels[is.na(labels) | !nzchar(labels)] <- "(unnamed)"
  list(columns = cols[supported], types = types[supported],
    labels = if (any(supported)) paste0(cols[supported], ": ", rr_text(labels[supported], 120L)) else character(),
    omitted = if (any(!supported)) paste0(cols[!supported], ": ", rr_text(labels[!supported], 120L)) else character())
}

rr_explorer_distribution_choices <- function(selection) {
  types <- selection$types
  if (!length(types)) return(character())
  choices <- c("Automatic" = "auto")
  if (all(types == "numeric")) return(c(choices, "Box plots" = "boxplot", "Violin + box plots" = "violin",
    "Histograms" = "histogram", "Density curves" = "density", "Frequency polygons" = "freqpoly", "Cumulative distributions" = "ecdf"))
  if (all(types %in% c("numeric", "time"))) return(c(choices, "Histograms" = "histogram", "Cumulative distributions" = "ecdf"))
  if (all(types == "category")) return(c(choices, "Category counts" = "bar", "Category proportions" = "proportion"))
  choices
}

rr_explorer_distributions <- function(x, cols, limits, type = "auto", layout = "compare",
                                      page = 1L, show_points = TRUE) {
  selection <- rr_explorer_columns(x, cols, limits)
  choices <- rr_explorer_distribution_choices(selection)
  if (!length(choices)) stop("Select at least one numeric, categorical or time column to draw.", call. = FALSE)
  if (!type %in% choices) stop("This chart does not support the selected column types; choose Automatic or a compatible chart.", call. = FALSE)
  layout <- match.arg(layout, c("compare", "separate"))
  pages <- max(1L, ceiling(length(selection$columns) / 6L))
  page <- rr_integer(as.numeric(page), "Chart page", upper = pages)
  offset <- (page - 1L) * 6L
  positions <- seq.int(offset + 1L, min(length(selection$columns), offset + 6L))
  cols <- selection$columns[positions]
  labels <- selection$labels[positions]
  actual <- if (type == "auto" && length(selection$columns) > 1L && all(selection$types == "numeric")) "boxplot" else type
  compare <- layout == "compare" && length(cols) > 1L && actual %in% c("boxplot", "violin")
  p <- rr_plain(x)
  total <- rr_df_nrow(x)
  if (compare) {
    values <- lapply(cols, function(j) p[[j]])
    data <- data.frame(column = factor(rep(labels, lengths(values)), levels = labels),
      value = unlist(values, use.names = FALSE))
    data <- data[is.finite(data$value), , drop = FALSE]
    if (!nrow(data)) stop("The selected columns have no finite values to plot.", call. = FALSE)
    if (actual == "violin" && !rr_violin_ready(data$value, data$column))
      stop("Violin plots need at least two distinct finite values in every column; choose box plots instead.", call. = FALSE)
    plot <- ggplot2::ggplot(data, ggplot2::aes(.data$column, .data$value, fill = .data$column))
    if (actual == "violin") plot <- plot + ggplot2::geom_violin(trim = TRUE, alpha = .45)
    plot <- plot + ggplot2::geom_boxplot(width = if (actual == "violin") .15 else .55,
      outlier.shape = if (isTRUE(show_points)) NA else 16, alpha = .7)
    if (isTRUE(show_points)) plot <- plot + ggplot2::geom_point(
      position = ggplot2::position_jitter(width = .12, height = 0, seed = 1), alpha = .45, size = 1.5)
    plot <- rr_plot_style(plot + ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_fill_manual(values = rr_style_values(NULL, labels, rr_palette("teal")), drop = FALSE) +
      ggplot2::guides(fill = "none") + ggplot2::labs(
      title = "Distributions of selected columns", x = "Column", y = "Value",
      subtitle = paste(total, "source rows per column |", nrow(data), "finite values shown"),
      caption = "All source rows used; missing and non-finite values excluded per column. Table search and paging do not filter this chart."))
    plots <- list(plot)
  } else {
    plots <- lapply(seq_along(cols), function(i) tryCatch({
      value <- p[[cols[i]]]
      plot <- rr_plot(value, actual, limits, show_points = show_points)
      plot + ggplot2::labs(title = labels[i], subtitle = paste(total, "source rows | all rows used"),
        x = if (rr_variable_type(value) == "category") NULL else rr_text(names(p)[cols[i]], 120L))
    }, error = function(e) structure(list(label = labels[i], message = conditionMessage(e)), class = "rr_distribution_error")))
  }
  structure(list(plots = plots, columns = cols, selection = selection$columns, labels = labels,
    omitted = selection$omitted, total_rows = total, page = page, pages = pages,
    total_columns = length(selection$columns), height = if (compare || length(plots) == 1L) 430L else 320L * ceiling(length(plots) / 2L)),
    class = "rr_explorer_distributions")
}

rr_draw_distributions <- function(result, style) {
  n <- length(result$plots)
  columns <- min(2L, n)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(ceiling(n / columns), columns)))
  on.exit(grid::popViewport(), add = TRUE)
  for (i in seq_len(n)) {
    viewport <- grid::viewport(layout.pos.row = ceiling(i / columns), layout.pos.col = (i - 1L) %% columns + 1L)
    plot <- result$plots[[i]]
    if (inherits(plot, "rr_distribution_error")) {
      grid::pushViewport(viewport)
      grid::grid.text(paste(plot$label, paste(strwrap(plot$message, 55L), collapse = "\n"), sep = "\n\n"),
        gp = grid::gpar(col = "#7a4040", fontsize = 11))
      grid::popViewport()
    } else print(rr_apply_style(plot, style), newpage = FALSE, vp = viewport)
  }
  invisible(NULL)
}

rr_explorer_statistics <- function(x, cols) {
  p <- rr_plain(x)
  rows <- lapply(cols, function(j) {
    v <- p[[j]]
    type <- rr_variable_type(v)
    is_numeric <- type == "numeric"
    finite <- if (is_numeric) v[is.finite(v)] else numeric()
    q <- if (length(finite)) as.numeric(stats::quantile(finite, c(0, .25, .5, .75, 1), names = FALSE)) else rep(NA_real_, 5L)
    data.frame(Column = paste0(j, ": ", names(p)[j]), Type = type, Rows = length(v), Missing = sum(is.na(v)),
      Nonfinite = if (is_numeric) sum(!is.finite(v) & !is.na(v)) else 0L,
      Minimum = q[1L], Q1 = q[2L], Median = q[3L], Mean = if (length(finite)) mean(finite) else NA_real_,
      Q3 = q[4L], Maximum = q[5L], SD = if (length(finite) > 1L) stats::sd(finite) else NA_real_,
      Categories = if (type == "category") length(unique(v[!is.na(v)])) else NA_integer_, check.names = FALSE)
  })
  do.call(rbind, rows)
}
