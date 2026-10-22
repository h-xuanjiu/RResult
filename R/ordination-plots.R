# Maps and diagnostics from saved ordinations --------------------------------

rr_ordination_labels <- function(data, mapping, colour = NULL, inherit.aes = FALSE, show.legend = FALSE) {
  if (requireNamespace("ggrepel", quietly = TRUE)) return(ggrepel::geom_text_repel(data = data, mapping = mapping,
    inherit.aes = inherit.aes, show.legend = show.legend, colour = colour, size = 3,
    seed = 1L, max.overlaps = Inf, max.time = .2, max.iter = 1000L, box.padding = .4,
    min.segment.length = 0, segment.color = "#9da9b6"))
  ggplot2::geom_text(data = data, mapping = mapping, inherit.aes = inherit.aes, show.legend = show.legend,
    colour = colour, size = 3, vjust = -.7, check_overlap = TRUE)
}

rr_ordination_column <- function(data, key, role, categorical = FALSE) {
  if (is.null(key) || identical(key, "") || identical(key, 0) || identical(key, "0")) return(NULL)
  if (is.null(data)) stop(role, " requires sample metadata.", call. = FALSE)
  if (is.character(key) && length(key) == 1L && sum(names(data) == key) == 1L) j <- match(key, names(data)) else
    j <- rr_integer(suppressWarnings(as.numeric(key)), paste(role, "column"), upper = ncol(data))
  v <- data[[j]]
  type <- rr_variable_type(v)
  if (!type %in% if (categorical) "category" else c("category", "numeric"))
    stop(role, if (categorical) " requires a categorical metadata column." else " requires a numeric or categorical metadata column.", call. = FALSE)
  if (type == "category") {
    v <- as.character(v)
    # A real '(missing)' level remains distinct from absent metadata.
    missing_label <- "(missing)"
    while (missing_label %in% v[!is.na(v)]) missing_label <- paste0(missing_label, "*")
    v[is.na(v)] <- missing_label
    v <- factor(v, levels = unique(v))
  } else v[!is.finite(v)] <- NA_real_
  list(value = v, name = names(data)[j], type = type)
}

rr_ordination_axes <- function(o, axis_x, axis_y, m) {
  if (is.null(m)) stop("Scores were not saved; this coordinate view is unavailable.", call. = FALSE)
  ax <- rr_integer(if (is.null(axis_x)) 1L else as.numeric(axis_x), "X component / axis", upper = ncol(m))
  ay <- rr_integer(if (is.null(axis_y)) 2L else as.numeric(axis_y), "Y component / axis", upper = ncol(m))
  if (ax == ay) stop("Choose two different components / axes.", call. = FALSE)
  c(ax, ay)
}

rr_ordination_axis_label <- function(o, axis) {
  if (o$kind %in% .rr_constrained_families) return(paste0(o$axis_names[axis], " (", round(o$percent[axis], 1L), "% total inertia)"))
  name <- paste0(if (o$method == "PCA") "PC" else if (o$method == "PCoA") "PCoA" else if (o$method == "NMDS") "NMDS" else "MDS", axis)
  if (o$method %in% c("PCA", "PCoA") && length(o$percent) >= axis && is.finite(o$percent[axis]))
    name <- paste0(name, " (", format(round(o$percent[axis], 1L), trim = TRUE), "%", if (o$method == "PCoA") " positive inertia" else "", ")")
  name
}

rr_ordination_plot <- function(x, type, limits, axis_x = NULL, axis_y = NULL,
                               metadata = NULL, sample_id = NULL, colour_by = NULL, shape_by = NULL,
                               facet_by = NULL, sample_labels = FALSE, max_labels = 20L,
                               top_variables = 12L, group_hulls = FALSE) {
  o <- rr_ordination(x)
  for (flag in list(sample_labels, group_hulls)) if (!is.logical(flag) || length(flag) != 1L || is.na(flag))
    stop("Ordination display switches must be TRUE or FALSE.", call. = FALSE)
  max_labels <- rr_integer(as.numeric(max_labels), "max_labels", 0, 200L)
  top_variables <- rr_integer(as.numeric(top_variables), "top_variables", 1, 100L)
  if (type %in% c("scores", "biplot", "environment")) axes <- rr_ordination_axes(o, axis_x, axis_y, o$scores)
  if (type %in% c("loadings", "species")) axes <- rr_ordination_axes(o, axis_x, axis_y, o$variables)
  if (!type %in% rr_ordination_choices(o)) stop("This chart is unavailable for the saved ordination.", call. = FALSE)
  if (type == "scree") {
    idx <- seq_along(o$eigen)
    d <- data.frame(component = idx, variance = o$percent[idx], cumulative = cumsum(o$percent)[idx])
    return(rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(.data$component, .data$variance)) +
      ggplot2::geom_col(fill = "#187f88", width = .65, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = .data$cumulative), colour = "#cc7852", na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(y = .data$cumulative), colour = "#cc7852", na.rm = TRUE) +
      ggplot2::labs(title = "How much fitted variation do the components retain?", subtitle = "Bars: individual variance | Line: cumulative variance",
        x = "Principal component", y = "Variance (%)")))
  }
  if (type == "eigenvalues") {
    idx <- seq_along(o$eigen)
    d <- data.frame(axis = idx, eigenvalue = o$eigen[idx], sign = ifelse(o$eigen[idx] >= 0, "Positive", "Negative"))
    return(rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(.data$axis, .data$eigenvalue, fill = .data$sign)) +
      ggplot2::geom_col(width = .7) + ggplot2::geom_hline(yintercept = 0, colour = "#9da9b6") +
      ggplot2::labs(title = "Positive and negative eigenvalues", subtitle = paste("Saved correction:", o$correction),
        x = "Eigenvalue position", y = "Eigenvalue", fill = NULL)))
  }
  if (type == "shepard") {
    idx <- seq_along(x$diss)
    d <- data.frame(dissimilarity = x$diss[idx], distance = x$dist[idx], disparity = x$dhat[idx])
    d <- d[stats::complete.cases(d) & is.finite(d$dissimilarity) & is.finite(d$distance) & is.finite(d$disparity), , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$dissimilarity, .data$distance)) +
      ggplot2::geom_point(colour = "#187f88", alpha = .5, size = 1.5)
    if (is.null(x$model) || identical(x$model, "global")) p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$disparity), colour = "#cc7852", linewidth = .8) else
      p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$disparity), colour = "#cc7852", shape = 3)
    return(rr_plot_style(p + ggplot2::labs(title = "How closely does the saved configuration fit?",
      subtitle = paste("Stress:", rr_fmt(o$stress), "|", nrow(d), "finite saved pairs of", length(x$diss)),
      x = "Original dissimilarity", y = "Saved engine distance", caption = "Teal: engine distances. Orange: fitted disparities. Both retain the original engine scale.")))
  }
  if (type == "loadings_heatmap") {
    m <- o$variables
    nc <- min(ncol(m), limits$max_cols, limits$max_cells, limits$max_plot_points)
    nr <- min(nrow(m), top_variables, limits$max_children, floor(min(limits$max_cells, limits$max_plot_points) / nc))
    rows <- utils::head(order(rowSums(m[, seq_len(nc), drop = FALSE]^2), decreasing = TRUE), nr)
    labels <- rownames(m); if (is.null(labels)) labels <- as.character(seq_len(nrow(m)))
    d <- data.frame(variable = factor(rep(paste0(rows, ": ", rr_text(labels[rows], 55L)), nc),
      levels = rev(paste0(rows, ": ", rr_text(labels[rows], 55L)))), component = rep(paste0("PC", seq_len(nc)), each = nr),
      value = as.vector(m[rows, seq_len(nc), drop = FALSE]))
    d$component <- factor(d$component, levels = paste0("PC", seq_len(nc)))
    return(rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(.data$component, .data$variable, fill = .data$value)) +
      ggplot2::geom_tile(colour = "white") + ggplot2::scale_fill_gradient2(low = "#cc7852", mid = "white", high = "#187f88") +
      ggplot2::labs(title = o$variable_label, subtitle = "Variables ranked by squared coordinates across the displayed components",
        x = "Component", y = NULL, fill = "Coordinate")))
  }
  ax <- axes[1L]; ay <- axes[2L]
  if (type %in% c("loadings", "species")) {
    m <- o$variables
    idx <- utils::head(order(rowSums(m[, axes, drop = FALSE]^2), decreasing = TRUE), min(top_variables, limits$max_children, limits$max_plot_points))
    labels <- rownames(m); if (is.null(labels)) labels <- as.character(seq_len(nrow(m)))
    d <- data.frame(x = m[idx, ax], y = m[idx, ay], variable = rr_text(labels[idx], 65L))
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y))
    if (type == "loadings") p <- p + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
      arrow = grid::arrow(length = grid::unit(.16, "cm")), colour = "#187f88", linewidth = .7) else
      p <- p + ggplot2::geom_point(colour = "#187f88", size = 2)
    p <- p + rr_ordination_labels(d, ggplot2::aes(.data$x, .data$y, label = .data$variable), colour = "#187f88") +
      ggplot2::labs(title = o$variable_label, subtitle = paste(length(idx), "leading variables by squared coordinates in these axes"))
  } else {
    m <- o$scores
    idx <- seq_len(nrow(m))
    matched <- rr_ordination_metadata(metadata, sample_id, m, idx, limits, preview = FALSE)
    ids <- rownames(m); if (is.null(ids)) ids <- as.character(seq_len(nrow(m)))
    d <- data.frame(x = m[idx, ax], y = m[idx, ay], sample = rr_text(ids[idx], 80L), index = idx)
    colour <- rr_ordination_column(matched$data, colour_by, "Colour")
    shape <- rr_ordination_column(matched$data, shape_by, "Shape", TRUE)
    facet <- rr_ordination_column(matched$data, facet_by, "Faceting", TRUE)
    if (!is.null(colour) && colour$type == "category" && nlevels(colour$value) > limits$max_children)
      stop("Too many displayed colour categories; increase max_children or use a simpler grouping.", call. = FALSE)
    if (!is.null(colour)) d$group <- colour$value
    if (!is.null(shape)) {
      if (nlevels(shape$value) > 26L) stop("Shape grouping supports at most 26 displayed categories; use colour or panels.", call. = FALSE)
      d$shape <- shape$value
    }
    if (!is.null(facet)) {
      if (nlevels(facet$value) > 12L) stop("Ordination faceting supports at most 12 displayed categories.", call. = FALSE)
      d$panel <- facet$value
    }
    finite <- is.finite(d$x) & is.finite(d$y)
    omitted <- sum(!finite); d <- d[finite, , drop = FALSE]
    if (!nrow(d)) stop("No finite sample coordinates in the selected axes.", call. = FALSE)
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y))
    if (!is.null(colour)) p <- p + ggplot2::aes(colour = .data$group)
    if (!is.null(shape)) p <- p + ggplot2::aes(shape = .data$shape) + ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 0:14, 19:25))
    if (group_hulls) {
      if (is.null(colour) || colour$type != "category") stop("Group hulls require categorical colour grouping.", call. = FALSE)
      keys <- if (!is.null(facet)) interaction(d$group, d$panel, drop = TRUE) else d$group
      hulls <- lapply(split(d, keys, drop = TRUE), function(g) {
        if (nrow(unique(g[c("x", "y")])) < 3L) return(NULL)
        hull <- grDevices::chull(g$x, g$y)
        g[c(hull, hull[1L]), , drop = FALSE]
      })
      hull <- do.call(rbind, hulls)
      if (!is.null(hull)) p <- p + ggplot2::geom_path(data = hull, ggplot2::aes(group = .data$group), linewidth = .7, show.legend = FALSE)
    }
    p <- p + if (is.null(colour)) ggplot2::geom_point(colour = "#187f88", size = 2.5, alpha = .8) else ggplot2::geom_point(size = 2.5, alpha = .8)
    if (sample_labels && max_labels > 0L) p <- p + rr_ordination_labels(utils::head(d, min(max_labels, limits$max_children)),
      ggplot2::aes(.data$x, .data$y, label = .data$sample), colour = "#526377")
    caption <- if (group_hulls) "Outlines show displayed samples, not confidence regions." else NULL
    if (type %in% c("biplot", "environment")) {
      v <- if (type == "environment") o$environment else o$variables
      if (max(axes) > ncol(v)) stop("Variable coordinates were not saved for both selected axes.", call. = FALSE)
      vi <- utils::head(order(rowSums(v[, axes, drop = FALSE]^2), decreasing = TRUE), min(top_variables, limits$max_children, limits$max_plot_points))
      names <- rownames(v); if (is.null(names)) names <- as.character(seq_len(nrow(v)))
      arrows <- data.frame(x = v[vi, ax], y = v[vi, ay], variable = rr_text(names[vi], 60L))
      arrows <- arrows[is.finite(arrows$x) & is.finite(arrows$y), , drop = FALSE]
      extent <- max(abs(c(arrows$x, arrows$y)))
      multiplier <- if (is.finite(extent) && extent > 0) .7 * max(abs(c(d$x, d$y))) / extent else 1
      arrows$x <- arrows$x * multiplier; arrows$y <- arrows$y * multiplier
      p <- p + ggplot2::geom_segment(data = arrows, ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
        inherit.aes = FALSE, colour = "#cc7852", linewidth = .7, arrow = grid::arrow(length = grid::unit(.18, "cm"))) +
        rr_ordination_labels(arrows, ggplot2::aes(.data$x, .data$y, label = .data$variable), colour = "#9d542e")
      caption <- trimws(paste(caption, if (type == "environment") "Explanatory-variable" else "Variable", "arrows multiplied by", rr_fmt(multiplier), "for display; separate coordinate scale."))
    }
    if (!is.null(facet)) p <- p + ggplot2::facet_wrap(~panel)
    p <- p + ggplot2::labs(title = paste(o$method, if (type == "biplot") "samples and variable arrows" else if (type == "environment") "samples and explanatory-variable arrows" else "sample map"),
      subtitle = paste(nrow(d), "displayed samples of", nrow(m), if (omitted) paste("|", omitted, "non-finite coordinate rows omitted"),
        if (!is.null(o$stress)) paste("| Stress:", rr_fmt(o$stress))),
      caption = caption, colour = if (!is.null(colour)) colour$name else NULL, shape = if (!is.null(shape)) shape$name else NULL)
  }
  rr_plot_style(p + ggplot2::geom_hline(yintercept = 0, colour = "#d7dfe6", linewidth = .3) +
    ggplot2::geom_vline(xintercept = 0, colour = "#d7dfe6", linewidth = .3) + ggplot2::coord_equal() +
    ggplot2::labs(x = rr_ordination_axis_label(o, ax), y = rr_ordination_axis_label(o, ay)))
}

