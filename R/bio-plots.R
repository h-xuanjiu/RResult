# Charts use saved coordinates or explicitly bounded expression windows.
rr_bio_default_group <- function(source, key = NULL) {
  if (!is.null(key) && !key %in% c("", "0")) return(key)
  if (!is.null(source$identity)) return(source$identity_key)
  category <- names(source$meta_types)[source$meta_types == "category"]
  if (length(category)) return(category[1L])
  NULL
}

rr_bio_coordinates <- function(source, type, limits, reduction = NULL, axis_x = NULL, axis_y = NULL) {
  spatial <- type %in% c("spatial", "spatial_feature")
  reduction <- if (spatial) "Saved spatial coordinates" else rr_bio_reduction(source, reduction)
  m <- if (spatial) source$spatial else if (is.null(reduction)) NULL else source$reduced(reduction)
  if (is.null(m) || ncol(m) < 2L) stop("This object has no saved two-dimensional coordinates for this view.", call. = FALSE)
  axes <- c(rr_integer(if (is.null(axis_x)) 1 else as.numeric(axis_x), "X dimension", upper = ncol(m)),
    rr_integer(if (is.null(axis_y)) 2 else as.numeric(axis_y), "Y dimension", upper = ncol(m)))
  if (axes[1L] == axes[2L]) stop("Choose two different coordinate dimensions.", call. = FALSE)
  idx <- seq_len(source$n_cells)
  observations <- source$cells
  population <- source$n_cells
  if (source$kind == "Seurat" && type == "embedding") {
    # A saved embedding is independent of a selected split expression layer.
    source$coordinate_index(reduction, integer())
    idx <- seq_len(nrow(m))
    position <- idx
    observations <- rownames(m)
    population <- nrow(m)
  } else if (source$kind == "Seurat") {
    position <- if (is.function(source$coordinate_index)) source$coordinate_index(reduction, idx) else {
      if (is.null(rownames(m)) || anyDuplicated(rownames(m))) stop("Saved Seurat coordinates need unique cell names.", call. = FALSE)
      match(source$cells[idx], rownames(m))
    }
  } else {
    if (nrow(m) != source$n_cells) stop("Saved coordinate rows do not match observation count.", call. = FALSE)
    position <- idx
  }
  d <- data.frame(index = idx, observation = observations[idx], x = as.numeric(m[position, axes[1L]]), y = as.numeric(m[position, axes[2L]]))
  keep <- is.finite(d$x) & is.finite(d$y)
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) stop("No observations have finite saved coordinates.", call. = FALSE)
  labels <- colnames(m)
  if (is.null(labels)) labels <- paste("Dimension", seq_len(ncol(m)))
  list(data = d, name = reduction, labels = labels[axes], omitted = sum(!keep), population = population)
}

rr_bio_container_plot <- function(x, type, limits, assay = NULL, layer = NULL, reduction = NULL,
                                  feature = NULL, colour_by = NULL, shape_by = NULL, facet_by = NULL,
                                  axis_x = NULL, axis_y = NULL, source = NULL) {
  s <- if (is.null(source)) rr_bio_source(x, limits, assay, layer) else source
  if (!type %in% rr_bio_container_choices(s)) stop("This view is unavailable for the selected saved expression source.", call. = FALSE)
  if (type %in% c("embedding", "feature_map", "spatial", "spatial_feature")) {
    o <- rr_bio_coordinates(s, type, limits, reduction, axis_x, axis_y)
    d <- o$data
    observation_ids <- if (s$kind == "Seurat") d$observation else NULL
    if (type %in% c("feature_map", "spatial_feature")) {
      genes <- rr_bio_features(s, feature, limits)
      if (length(genes) != 1L) stop("Choose one feature for an expression map.", call. = FALSE)
      d$colour <- rr_bio_expression_values(s, genes, d$index, limits)
      label <- s$features[genes]
    } else {
      group <- rr_bio_group(s, d$index, colour_by, observation_ids = observation_ids)
      if (!is.null(group)) { d$colour <- group$value; label <- group$name } else label <- NULL
    }
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$y))
    if ("colour" %in% names(d)) p <- p + ggplot2::aes(colour = .data$colour)
    if (!is.null(shape_by) && !shape_by %in% c("", "0")) {
      shape <- rr_bio_group(s, d$index, shape_by, TRUE, observation_ids)
      if (nlevels(shape$value) > 26L) stop("Shape mapping supports at most 26 groups.", call. = FALSE)
      d$shape <- shape$value; p$data <- d
      p <- p + ggplot2::aes(shape = .data$shape) + ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 0:14, 19:25), name = shape$name)
    }
    p <- p + ggplot2::geom_point(size = 2, alpha = .7, na.rm = TRUE)
    if (!is.null(facet_by) && !facet_by %in% c("", "0")) {
      facet <- rr_bio_group(s, d$index, facet_by, TRUE, observation_ids)
      if (nlevels(facet$value) > 12L) stop("Faceting supports at most 12 groups.", call. = FALSE)
      d$facet <- facet$value; p$data <- d
      p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$facet))
    }
    return(rr_plot_style(p + ggplot2::coord_equal() + ggplot2::labs(title = o$name,
      subtitle = paste(nrow(d), "displayed observations of", o$population,
        if (type == "embedding") "in saved embedding |" else "in selected source |", o$omitted, "coordinates unavailable"),
      x = o$labels[1L], y = o$labels[2L], colour = label,
      caption = if (type %in% c("feature_map", "spatial_feature")) paste("Saved expression:", s$assay, "/", s$layer, "| Feature:", label) else "Saved coordinates; no dimensional reduction or spatial registration recomputed.")))
  }
  if (!is.null(facet_by) && !facet_by %in% c("", "0")) stop("Bioinformatics faceting is currently available for saved/spatial maps.", call. = FALSE)
  if (type == "metadata_distribution") {
    key <- colour_by
    if (is.null(key) || key %in% c("", "0")) key <- names(s$meta_types)[s$meta_types == "numeric"][1L]
    idx <- seq_len(s$n_cells)
    metric <- rr_bio_group(s, idx, key)
    if (is.null(metric) || metric$type != "numeric") stop("Choose a numeric metadata metric for this view.", call. = FALSE)
    d <- data.frame(index = idx, value = metric$value)
    return(rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(.data$value)) + ggplot2::geom_histogram(bins = 30, fill = "#187f88", colour = "white", na.rm = TRUE) +
      ggplot2::labs(title = metric$name, subtitle = paste(length(idx), "observations | all saved metadata values"), x = metric$name, y = "Observations")))
  }
  group_key <- rr_bio_default_group(s, colour_by)
  if (type == "composition") {
    idx <- seq_len(s$n_cells)
    group <- rr_bio_group(s, idx, group_key, TRUE)
    if (is.null(group)) stop("Choose a categorical metadata column.", call. = FALSE)
    counts <- sort(table(group$value), decreasing = TRUE)
    keep <- seq_along(counts)
    d <- data.frame(group = names(counts)[keep], count = as.numeric(counts[keep]))
    return(rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(stats::reorder(.data$group, .data$count), .data$count, fill = .data$group)) +
      ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(title = "Composition of all observations", x = group$name, y = "Observations", fill = NULL,
        subtitle = paste(length(idx), "observations | all", length(counts), "groups displayed"))))
  }
  genes <- rr_bio_features(s, feature, limits, default = if (type == "expression_heatmap") 20L else if (type == "expression_dot") 6L else 1L)
  if (!length(genes)) stop("This source has no features.", call. = FALSE)
  if (type == "expression_violin" && length(genes) != 1L) stop("Choose one feature for a distribution plot.", call. = FALSE)
  source <- paste(s$assay, "/", s$layer)
  if (type == "expression_heatmap") {
    cap <- min(limits$max_plot_points, limits$max_cells) %/% length(genes)
    cap <- min(cap, limits$max_cols)
    if (cap < 1L) stop("Expression selection exceeds matrix display limits.", call. = FALSE)
    idx <- rr_sample_indices(s$n_cells, cap)
    m <- rr_bio_slice(s, genes, idx, limits)
    subtitle <- paste(length(genes), "features |", length(idx), "sampled observations of", s$n_cells, "|", source)
    d <- data.frame(feature = factor(rep(seq_along(genes), length(idx)), levels = rev(seq_along(genes))), observation = rep(seq_along(idx), each = length(genes)), value = as.vector(m))
    return(rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(.data$observation, .data$feature, fill = .data$value)) + ggplot2::geom_tile() +
      ggplot2::scale_y_discrete(labels = stats::setNames(rr_text(s$features[genes], 55L), seq_along(genes))) +
      ggplot2::scale_x_continuous(breaks = seq_along(idx), labels = paste0(idx, ": ", rr_text(s$cells[idx], 25L))) +
      ggplot2::labs(title = "Saved expression window", subtitle = subtitle, x = "Observation index / ID", y = "Feature", fill = "Stored value") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))))
  }
  idx <- seq_len(s$n_cells)
  subtitle <- paste(length(genes), "features | all", length(idx), "observations |", source)
  group <- rr_bio_group(s, idx, group_key, TRUE)
  if (is.null(group)) group <- list(value = factor(rep("All observations", length(idx))), name = "Observations")
  if (nlevels(group$value) > limits$max_children) stop("Too many groups; choose a simpler categorical metadata column.", call. = FALSE)
  if (type == "expression_violin") {
    d <- data.frame(index = idx, group = group$value, value = rr_bio_expression_values(s, genes, idx, limits))
    d <- d[is.finite(d$value), , drop = FALSE]
    if (!nrow(d)) stop("No finite saved values for this feature.", call. = FALSE)
    p <- ggplot2::ggplot(d, ggplot2::aes(.data$group, .data$value, fill = .data$group))
    viable <- all(vapply(split(d$value, d$group, drop = TRUE), function(v) length(v) >= 3L && length(unique(v)) >= 2L, logical(1)))
    if (viable) p <- p + ggplot2::geom_violin(alpha = .45, trim = FALSE)
    return(rr_plot_style(p + ggplot2::geom_boxplot(width = .2, outlier.shape = NA, na.rm = TRUE) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = .12, seed = 1), alpha = .3, size = 1, na.rm = TRUE) +
      ggplot2::labs(title = s$features[genes], subtitle = subtitle, x = group$name, y = "Stored expression", fill = NULL)))
  }
  d <- rr_bio_expression_summary(s, genes, idx, group$value, limits)
  d$feature <- factor(d$feature, levels = rev(s$features[genes]))
  rr_plot_style(ggplot2::ggplot(d, ggplot2::aes(.data$group, .data$feature, colour = .data$mean, size = .data$above_zero)) +
    ggplot2::geom_point(na.rm = TRUE) + ggplot2::scale_size_continuous(range = c(1, 8), limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    ggplot2::guides(colour = ggplot2::guide_colourbar(title.position = "top", barwidth = grid::unit(36, "mm")),
      size = ggplot2::guide_legend(nrow = 2, byrow = TRUE, title.position = "top")) +
    ggplot2::labs(title = "Grouped saved expression", subtitle = subtitle, x = group$name, y = NULL,
      colour = "Mean stored value", size = "Above zero (%)", caption = "Means and percentages use all finite values in the selected source. Above-zero values in a centered/scaled layer are not a detection rate."))
}

rr_bio_fraction <- function(x) {
  vapply(as.character(x), function(value) {
    parts <- strsplit(value, "/", fixed = TRUE)[[1L]]
    num <- suppressWarnings(as.numeric(parts))
    if (length(num) == 1L && is.finite(num)) num else if (length(num) == 2L && all(is.finite(num)) && num[2L] > 0) num[1L] / num[2L] else NA_real_
  }, numeric(1))
}

rr_bio_enrichment_plot <- function(x, kind, type, limits, top_terms) {
  slot <- if (kind == "compareClusterResult") "compareClusterResult" else "result"
  raw <- methods::slot(x, slot)
  if (nrow(raw) > limits$max_scan_rows) stop("Term ranking exceeds max_scan_rows.", call. = FALSE)
  if (!nrow(raw)) stop("The stored enrichment table is empty.", call. = FALSE)
  n <- rr_integer(as.numeric(top_terms), "top_terms", 1, 100)
  pcol <- intersect(c("p.adjust", "qvalue", "pvalue"), names(raw))
  probability <- if (length(pcol)) as.numeric(raw[[pcol[1L]]]) else rep(NA_real_, nrow(raw))
  cluster <- if ("Cluster" %in% names(raw)) as.character(raw$Cluster) else rep("Saved terms", nrow(raw))
  groups <- utils::head(unique(cluster), limits$max_children)
  cap <- min(n, limits$max_plot_points %/% max(1L, length(groups)), limits$max_rows)
  idx <- unlist(lapply(groups, function(g) { rows <- which(cluster == g); utils::head(rows[order(probability[rows], na.last = TRUE)], cap) }), use.names = FALSE)
  d <- raw[idx, , drop = FALSE]
  if (!nrow(d)) stop("No saved terms fit the selected limits.", call. = FALSE)
  ids <- if ("ID" %in% names(d)) as.character(d$ID) else as.character(idx)
  descriptions <- if ("Description" %in% names(d)) rr_text(as.character(d$Description), 80L) else ids
  d$term <- factor(paste0(descriptions, " [", ids, "]"), levels = rev(unique(paste0(descriptions, " [", ids, "]"))))
  d$cluster <- cluster[idx]
  d$significance <- -log10(pmax(probability[idx], .Machine$double.xmin))
  d$significance[!is.finite(d$significance)] <- NA_real_
  d$size <- if ("Count" %in% names(d)) as.numeric(d$Count) else if ("setSize" %in% names(d)) as.numeric(d$setSize) else rep(1, nrow(d))
  d$value <- if ("NES" %in% names(d)) as.numeric(d$NES) else if ("GeneRatio" %in% names(d)) rr_bio_fraction(d$GeneRatio) else d$size
  p <- if (type == "enrichment_dot") {
    if (kind == "compareClusterResult") ggplot2::ggplot(d, ggplot2::aes(.data$cluster, .data$term, colour = .data$significance, size = .data$size)) else
      ggplot2::ggplot(d, ggplot2::aes(.data$value, .data$term, colour = .data$significance, size = .data$size))
  } else ggplot2::ggplot(d, ggplot2::aes(.data$value, .data$term, fill = .data$significance))
  if (type == "enrichment_dot") p <- p + ggplot2::geom_point(na.rm = TRUE) else p <- p + ggplot2::geom_col(na.rm = TRUE)
  if (type == "enrichment_bar" && kind == "compareClusterResult") p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$cluster), scales = "free_y")
  xlab <- if (type == "enrichment_dot" && kind == "compareClusterResult") "Saved cluster" else if ("NES" %in% names(d)) "Saved normalized enrichment score" else if ("GeneRatio" %in% names(d)) "Saved gene ratio" else "Saved term size"
  legend <- if (length(pcol)) paste0("-log10(", pcol[1L], ")") else "Unavailable P value"
  labels <- list(title = "Leading stored enrichment terms", subtitle = paste(nrow(d), "of", nrow(raw), "saved term rows | selection within", length(groups), "displayed groups"), x = xlab, y = NULL)
  if (type == "enrichment_dot") { labels$colour <- legend; labels$size <- "Saved term size" } else labels$fill <- legend
  rr_plot_style(p + do.call(ggplot2::labs, labels))
}

rr_bio_result_plot <- function(x, kind, type, limits, bio_coef = NULL, padj_cutoff = NULL, lfc_cutoff = 0, top_terms = 20L, result = NULL) {
  if (kind %in% c("enrichResult", "gseaResult", "compareClusterResult")) return(rr_bio_enrichment_plot(x, kind, type, limits, top_terms))
  r <- if (is.null(result)) rr_bio_result_data(x, kind, limits, bio_coef, complete = TRUE) else result
  d <- r$data
  cutoff <- if (is.null(padj_cutoff)) .05 else padj_cutoff
  if (!is.numeric(cutoff) || length(cutoff) != 1L || !is.finite(cutoff) || cutoff <= 0 || cutoff > 1 || !is.numeric(lfc_cutoff) || length(lfc_cutoff) != 1L || !is.finite(lfc_cutoff) || lfc_cutoff < 0) stop("Invalid display significance/effect cutoff.", call. = FALSE)
  positive <- if (kind == "MArrayLM") "Positive" else "Up"
  negative <- if (kind == "MArrayLM") "Negative" else "Down"
  d$direction <- factor(ifelse(!is.finite(d$probability), "Unavailable", ifelse(d$probability <= cutoff & abs(d$effect) >= lfc_cutoff,
    ifelse(d$effect > 0, positive, negative), "Other")), levels = c(negative, "Other", positive, "Unavailable"))
  subtitle <- paste(nrow(d), "gene rows of", r$total, "|", if (is.null(r$coefficient)) kind else r$coefficient$name)
  if (type == "probability") return(rr_plot_style(ggplot2::ggplot(d[is.finite(d$probability), , drop = FALSE], ggplot2::aes(.data$probability)) +
    ggplot2::geom_histogram(bins = 30, fill = "#187f88", colour = "white") + ggplot2::labs(title = r$probability_label, subtitle = subtitle, x = r$probability_label, y = "Displayed genes")))
  if (!r$one_effect) stop("This saved test is multi-parameter; use its probability distribution or result table.", call. = FALSE)
  d$significance <- -log10(pmax(d$probability, .Machine$double.xmin))
  if (type == "volcano") p <- ggplot2::ggplot(d, ggplot2::aes(.data$effect, .data$significance, colour = .data$direction)) +
    ggplot2::geom_point(alpha = .65, na.rm = TRUE) + ggplot2::geom_hline(yintercept = -log10(cutoff), linetype = 2, colour = "#9da9b6") +
    ggplot2::geom_vline(xintercept = c(-lfc_cutoff, lfc_cutoff), linetype = 2, colour = "#9da9b6") + ggplot2::labs(x = r$effect_label, y = paste0("-log10(", r$probability_label, ")")) else
      p <- ggplot2::ggplot(d, ggplot2::aes(.data$mean, .data$effect, colour = .data$direction)) + ggplot2::geom_point(alpha = .65, na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "#9da9b6") + ggplot2::labs(x = r$mean_label, y = r$effect_label)
  rr_plot_style(p + ggplot2::labs(title = if (type == "volcano") "Saved differential-expression effects" else "Saved mean-expression and effects", subtitle = subtitle, colour = NULL,
    caption = paste(r$probability_label, "cutoff:", cutoff, "| absolute effect cutoff:", lfc_cutoff)))
}





