# Each clone has a stable parent binding, including across layer loops.
rr_clone_ggproto <- function(x, ...) {
  force(x)
  ggplot2::ggproto(NULL, x, ...)
}

# Shared, copy-on-edit ggplot styling. Mapped aesthetics retain their meaning.
rr_style_layers <- function(plot, s, active, generated = FALSE) {
  for (i in seq_along(plot$layers)) {
    original <- plot$layers[[i]]
    layer <- rr_clone_ggproto(original)
    mapping <- names(layer$mapping)
    if (isTRUE(layer$inherit.aes)) mapping <- union(mapping, names(plot$mapping))
    params <- layer$aes_params
    is_geom <- function(classes) any(vapply(classes, function(g) inherits(layer$geom, g), logical(1)))
    set <- function(key, aesthetic) {
      if (key %in% active && !is.null(s[[key]]) && !aesthetic %in% mapping) params[[aesthetic]] <<- s[[key]]
    }
    if (is_geom("GeomPoint")) {
      set("point_size", "size"); set("alpha", "alpha"); set("point_shape", "shape"); set("stroke", "stroke")
    }
    if (is_geom(c("GeomLine", "GeomPath", "GeomSegment", "GeomBoxplot", "GeomViolin", "GeomSmooth", "GeomErrorbar", "GeomRect", "GeomBar"))) {
      if (!"size" %in% mapping) set("line_width", "linewidth")
      set("line_type", "linetype")
    }
    if (is_geom(c("GeomRibbon", "GeomSmooth"))) set("ribbon_alpha", "alpha")
    if (is_geom(c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel"))) {
      set("label_size", "size"); set("font_family", "family"); set("font_face", "fontface"); set("text_colour", "colour")
    }
    if (generated) for (aes in c("colour", "fill")) {
      value <- params[[aes]]
      if (is.character(value) && length(value) == 1L && value %in% c("#187f88", "#1a7e8c", "#477984", "#b2d8db", "#cc7852")) params[[aes]] <- rr_palette(s$palette)[if (value == "#cc7852") 2L else 1L]
    }
    if (!is_geom(c("GeomVline", "GeomHline", "GeomAbline"))) set("colour", "colour")
    set("fill", "fill")
    layer$aes_params <- params
    plot$layers[[i]] <- layer
  }
  plot
}

rr_style_values <- function(custom, groups, fallback) {
  n <- length(groups)
  values <- if (length(fallback) >= n) fallback[seq_len(n)] else grDevices::colorRampPalette(fallback)(n)
  names(values) <- groups
  if (is.null(custom)) return(values)
  if (!is.null(names(custom))) {
    unknown <- setdiff(names(custom), groups)
    if (length(unknown)) stop("Unknown colour/shape group: ", paste(unknown, collapse = ", "), call. = FALSE)
    values[names(custom)] <- custom
  } else {
    if (length(custom) < n) stop("Supply at least ", n, " colours/shapes, or use named group values.", call. = FALSE)
    values[] <- custom[seq_len(n)]
  }
  values
}

rr_style_gradient <- function(colours) {
  if (length(colours) == 1L) colours <- c("#f5f6f8", colours)
  ramp <- grDevices::colorRamp(unname(colours), alpha = TRUE)
  function(x) {
    ok <- is.finite(x)
    out <- rep(NA_character_, length(x))
    rgba <- ramp(pmax(0, pmin(1, x[ok])))
    if (any(ok)) out[ok] <- grDevices::rgb(rgba[, 1L], rgba[, 2L], rgba[, 3L], rgba[, 4L], maxColorValue = 255)
    out
  }
}

rr_style_scales <- function(plot, s, active) {
  selected <- character()
  if (any(c("palette", "colours") %in% active)) selected <- c(selected, "colour")
  if (any(c("palette", "fills") %in% active)) selected <- c(selected, "fill")
  if ("shapes" %in% active) selected <- c(selected, "shape")
  if ("legend_rows" %in% active) selected <- union(selected, c("colour", "fill", "shape"))
  if (!length(selected)) return(plot)
  mapped <- unique(c(names(plot$mapping), unlist(lapply(plot$layers, function(l) names(l$mapping)))))
  selected <- intersect(selected, mapped)
  if (!length(selected)) return(plot)
  pal <- rr_palette(s$palette)
  if (s$theme == "dark") {
    rgb <- .65 * grDevices::col2rgb(pal) + .35 * 255
    pal <- grDevices::rgb(rgb[1L, ], rgb[2L, ], rgb[3L, ], maxColorValue = 255)
  }
  sources <- lapply(selected, function(aes) plot$scales$get_scales(aes))
  # An existing discrete scale already defines its limits and group order.
  # Its palette receives the trained group count when the plot is drawn, so
  # changing the default palette need not execute layer statistics beforehand.
  if (!any(c("shapes", "legend_rows") %in% active) &&
      is.null(s$colours) && is.null(s$fills) &&
      all(vapply(sources, function(scale) inherits(scale, "ScaleDiscrete"), logical(1)))) {
    plot$scales <- plot$scales$clone()
    for (i in seq_along(selected)) {
      aes <- selected[[i]]
      scale <- sources[[i]]$clone()
      if ("palette" %in% active) scale$palette <- local({
        colours <- pal
        function(n) unname(rr_style_values(NULL, seq_len(n), colours))
      })
      scale$palette.cache <- NULL
      scale$n.breaks.cache <- NULL
      plot$scales$scales <- Filter(function(v) !aes %in% v$aesthetics, plot$scales$scales)
      plot <- plot + scale
    }
    return(plot)
  }
  # Train only when a scale is explicitly styled; retain its transformation,
  # limits, breaks, labels, midpoint and guide instead of replacing its meaning.
  if ("shapes" %in% active && "shape" %in% mapped && is.null(plot$scales$get_scales("shape"))) plot <- plot + ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 0:14, 19:25))
  built <- ggplot2::ggplot_build(plot)
  plot$scales <- plot$scales$clone()
  for (aes in selected) {
    source <- built$plot$scales$get_scales(aes)
    if (is.null(source)) next
    scale <- source$clone()
    custom <- s[[switch(aes, colour = "colours", fill = "fills", shape = "shapes")]]
    if (inherits(scale, "ScaleDiscrete")) {
      groups <- source$get_limits()
      groups <- as.character(groups[!is.na(groups)])
      if (!length(groups)) next
      if (aes == "shape") {
        if (!is.null(custom) && length(groups) > 26L) stop("Shape mapping supports at most 26 groups.", call. = FALSE)
        fallback <- c(16, 17, 15, 18, 0:14, 19:25)
      } else fallback <- pal
      values <- if (!is.null(custom) || (aes != "shape" && "palette" %in% active)) rr_style_values(custom, groups, fallback) else NULL
      if (!is.null(custom) || (aes != "shape" && "palette" %in% active)) scale$palette <- local({ chosen <- unname(values); function(n) chosen[seq_len(n)] })
      scale$palette.cache <- NULL
      scale$n.breaks.cache <- NULL
      if ("legend_rows" %in% active && !is.null(s$legend_rows)) scale$guide <- ggplot2::guide_legend(nrow = s$legend_rows, byrow = TRUE)
    } else if (aes != "shape" && inherits(scale, "ScaleContinuous") && !inherits(scale, "ScaleBinned")) {
      if (is.null(custom) && !"palette" %in% active) next
      if (!is.null(names(custom))) stop("Continuous colour scales use unnamed colour values.", call. = FALSE)
      limits <- source$get_limits()
      diverging <- aes == "fill" && length(limits) == 2L && all(is.finite(limits)) && limits[1L] < 0 && limits[2L] > 0
      colors <- if (!is.null(custom)) custom else if (diverging && !s$palette %in% c("viridis", "sunset", "inferno", "blue_red", "grey")) c(pal[2L], "#f5f6f8", pal[1L]) else if (s$palette %in% c("viridis", "sunset", "inferno", "blue_red", "grey")) pal else c("#eaf1f6", pal[1L])
      scale$palette <- rr_style_gradient(colors)
    } else next
    plot$scales$scales <- Filter(function(v) !aes %in% v$aesthetics, plot$scales$scales)
    plot <- plot + scale
  }
  plot
}

rr_style_details <- function(plot, s, active) {
  has <- function(key) key %in% active && !is.null(s[[key]])
  theme <- list()
  text <- list()
  for (pair in list(c("base_size", "size"), c("font_family", "family"), c("font_face", "face"), c("text_colour", "colour")))
    if (has(pair[1L])) text[[pair[2L]]] <- s[[pair[1L]]]
  if (length(text)) theme$text <- do.call(ggplot2::element_text, text)
  if (has("text_colour")) for (target in c("axis.text", "axis.title", "legend.text", "legend.title", "strip.text", "plot.title", "plot.subtitle", "plot.caption")) theme[[target]] <- ggplot2::element_text(colour = s$text_colour)
  targets <- c(title_size = "plot.title", subtitle_size = "plot.subtitle", caption_size = "plot.caption", axis_title_size = "axis.title", axis_text_size = "axis.text", legend_title_size = "legend.title", legend_text_size = "legend.text", strip_text_size = "strip.text")
  for (key in names(targets)) if (has(key)) theme[[targets[[key]]]] <- ggplot2::element_text(size = s[[key]], colour = s$text_colour)
  if (has("title_hjust")) theme$plot.title <- ggplot2::element_text(size = s$title_size, hjust = s$title_hjust, colour = s$text_colour)
  if (has("legend")) theme$legend.position <- s$legend
  for (key in c("x", "y")) {
    angle <- paste0(key, "_angle")
    if (has(angle)) theme[[paste0("axis.text.", key)]] <- ggplot2::element_text(angle = s[[angle]], hjust = if (s[[angle]] == 0) .5 else 1)
    if (has(paste0("show_", key, "_text")) && !s[[paste0("show_", key, "_text")]]) theme[[paste0("axis.text.", key)]] <- ggplot2::element_blank()
  }
  for (kind in c("lines", "ticks")) {
    key <- paste0("axis_", kind)
    target <- if (kind == "lines") "axis.line" else "axis.ticks"
    width <- if (kind == "lines") s$axis_line_width else s$tick_width
    touched <- has(key) && s[[key]] != "theme"
    if (touched) {
      theme[[target]] <- ggplot2::element_blank()
      for (axis in c("x", "y")) theme[[paste0(target, ".", axis)]] <- if (s[[key]] %in% c("both", axis)) ggplot2::element_line(colour = s$axis_colour, linewidth = width) else ggplot2::element_blank()
    } else if (has("axis_colour") || has(if (kind == "lines") "axis_line_width" else "tick_width")) {
      theme[[target]] <- ggplot2::element_line(colour = s$axis_colour, linewidth = width)
    }
  }
  if (has("tick_length")) theme$axis.ticks.length <- grid::unit(s$tick_length, "mm")
  if (has("grid")) {
    if (s$grid == "none") {
      theme$panel.grid <- theme$panel.grid.major <- theme$panel.grid.minor <- ggplot2::element_blank()
    } else if (s$grid != "theme") {
      theme$panel.grid.major <- ggplot2::element_line(colour = s$grid_colour, linewidth = s$grid_width, linetype = s$grid_type)
      theme$panel.grid.minor <- if (s$grid == "all") ggplot2::element_line(colour = s$grid_colour, linewidth = if (is.null(s$grid_width)) .2 else s$grid_width / 2, linetype = s$grid_type) else ggplot2::element_blank()
    }
  }
  if ((!has("grid") || s$grid == "theme") && any(c("grid_colour", "grid_width", "grid_type") %in% active)) theme$panel.grid <- ggplot2::element_line(colour = s$grid_colour, linewidth = s$grid_width, linetype = s$grid_type)
  if (has("panel_border") && s$panel_border == "hide") theme$panel.border <- ggplot2::element_blank()
  else if ((has("panel_border") && s$panel_border == "show") || has("border_colour") || has("border_width")) theme$panel.border <- ggplot2::element_rect(fill = NA, colour = if (is.null(s$border_colour)) "#54616e" else s$border_colour, linewidth = s$border_width)
  for (pair in list(c("panel_fill", "panel.background"), c("plot_fill", "plot.background"), c("legend_fill", "legend.background"), c("strip_fill", "strip.background")))
    if (has(pair[1L])) theme[[pair[2L]]] <- ggplot2::element_rect(fill = s[[pair[1L]]], colour = NA)
  if (has("panel_spacing")) theme$panel.spacing <- grid::unit(s$panel_spacing, "mm")
  if (has("legend_key_size")) theme$legend.key.size <- grid::unit(s$legend_key_size, "mm")
  if (has("plot_margin")) theme$plot.margin <- do.call(ggplot2::margin, as.list(rep(s$plot_margin, length.out = 4L)))
  if (length(theme)) plot <- plot + do.call(ggplot2::theme, theme)
  labels <- list(title = s$title, subtitle = s$subtitle, caption = s$caption, x = s$x_label, y = s$y_label)
  labels <- labels[!vapply(labels, is.null, logical(1))]
  for (key in c("title", "subtitle", "caption")) if (has(paste0("hide_", key)) && s[[paste0("hide_", key)]]) labels[key] <- list(NULL)
  if (length(labels)) plot <- plot + do.call(ggplot2::labs, labels)
  if (has("flip") && s$flip) {
    if (!inherits(plot$coordinates, "CoordCartesian") || inherits(plot$coordinates, "CoordSf")) stop("Axis swapping requires Cartesian coordinates.", call. = FALSE)
    coord <- rr_clone_ggproto(plot$coordinates)
    coord <- rr_clone_ggproto(if (inherits(coord, "CoordFlip")) ggplot2::coord_cartesian() else ggplot2::coord_flip(), limits = coord$limits, expand = coord$expand, clip = coord$clip, ratio = coord$ratio)
    plot$coordinates <- coord
  }
  if (has("aspect") && s$aspect == "fill") {
    if (!inherits(plot$coordinates, "CoordCartesian") || inherits(plot$coordinates, "CoordSf")) stop("Fill canvas is available for Cartesian plots; map and polar geometry keep their aspect.", call. = FALSE)
    # ggplot2 before 4.0 uses a separate CoordFixed whose aspect method
    # multiplies by ratio; switch that class to Cartesian when releasing it.
    coord <- plot$coordinates
    plot$coordinates <- if (inherits(coord, "CoordFixed")) ggplot2::coord_cartesian(
      xlim = coord$limits$x, ylim = coord$limits$y, expand = coord$expand, clip = coord$clip) else rr_clone_ggproto(coord, ratio = NULL)
    plot <- plot + ggplot2::theme(aspect.ratio = NULL)
  }
  plot
}







