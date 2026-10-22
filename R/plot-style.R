# Shared appearance settings used by the viewer and exported plots.
rr_style <- function(style = list()) {
  defaults <- list(theme = "minimal", palette = "teal", base_size = 14, point_size = 2.4,
    alpha = .7, line_width = .8, point_shape = 16, legend = "bottom",
    title = NULL, subtitle = NULL, caption = NULL, x_label = NULL, y_label = NULL,
    colour = NULL, fill = NULL, colours = NULL, fills = NULL, shapes = NULL,
    flip = FALSE, x_angle = 0, y_angle = 0, grid = "major", aspect = "preserve",
    font_family = NULL, font_face = NULL, text_colour = NULL, label_size = NULL,
    title_size = NULL, subtitle_size = NULL, caption_size = NULL, axis_title_size = NULL,
    axis_text_size = NULL, legend_title_size = NULL, legend_text_size = NULL, strip_text_size = NULL,
    title_hjust = NULL, stroke = NULL, line_type = NULL, ribbon_alpha = NULL,
    axis_lines = "theme", axis_ticks = "theme", axis_colour = NULL, axis_line_width = NULL,
    tick_length = NULL, tick_width = NULL, show_x_text = TRUE, show_y_text = TRUE,
    grid_colour = NULL, grid_width = NULL, grid_type = NULL, panel_border = "theme",
    border_colour = NULL, border_width = NULL, panel_fill = NULL, plot_fill = NULL,
    legend_fill = NULL, strip_fill = NULL, panel_spacing = NULL, plot_margin = NULL,
    legend_key_size = NULL, legend_rows = NULL, hide_title = FALSE, hide_subtitle = FALSE, hide_caption = FALSE)
  if (!is.list(style) || (length(style) && (is.null(names(style)) || any(!names(style) %in% names(defaults)) || anyDuplicated(names(style)))))
    stop("style must be a named list of supported appearance options.", call. = FALSE)
  for (nm in names(style)) defaults[nm] <- style[nm]
  enums <- list(theme = c("minimal", "classic", "light", "dark", "bw", "grey", "linedraw", "void", "test"),
    palette = c("teal", "ocean", "colorblind", "purple", "viridis", "sunset", "grey", "inferno", "blue_red"),
    legend = c("bottom", "right", "top", "left", "none"), grid = c("theme", "major", "all", "none"),
    aspect = c("preserve", "fill"), axis_lines = c("theme", "both", "x", "y", "none"),
    axis_ticks = c("theme", "both", "x", "y", "none"), panel_border = c("theme", "show", "hide"),
    font_face = c("plain", "bold", "italic", "bold.italic"),
    line_type = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank"),
    grid_type = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "blank"))
  for (nm in names(enums)) {
    v <- defaults[[nm]]
    if (!is.null(v) && (!is.character(v) || length(v) != 1L || is.na(v) || !v %in% enums[[nm]])) stop("Invalid style ", nm, ".", call. = FALSE)
  }
  ranges <- list(base_size = c(6, 72), point_size = c(.1, 24), alpha = c(0, 1), line_width = c(.05, 12),
    point_shape = c(0, 25), x_angle = c(-180, 180), y_angle = c(-180, 180), label_size = c(.5, 24),
    title_hjust = c(0, 1), stroke = c(0, 8), ribbon_alpha = c(0, 1), axis_line_width = c(.05, 6),
    tick_length = c(0, 15), tick_width = c(.05, 6), grid_width = c(.05, 4), border_width = c(.05, 6),
    panel_spacing = c(0, 40), legend_key_size = c(1, 40), legend_rows = c(1, 20))
  for (nm in c("title_size", "subtitle_size", "caption_size", "axis_title_size", "axis_text_size", "legend_title_size", "legend_text_size", "strip_text_size")) ranges[[nm]] <- c(4, 120)
  for (nm in names(ranges)) {
    v <- defaults[[nm]]
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < ranges[[nm]][1L] || v > ranges[[nm]][2L])) stop("Invalid style ", nm, ".", call. = FALSE)
  }
  defaults$point_shape <- rr_integer(defaults$point_shape, "point_shape", 0, 25)
  if (!is.null(defaults$legend_rows)) defaults$legend_rows <- rr_integer(defaults$legend_rows, "legend_rows", 1, 20)
  for (nm in c("flip", "show_x_text", "show_y_text", "hide_title", "hide_subtitle", "hide_caption"))
    if (!is.logical(defaults[[nm]]) || length(defaults[[nm]]) != 1L || is.na(defaults[[nm]])) stop("Invalid style ", nm, ".", call. = FALSE)
  for (nm in c("colour", "fill", "text_colour", "axis_colour", "grid_colour", "border_colour", "panel_fill", "plot_fill", "legend_fill", "strip_fill", "colours", "fills")) {
    v <- defaults[[nm]]
    if (!is.null(v)) {
      many <- nm %in% c("colours", "fills")
      if (!is.character(v) || !length(v) || length(v) > (if (many) 256L else 1L) || anyNA(v) || inherits(try(grDevices::col2rgb(v), silent = TRUE), "try-error")) stop("Invalid style ", nm, ".", call. = FALSE)
      if (!is.null(names(v)) && (anyNA(names(v)) || any(!nzchar(names(v))) || anyDuplicated(names(v)))) stop("Colour group names must be unique and non-empty.", call. = FALSE)
    }
  }
  v <- defaults$shapes
  if (!is.null(v) && (!is.numeric(v) || !length(v) || length(v) > 26L || anyNA(v) || any(v != floor(v) | v < 0 | v > 25) || (!is.null(names(v)) && (anyNA(names(v)) || any(!nzchar(names(v))) || anyDuplicated(names(v)))))) stop("Invalid style shapes (use integers 0-25).", call. = FALSE)
  v <- defaults$plot_margin
  if (!is.null(v) && (!is.numeric(v) || !length(v) %in% c(1L, 4L) || anyNA(v) || any(!is.finite(v) | v < 0 | v > 120))) stop("plot_margin must contain one or four values between 0 and 120 points.", call. = FALSE)
  for (nm in c("title", "subtitle", "caption", "x_label", "y_label", "font_family")) {
    v <- defaults[[nm]]
    if (!is.null(v) && (!is.character(v) || length(v) != 1L || is.na(v) || nchar(v, type = "bytes") > 500L)) stop("Style labels and font families must be at most 500 bytes.", call. = FALSE)
  }
  defaults
}

rr_palette <- function(name) {
  switch(name, teal = c("#187f88", "#d47b50", "#647bb4", "#aa6290", "#8a9c45", "#d0a344", "#487ba1", "#7b6c63"),
    ocean = c("#2864a0", "#e49b41", "#52a69c", "#a879b3", "#cf6d73", "#748b3b", "#64708e", "#9b7252"),
    colorblind = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#4D4D4D", "#F0E442"),
    purple = c("#7655a3", "#37a49a", "#d88852", "#ba668a", "#718942", "#628ac0", "#a57c46", "#6a737c"),
    viridis = grDevices::hcl.colors(8, "Viridis"), sunset = grDevices::hcl.colors(8, "Sunset"),
    grey = grDevices::gray.colors(8, start = .2, end = .8), inferno = grDevices::hcl.colors(8, "Inferno"),
    blue_red = c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"))
}

rr_style_theme <- function(name) {
  get(paste0("theme_", name), envir = asNamespace("ggplot2"))
}

rr_apply_style <- function(plot, style = list()) {
  s <- rr_style(style)
  requested <- names(style)
  angle <- plot$theme$axis.text.x$angle
  hjust <- plot$theme$axis.text.x$hjust
  plot <- plot + rr_style_theme(s$theme)(base_size = s$base_size) + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"), plot.title.position = "plot", plot.caption.position = "plot",
    plot.subtitle = ggplot2::element_text(size = s$base_size * .85, lineheight = 1.2, margin = ggplot2::margin(b = 8)),
    plot.caption = ggplot2::element_text(size = s$base_size * .75, hjust = 0, lineheight = 1.2),
    plot.margin = ggplot2::margin(10, 12, 10, 10))
  if (s$theme == "dark") plot <- plot + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#262d38", colour = NA),
    panel.background = ggplot2::element_rect(fill = "#303846", colour = NA),
    panel.grid = ggplot2::element_line(colour = "#465263"),
    text = ggplot2::element_text(colour = "#f0f4f8"), axis.text = ggplot2::element_text(colour = "#d6e0eb"),
    legend.background = ggplot2::element_rect(fill = "#262d38", colour = NA), legend.key = ggplot2::element_rect(fill = "#303846", colour = NA))
  if (length(angle) && !"x_angle" %in% requested) plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust))
  active <- union(c("base_size", "point_size", "alpha", "line_width", "point_shape", "legend", "grid", "palette", "label_size"), requested)
  if (is.null(s$label_size)) s$label_size <- s$base_size / 4
  plot <- rr_style_layers(plot, s, active, generated = TRUE)
  plot <- rr_style_scales(plot, s, active)
  plot <- rr_style_details(plot, s, active)
  attr(plot, "rresult_style") <- s
  plot
}


