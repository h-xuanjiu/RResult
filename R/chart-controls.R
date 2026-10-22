# Shared chart controls, keeping the canvas separate from settings.

rr_data_options_input <- function(input, x, limits) {
  options <- list(colour_by = input$report_colour_by, facet_by = input$report_facet_by,
    x_as_category = isTRUE(input$report_x_category), y_as_category = isTRUE(input$report_y_category))
  # Control compatibility depends on column types, not the observation values.
  # Do not allocate the full selected data just to decide whether colour is used.
  two_categories <- tryCatch({
    p <- rr_plain(x)
    defaults <- if (is.null(input$report_axis_x) || is.null(input$report_axis_y)) rr_default_axes(x, limits) else NULL
    ax <- rr_integer(as.numeric(if (is.null(input$report_axis_x)) defaults[1L] else input$report_axis_x), "X column", upper = length(p))
    ay <- rr_integer(as.numeric(if (is.null(input$report_axis_y)) defaults[2L] else input$report_axis_y), "Y column", 0, length(p))
    tx <- rr_variable_type(p[[ax]])
    ty <- if (ay) rr_variable_type(p[[ay]]) else NULL
    !identical(tx, "unsupported") && !identical(ty, "unsupported") &&
      (identical(tx, "category") || options$x_as_category) &&
      ay > 0L && (identical(ty, "category") || options$y_as_category)
  }, error = function(e) FALSE)
  if (isTRUE(two_categories)) options$colour_by <- NULL
  options
}

rr_data_axes_controls <- function(x, limits) {
  types <- rr_plot_columns(x, limits)
  cols <- as.integer(names(types))
  choices <- stats::setNames(cols, paste0(cols, ": ", rr_text(names(rr_plain(x))[cols], 38L), " [", types, "]"))
  defaults <- rr_default_axes(x, limits)
  categorical <- choices[types == "category"]
  htmltools::tagList(
    htmltools::tags$div(class = "rr-control-grid",
      shiny::selectInput("report_axis_x", "X variable", choices, selected = defaults[1L]),
      shiny::selectInput("report_axis_y", "Y variable", c("None: distribution" = 0, choices[unname(choices) != defaults[1L]]), selected = defaults[2L])),
    htmltools::tags$details(class = "rr-style", htmltools::tags$summary("Grouping and variable roles"),
      shiny::checkboxInput("report_x_category", "Treat X as categories", FALSE),
      shiny::checkboxInput("report_y_category", "Treat Y as categories", FALSE),
      shiny::conditionalPanel("!['bar','proportion','count_heatmap','count_bubble'].includes(input.report_plot_type)",
        shiny::selectInput("report_colour_by", "Colour by", c("None" = 0, categorical))),
      shiny::selectInput("report_facet_by", "Separate panels by", c("None" = 0, categorical)),
      shiny::selectInput("report_facet_scales", "Panel scales", c("Shared" = "fixed", "Free Y" = "free_y", "Free X" = "free_x", "Free X and Y" = "free")),
      htmltools::tags$p(class = "rr-chart-hint", "Category switches affect this chart only. Colour and panel grouping use categorical columns; all observed groups and panels are retained.")))
}

rr_export_controls <- function(imported = FALSE) {
  htmltools::tags$details(class = "rr-style", htmltools::tags$summary("Canvas and export size"),
    shiny::checkboxInput("report_auto_height", "Fit canvas height to coordinate ratio", !imported),
    shiny::sliderInput("report_height", "Canvas height / fallback (pixels)", min = 240, max = 1800, value = 600, step = 20),
    htmltools::tags$div(class = "rr-control-grid",
      shiny::numericInput("export_width", "Width (inches)", value = 10, min = 2, max = 40, step = .5),
      shiny::numericInput("export_height", "Height (inches)", value = 6, min = 2, max = 32, step = .5)),
    shiny::selectInput("export_dpi", "PNG resolution", c("Screen: 120 dpi" = 120, "Standard: 180 dpi" = 180, "Print: 300 dpi" = 300, "High: 600 dpi" = 600), selected = 180),
    htmltools::tags$p(class = "rr-chart-hint", "Automatic height reduces blank space around equal-unit plots. Style settings includes a Fill available canvas option. Export dimensions are independent of screen size."))
}

rr_export_dimensions <- function(input) {
  value <- function(key, default, lower, upper) {
    x <- input[[key]]
    if (is.null(x)) x <- default
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || !is.finite(x) || x < lower || x > upper) stop("Invalid export dimension: ", key, call. = FALSE)
    x
  }
  list(width = value("export_width", 10, 2, 40), height = value("export_height", 6, 2, 32), dpi = value("export_dpi", 180, 72, 600))
}

rr_chart_panel <- function(imported = FALSE) {
  prefix <- if (imported) "ggplot_" else "report_"
  htmltools::tags$section(class = "rr-card rr-chart-panel", id = "rr_chart_panel",
    htmltools::tags$div(class = "rr-chart-toolbar",
      htmltools::tags$div(htmltools::tags$div(class = "rr-eyebrow", "CHART STUDIO"), htmltools::tags$h3("Explore the visual evidence")),
      htmltools::tags$div(class = "rr-chart-actions",
        htmltools::tags$button(type = "button", class = "btn rr-secondary", `data-rr-action` = "style-open", `data-rr-target` = paste0(prefix, "style_dialog"), "Style settings"),
        htmltools::tags$button(type = "button", class = "btn rr-secondary", `data-rr-action` = "settings", `aria-expanded` = "true", "Hide settings"),
        htmltools::tags$button(type = "button", class = "btn rr-secondary", `data-rr-action` = "expand", `aria-expanded` = "false", "Expand chart"))),
    htmltools::tags$div(class = "rr-chart-card",
      htmltools::tags$aside(class = "rr-chart-controls", shiny::uiOutput("report_chart_controls")),
      htmltools::tags$div(class = "rr-chart-stage", shiny::uiOutput("report_chart_empty"), shiny::plotOutput("report_plot", height = "auto"))),
    rr_style_controls(prefix, imported = imported, launcher = FALSE))
}

rr_canvas_plot <- function(plot, width = NULL, dpi = 110) {
  style <- attr(plot, "rresult_style", exact = TRUE)
  if (is.null(style)) return(plot)
  if (is.null(width) || !is.finite(width) || width < 100) width <- 800
  for (key in c("title", "subtitle", "caption")) {
    value <- plot$labels[[key]]
    size <- style[[paste0(key, "_size")]]
    if (is.null(size)) size <- style$base_size * switch(key, title = 1.2, subtitle = .85, caption = .75)
    chars <- max(8L, min(160L, floor((width - 80) / (size * dpi / 72 * .52))))
    if (is.character(value) && length(value) == 1L) plot$labels[[key]] <- paste(strwrap(gsub("\n", " ", value, fixed = TRUE), width = chars), collapse = "\n")
  }
  plot
}

rr_canvas_height <- function(plot, width = NULL, fallback = 600, auto = TRUE) {
  fallback <- rr_integer(as.numeric(fallback), "canvas height", 240, 1800)
  if (!isTRUE(auto) || !inherits(plot, "ggplot") || is.null(width) || !is.finite(width) || width < 100) return(fallback)
  ratio <- plot$theme$aspect.ratio
  coord <- plot$coordinates
  if (is.null(ratio) && (is.null(coord$ratio) || !inherits(coord, "CoordCartesian"))) return(fallback)
  built <- tryCatch(ggplot2::ggplot_build(plot), error = function(e) NULL)
  if (is.null(built)) return(fallback)
  if (is.null(ratio)) ratio <- coord$aspect(built$layout$panel_params[[1L]])
  if (length(ratio) != 1L || !is.finite(ratio) || ratio <= 0) return(fallback)
  layout <- built$layout$layout
  nr <- max(layout$ROW); nc <- max(layout$COL)
  style <- attr(plot, "rresult_style", exact = TRUE)
  base <- if (is.null(style)) 14 else style$base_size
  # Budget for axis labels, wrapped headings and legend, then match panel ratio.
  wrapped <- rr_canvas_plot(plot, width)
  lines <- sum(vapply(c("title", "subtitle", "caption"), function(key) {
    v <- wrapped$labels[[key]]
    if (is.character(v) && length(v) == 1L && nzchar(v)) length(strsplit(v, "\n", fixed = TRUE)[[1L]]) else 0L
  }, integer(1)))
  overhead <- 65 + base * (3 + lines * 1.5)
  as.integer(max(240, min(1800, (max(100, width - 120) / nc) * ratio * nr + overhead)))
}
