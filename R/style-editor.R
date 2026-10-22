# A persistent dialog keeps Shiny input values when closed or charts change.
rr_style_controls <- function(prefix, imported = FALSE, launcher = TRUE) {
  id <- function(key) paste0(prefix, key)
  number <- function(key, label, min, max, value = NA_real_, step = 1) shiny::numericInput(id(key), label, value, min, max, step)
  color <- function(key, label) shiny::textInput(id(key), label, placeholder = "#336699 / steelblue / transparent")
  original <- function(choices) if (imported) c("Keep original" = "original", choices) else choices
  themes <- c("Minimal" = "minimal", "Classic" = "classic", "Black and white" = "bw", "Light" = "light", "Dark" = "dark", "Grey" = "grey", "Line drawing" = "linedraw", "Void" = "void", "Test" = "test")
  palettes <- c("Teal / orange" = "teal", "Ocean" = "ocean", "Colorblind" = "colorblind", "Purple" = "purple", "Viridis" = "viridis", "Sunset" = "sunset", "Greys" = "grey", "Inferno" = "inferno", "Blue / red" = "blue_red")
  shapes <- stats::setNames(0:25, paste0(0:25, ": ", c("Open square", "Open circle", "Open triangle", "Plus", "Cross", "Open diamond", "Down triangle", "Square + cross", "Star", "Diamond + plus", "Circle + plus", "Double triangle", "Square + plus", "Circle + cross", "Square + triangle", "Solid square", "Solid circle", "Solid triangle", "Solid diamond", "Filled circle", "Small circle", "Circle with fill", "Square with fill", "Diamond with fill", "Triangle with fill", "Down triangle with fill")))
  lines <- c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted", "Dot dash" = "dotdash", "Long dash" = "longdash", "Two dash" = "twodash", "Hidden" = "blank")
  grid <- function(...) htmltools::tags$div(class = "rr-style-grid", ...)
  optional <- htmltools::tags$p(class = "rr-chart-hint", "Blank fields keep automatic settings. Font sizes are points; plot labels and point sizes are millimetres. Use fonts installed on this computer.")
  tabs <- shiny::tabsetPanel(id = id("style_tab"),
    shiny::tabPanel("Theme & colours",
      grid(shiny::selectInput(id("theme"), "Theme", original(themes)),
        shiny::selectInput(id("palette"), "Palette", original(palettes)),
        color("colour", "Fixed outline / point colour"), color("fill", "Fixed fill colour")),
      htmltools::tags$p(class = "rr-chart-hint", "Mapped groups use the palette or custom colours below. Fixed colours apply to marks without a colour mapping."),
      grid(shiny::textAreaInput(id("colours"), "Custom group / gradient colours", rows = 4, placeholder = "setosa = #0072B2\nversicolor = #D55E00\nvirginica = #009E73"),
        shiny::textAreaInput(id("fills"), "Custom group / gradient fills", rows = 4, placeholder = "#2166ac, #f7f7f7, #b2182b")),
      htmltools::tags$p(class = "rr-chart-hint", "For groups: one exact group name = colour per line; remaining groups keep palette colours. Or enter a colour list separated by commas/newlines, in scale order (usually legend order). Continuous gradients use an unnamed colour list.")),
    shiny::tabPanel("Points & lines",
      if (imported) shiny::checkboxInput(id("points_override"), "Adjust fixed point appearance", FALSE),
      grid(number("point", "Point size (mm)", .1, 24, 2.4, .1), number("alpha", "Point opacity", 0, 1, .7, .05),
        shiny::selectInput(id("shape"), "Fixed point shape (0-25)", shapes, selected = 16), number("stroke", "Point outline width", 0, 8, step = .1)),
      shiny::textInput(id("shapes"), "Mapped shapes (optional codes in scale order (usually legend order))", placeholder = "16, 17, 15, 18, 21, 22, 23, 24, 25"),
      if (imported) shiny::checkboxInput(id("line_override"), "Adjust fixed line width", FALSE),
      grid(number("line", "Line width (mm)", .05, 12, .8, .05), shiny::selectInput(id("line_type"), "Line type", c("Automatic" = "auto", lines)),
        number("ribbon_alpha", "Confidence band / ribbon opacity", 0, 1, step = .05)),
      htmltools::tags$p(class = "rr-chart-hint", "Data-mapped point sizes, opacity and line types retain their meaning. Shapes 21-25 use both colour and fill.")),
    shiny::tabPanel("Text & labels", optional,
      if (imported) shiny::checkboxInput(id("font_override"), "Adjust base font size", FALSE),
      grid(number("font", "Base font size (pt)", 6, 72, 14), number("label_size", "Labels inside the plot (mm)", .5, 24, step = .25),
        shiny::textInput(id("font_family"), "Font family", placeholder = "sans / serif / mono"),
        shiny::selectInput(id("font_face"), "Font face", c("Automatic" = "auto", "Plain" = "plain", "Bold" = "bold", "Italic" = "italic", "Bold italic" = "bold.italic")), color("text_colour", "Text colour")),
      grid(number("title_size", "Title size (pt)", 4, 120), number("subtitle_size", "Subtitle size (pt)", 4, 120),
        number("axis_title_size", "Axis title size (pt)", 4, 120), number("axis_text_size", "Tick label size (pt)", 4, 120),
        number("legend_title_size", "Legend title size (pt)", 4, 120), number("legend_text_size", "Legend label size (pt)", 4, 120),
        number("strip_text_size", "Facet label size (pt)", 4, 120), number("caption_size", "Caption size (pt)", 4, 120)),
      grid(shiny::textInput(id("title"), "Title (blank = default)"), shiny::textInput(id("subtitle"), "Subtitle (blank = default)"),
        shiny::textInput(id("x_label"), "X-axis title"), shiny::textInput(id("y_label"), "Y-axis title"), shiny::textInput(id("caption"), "Caption (blank = default)"), number("title_hjust", "Title alignment (0 = left, 1 = right)", 0, 1, step = .1)),
      grid(shiny::checkboxInput(id("hide_title"), "Hide title", FALSE), shiny::checkboxInput(id("hide_subtitle"), "Hide subtitle", FALSE), shiny::checkboxInput(id("hide_caption"), "Hide caption", FALSE))),
    shiny::tabPanel("Axes & grid",
      grid(shiny::selectInput(id("axis_lines"), "Axis lines", c("Theme default" = "theme", "Both axes" = "both", "X only" = "x", "Y only" = "y", "Hidden" = "none")),
        shiny::selectInput(id("axis_ticks"), "Axis ticks", c("Theme default" = "theme", "Both axes" = "both", "X only" = "x", "Y only" = "y", "Hidden" = "none")),
        color("axis_colour", "Axis / tick colour"), number("axis_line_width", "Axis line width (mm)", .05, 6, step = .05),
        number("tick_length", "Tick length (mm)", 0, 15, step = .5), number("tick_width", "Tick width (mm)", .05, 6, step = .05),
        number("x_angle", "X tick angle (degrees)", -180, 180), number("y_angle", "Y tick angle (degrees)", -180, 180)),
      grid(shiny::checkboxInput(id("show_x_text"), "Show X tick labels", TRUE), shiny::checkboxInput(id("show_y_text"), "Show Y tick labels", TRUE)),
      grid(shiny::selectInput(id("grid"), "Grid", c("Theme default" = "theme", "Major lines" = "major", "All lines" = "all", "Hidden" = "none"), selected = if (imported) "theme" else "major"),
        color("grid_colour", "Grid colour"), number("grid_width", "Grid line width (mm)", .05, 4, step = .05),
        shiny::selectInput(id("grid_type"), "Grid line type", c("Automatic" = "auto", lines)))),
    shiny::tabPanel("Layout & background",
      grid(shiny::selectInput(id("legend"), "Legend", original(c("Bottom" = "bottom", "Right" = "right", "Top" = "top", "Left" = "left", "Hidden" = "none"))),
        number("legend_rows", "Legend rows (discrete colours / shapes)", 1, 20), number("legend_key_size", "Legend key size (mm)", 1, 40),
        shiny::selectInput(id("aspect"), "Plot proportions", c("Preserve coordinate ratio" = "preserve", "Fill available canvas" = "fill")),
        number("plot_margin", "Outer margin (pt)", 0, 120), number("panel_spacing", "Space between facets (mm)", 0, 40),
        shiny::selectInput(id("panel_border"), "Panel border", c("Theme default" = "theme", "Show" = "show", "Hide" = "hide")),
        color("border_colour", "Border colour"), number("border_width", "Border width (mm)", .05, 6, step = .05),
        color("panel_fill", "Panel background"), color("plot_fill", "Canvas background"), color("legend_fill", "Legend background"), color("strip_fill", "Facet strip background")),
      shiny::checkboxInput(id("flip"), "Swap horizontal / vertical layout", FALSE),
      htmltools::tags$p(class = "rr-chart-hint", "Fill canvas lets X and Y use different physical unit lengths. Preserve the coordinate ratio when distances or equal units matter (ordination, ROC). Map and polar plots retain their geometry.")))
  htmltools::tagList(
    if (launcher) htmltools::tags$button(type = "button", class = "btn rr-secondary rr-style-launch", `data-rr-action` = "style-open", `data-rr-target` = id("style_dialog"), "Style settings"),
    htmltools::tags$dialog(id = id("style_dialog"), class = "rr-style-dialog", `aria-labelledby` = id("style_heading"),
      htmltools::tags$div(class = "rr-style-header", htmltools::tags$div(htmltools::tags$h3(id = id("style_heading"), "Chart style"), htmltools::tags$p("Changes apply live and are used by downloads.")),
        htmltools::tags$button(type = "button", class = "btn rr-secondary", `data-rr-action` = "style-close", "Done")), tabs))
}

rr_parse_style_values <- function(value, numeric = FALSE) {
  if (is.null(value) || !nzchar(trimws(value))) return(NULL)
  parts <- trimws(unlist(strsplit(value, "[,;\n]")))
  parts <- parts[nzchar(parts)]
  if (!length(parts) || length(parts) > 256L) stop("Supply between 1 and 256 colour/shape values.", call. = FALSE)
  named <- grepl("=", parts, fixed = TRUE)
  if (any(named) && !all(named)) stop("Use either named group values or an unnamed list, not both.", call. = FALSE)
  if (all(named)) {
    labels <- trimws(sub("=[^=]*$", "", parts))
    parts <- trimws(sub("^.*=", "", parts))
  } else labels <- NULL
  if (numeric) parts <- suppressWarnings(as.numeric(parts))
  stats::setNames(parts, labels)
}

rr_style_input <- function(input, prefix, imported = FALSE) {
  get <- function(key, default = NULL) { value <- input[[paste0(prefix, key)]]; if (is.null(value)) default else value }
  style <- if (imported) list() else list(theme = get("theme", "minimal"), palette = get("palette", "teal"), base_size = get("font", 14),
    point_size = get("point", 2.4), alpha = get("alpha", .7), line_width = get("line", .8), point_shape = as.numeric(get("shape", 16)),
    legend = get("legend", "bottom"), grid = get("grid", "major"))
  if (imported) {
    for (key in c("theme", "palette", "legend")) if (!is.null(get(key)) && get(key) != "original") style[[key]] <- get(key)
    if (isTRUE(get("font_override"))) style$base_size <- get("font", 14)
    if (isTRUE(get("points_override"))) {
      style$point_size <- get("point", 2.4); style$alpha <- get("alpha", .7); style$point_shape <- as.numeric(get("shape", 16))
    }
    if (isTRUE(get("line_override"))) style$line_width <- get("line", .8)
    if (!is.null(get("grid")) && get("grid") != "theme") style$grid <- get("grid")
  }
  for (key in c("title", "subtitle", "caption", "x_label", "y_label", "colour", "fill", "font_family", "text_colour", "axis_colour", "grid_colour", "border_colour", "panel_fill", "plot_fill", "legend_fill", "strip_fill"))
    if (!is.null(get(key)) && nzchar(trimws(get(key)))) style[[key]] <- get(key)
  for (key in c("x_angle", "y_angle", "label_size", "title_size", "subtitle_size", "caption_size", "axis_title_size", "axis_text_size", "legend_title_size", "legend_text_size", "strip_text_size", "title_hjust", "stroke", "ribbon_alpha", "axis_line_width", "tick_length", "tick_width", "grid_width", "border_width", "panel_spacing", "plot_margin", "legend_key_size", "legend_rows")) {
    v <- get(key)
    if (!is.null(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v)) && !identical(v, "auto")) style[[key]] <- suppressWarnings(as.numeric(v))
  }
  for (key in c("font_face", "line_type", "grid_type", "aspect", "axis_lines", "axis_ticks", "panel_border"))
    if (!is.null(get(key)) && !get(key) %in% c("auto", "theme", "preserve")) style[[key]] <- get(key)
  for (key in c("flip", "hide_title", "hide_subtitle", "hide_caption")) if (isTRUE(get(key))) style[[key]] <- TRUE
  for (key in c("show_x_text", "show_y_text")) if (identical(get(key), FALSE)) style[[key]] <- FALSE
  for (key in c("colours", "fills", "shapes")) {
    v <- rr_parse_style_values(get(key), numeric = key == "shapes")
    if (!is.null(v)) style[[key]] <- v
  }
  style
}

rr_ggplot_style_input <- function(input) rr_style_input(input, "ggplot_", imported = TRUE)

