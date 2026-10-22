# Controls describe the analysis and keep optional metadata roles explicit.

rr_ordination_controls <- function(x, metadata, sample_id, limits, input = NULL) {
  get <- function(key, default) {
    value <- if (is.null(input)) NULL else shiny::isolate(input[[key]])
    if (is.null(value)) default else value
  }
  selected <- function(key, choices, default) {
    value <- get(key, default)
    if (length(value) == 1L && value %in% unname(choices)) value else default
  }
  o <- rr_ordination(x)
  count <- o$axes
  axes <- seq_len(count)
  controls <- NULL
  axis_x <- selected("report_axis_x", axes, 1L)
  vertical <- axes[axes != as.integer(axis_x)]
  if (count >= 2L) controls <- shiny::conditionalPanel("['scores','biplot','environment','loadings','species'].includes(input.report_plot_type)",
    htmltools::tags$div(class = "rr-control-grid",
      shiny::selectInput("report_axis_x", "Horizontal axis", stats::setNames(axes, paste("Axis", axes)), selected = axis_x),
      shiny::selectInput("report_axis_y", "Vertical axis", stats::setNames(vertical, paste("Axis", vertical)),
        selected = selected("report_axis_y", vertical, vertical[1L]))))
  grouping <- rr_notice("Supply sample metadata when opening this analysis to enable colour groups, point shapes and separate panels.")
  if (!is.null(metadata)) {
    # Only inspect field types here; no observation values need copying for choices.
    matched <- rr_ordination_metadata(metadata, sample_id, o$scores, integer(), limits, preview = FALSE)
    d <- matched$data
    types <- vapply(d, rr_variable_type, character(1))
    choices <- stats::setNames(as.character(seq_along(d)), rr_text(names(d), 45L))
    categorical <- choices[types == "category"]
    grouping <- htmltools::tagList(
      htmltools::tags$p(class = "rr-chart-hint", paste("Metadata ready:", o$observations,
        "saved samples and", length(choices), "available fields. Choose a field below to colour the samples.")),
      shiny::selectInput("ord_colour", "Colour samples by", c("None" = 0, choices[types %in% c("category", "numeric")]),
        selected = selected("ord_colour", c(0, choices[types %in% c("category", "numeric")]), 0)),
      htmltools::tags$details(class = "rr-style", htmltools::tags$summary("Shapes, panels and group outlines"),
        shiny::selectInput("ord_shape", "Point shapes by", c("None" = 0, categorical),
          selected = selected("ord_shape", c(0, categorical), 0)),
        shiny::selectInput("ord_facet", "Separate panels by", c("None" = 0, categorical),
          selected = selected("ord_facet", c(0, categorical), 0)),
        shiny::checkboxInput("ord_hulls", "Outline displayed colour groups", get("ord_hulls", FALSE)),
        htmltools::tags$p(class = "rr-chart-hint", "Outlines show the extent of displayed samples; they are not confidence regions or significance tests."),
        htmltools::tags$p(class = "rr-chart-hint", matched$note)))
  }
  htmltools::tagList(
    shiny::conditionalPanel("['scores','biplot','environment'].includes(input.report_plot_type)", grouping),
    controls,
    shiny::conditionalPanel("['scores','biplot','environment'].includes(input.report_plot_type)",
      htmltools::tags$details(class = "rr-style", htmltools::tags$summary("Sample labels"),
        shiny::checkboxInput("ord_labels", "Show sample names", get("ord_labels", FALSE)),
        shiny::numericInput("ord_max_labels", "Maximum sample labels", get("ord_max_labels", 20), min = 0, max = 200, step = 1))),
    shiny::conditionalPanel("['biplot','environment','loadings','loadings_heatmap','species'].includes(input.report_plot_type)",
      shiny::numericInput("ord_top_variables", "Maximum variable / species labels", get("ord_top_variables", min(12L, limits$max_children)), min = 1, max = min(100L, limits$max_children))),
    htmltools::tags$p(class = "rr-chart-hint", if (o$kind %in% .rr_constrained_families) "Saved scaling-0 coordinates; axis percentages use total inertia, including conditioned and residual components." else
      if (o$method == "PCA") "Axis percentages describe variance in the full fitted PCA. Points are saved samples; arrows describe variables." else
      if (o$method == "PCoA") "Axis percentages use positive eigenvalue inertia. Saved corrections are retained." else
        "Stress describes the full saved configuration. NMDS axes do not have explained-variance percentages."))
}
