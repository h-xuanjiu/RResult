# Bioinformatics-specific selectors keep saved assay/layer choices explicit.
rr_bio_options_input <- function(input, kind, x = NULL) {
  options <- list(assay = input$bio_assay,
    layer = if (kind %in% c("Seurat", "Assay", "Assay5")) input$bio_layer else NULL,
    bio_coef = input$bio_coef)
  if (kind == "Seurat" && !is.null(x)) {
    assay <- if (is.null(options$assay)) SeuratObject::DefaultAssay(x) else options$assay
    choices <- SeuratObject::Layers(x[[assay]], search = NA)
    if (!is.null(options$layer) && !options$layer %in% choices) options$layer <- NULL
  }
  options
}

rr_bio_plot_options <- function(input, kind, x = NULL) {
  options <- rr_bio_options_input(input, kind, x)
  options$reduction <- input$bio_reduction
  value <- input$bio_feature
  if (!is.null(value) && nzchar(trimws(value))) options$feature <- trimws(unlist(strsplit(value, "[,;\n]")))
  options$colour_by <- if (identical(input$report_plot_type, "metadata_distribution")) input$bio_metric else input$bio_colour_by
  options$shape_by <- input$bio_shape_by
  options$facet_by <- input$bio_facet_by
  options$top_terms <- if (is.null(input$bio_top_terms)) 20L else input$bio_top_terms
  options
}

rr_bio_controls <- function(x, kind, limits, input, source = NULL) {
  get <- function(key, default = NULL) {
    value <- shiny::isolate(input[[key]])
    if (is.null(value)) default else value
  }
  selected <- function(key, choices, default = NULL) {
    value <- get(key)
    if (length(value) && value %in% unname(choices)) value else if (is.null(default)) unname(choices[1L]) else default
  }
  if (kind %in% .rr_bio_containers) {
    if (is.null(source)) {
      options <- rr_bio_options_input(input, kind, x)
      source <- rr_bio_source(x, limits, options$assay, options$layer)
    }
    s <- source
    columns <- if (is.null(s$meta)) character() else names(s$meta)
    eligible <- columns[vapply(columns, function(nm) rr_bio_column_type(s$meta[[nm]]) %in% c("numeric", "category"), logical(1))]
    category <- eligible[vapply(eligible, function(nm) rr_bio_column_type(s$meta[[nm]]) == "category", logical(1))]
    eligible <- stats::setNames(eligible, eligible); category <- stats::setNames(category, category)
    if (!is.null(s$identity)) { eligible <- c(eligible, "Saved identities" = s$identity_key); category <- c(category, "Saved identities" = s$identity_key) }
    numeric <- eligible[vapply(unname(eligible), function(nm) !identical(nm, s$identity_key) && rr_bio_column_type(s$meta[[nm]]) == "numeric", logical(1))]
    group_choices <- c("None / automatic for distributions" = "0", eligible)
    category_choices <- c("None" = "0", category)
    reduction <- rr_bio_reduction(s)
    htmltools::tagList(
      shiny::selectInput("bio_assay", "Expression assay", s$assays, selected = s$assay),
      if (kind %in% c("Seurat", "Assay", "Assay5")) shiny::selectInput("bio_layer", "Saved layer (one at a time)", s$layers, selected = s$layer),
      shiny::conditionalPanel("['embedding','feature_map'].includes(input.report_plot_type)",
        shiny::selectInput("bio_reduction", "Saved reduction", s$reductions, selected = selected("bio_reduction", s$reductions, reduction))),
      shiny::conditionalPanel("['embedding','feature_map','spatial','spatial_feature'].includes(input.report_plot_type)",
        shiny::numericInput("report_axis_x", "X coordinate dimension", get("report_axis_x", 1), min = 1, step = 1),
        shiny::numericInput("report_axis_y", "Y coordinate dimension", get("report_axis_y", 2), min = 1, step = 1)),
      shiny::conditionalPanel("['feature_map','spatial_feature','expression_heatmap','expression_violin','expression_dot'].includes(input.report_plot_type)",
        shiny::textInput("bio_feature", "Feature IDs (comma-separated; blank = automatic)", get("bio_feature", "")),
        htmltools::tags$p(class = "rr-chart-hint", "Blank selects the first 6 genes for grouped summaries, 20 for heatmaps, or 1 for single-gene views. Enter IDs to choose your genes."),
        htmltools::tags$p(class = "rr-chart-hint", paste("Example feature IDs:", paste(rr_text(utils::head(s$features, 5L), 35L), collapse = ", ")))),
      shiny::conditionalPanel("!['feature_map','spatial_feature','expression_heatmap','metadata_distribution'].includes(input.report_plot_type)",
        shiny::selectInput("bio_colour_by", "Colour / categorical group", group_choices, selected = selected("bio_colour_by", group_choices, "0"))),
      shiny::conditionalPanel("input.report_plot_type === 'metadata_distribution'",
        shiny::selectInput("bio_metric", "Saved numeric metadata metric", numeric, selected = selected("bio_metric", numeric))),
      shiny::conditionalPanel("['embedding','feature_map','spatial','spatial_feature'].includes(input.report_plot_type)",
        shiny::selectInput("bio_shape_by", "Shape by", category_choices, selected = selected("bio_shape_by", category_choices, "0")),
        shiny::selectInput("bio_facet_by", "Separate panels by", category_choices, selected = selected("bio_facet_by", category_choices, "0"))),
      htmltools::tags$p(class = "rr-chart-hint", "Saved maps display all available coordinates. Gene means and above-zero percentages use all observations in the selected source. Expression heatmaps and result tables are windowed previews."))
  } else htmltools::tagList(
    if (kind == "MArrayLM") {
      choices <- rr_bio_coef(x)$choices
      shiny::selectInput("bio_coef", "Saved coefficient / contrast", choices, selected = selected("bio_coef", choices))
    },
    if (kind %in% c("DGEExact", "DGELRT", "MArrayLM")) htmltools::tagList(
      shiny::numericInput("de_padj_cutoff", "Display P / FDR cutoff", get("de_padj_cutoff", .05), min = .000001, max = 1, step = .01),
      shiny::numericInput("de_lfc_cutoff", "Display absolute effect cutoff", get("de_lfc_cutoff", 0), min = 0, step = .25)),
    if (kind %in% c("enrichResult", "gseaResult", "compareClusterResult")) shiny::numericInput("bio_top_terms", "Leading terms per displayed group", get("bio_top_terms", 20), min = 1, max = 100, step = 1),
    htmltools::tags$p(class = "rr-chart-hint", "Uses saved tests only. Any BH adjustment covers the full stored gene family; gene-result charts retain all saved rows."))
}






