# Reusable result presentation ------------------------------------------------

rr_metric_cards <- function(metrics) {
  htmltools::tags$div(class = "rr-metrics rr-result-metrics", lapply(names(metrics), function(label) {
    htmltools::tags$div(class = "rr-metric", htmltools::tags$span(label), htmltools::tags$strong(rr_fmt(metrics[[label]])))
  }))
}

rr_formatted_table <- function(data) {
  for (j in seq_len(ncol(data))) if (is.numeric(data[[j]]) && length(data[[j]])) data[[j]] <- rr_fmt(data[[j]])
  rr_table(data)
}

rr_result_ui <- function(report, module = "overview", table = NULL, limits = rr_limits(),
                         page = 1L, page_size = min(25L, limits$max_rows), column_start = 1L) {
  if (inherits(report, "error")) return(rr_notice(conditionMessage(report), TRUE))
  if (module == "interpretation") return(htmltools::tags$section(class = "rr-card",
    htmltools::tags$div(class = "rr-eyebrow", "READ THE RESULT"), htmltools::tags$h2("What the result tells you"),
    htmltools::tags$div(class = "rr-insights", lapply(report$highlights, function(text)
      htmltools::tags$p(class = "rr-insight", text))),
    htmltools::tags$h3("Context for interpretation"), htmltools::tags$ul(class = "rr-reading-notes", lapply(report$notes, htmltools::tags$li))))
  if (module %in% c("results", "coefficients")) {
    choices <- if (is.null(report$table_choices)) names(report$tables) else report$table_choices
    if (is.null(table) || !table %in% choices) table <- choices[1L]
    data <- report$tables[[table]]
    window <- if (is.null(data)) NULL else tryCatch(rr_report_page(data, limits, page, page_size, column_start), error = function(e) e)
    controls <- if (!is.null(window) && !inherits(window, "error")) htmltools::tagList(
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("report_table_page", "Page", window$page, min = 1, max = window$pages, step = 1)),
        shiny::column(4, shiny::numericInput("report_table_size", "Rows per page", window$page_size, min = 1, max = limits$max_rows, step = 1)),
        shiny::column(4, shiny::numericInput("report_table_col", "First column", if (length(window$cols)) window$cols[1L] else 1L, min = 1, max = max(1, ncol(data)), step = 1))),
      shiny::actionButton("report_table_prev", "Previous page", class = "rr-secondary"),
      shiny::actionButton("report_table_next", "Next page", class = "rr-secondary"),
      rr_notice(paste("Rows", if (length(window$rows)) paste(range(window$rows), collapse = "- ") else "0", "of", window$total_rows,
        "| Columns", if (length(window$cols)) paste(range(window$cols), collapse = "- ") else "0", "of", window$total_cols,
        "| Page", window$page, "of", window$pages))) else if (!is.null(data)) htmltools::tagList(
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("report_table_page", "Page", 1, min = 1, step = 1)),
        shiny::column(4, shiny::numericInput("report_table_size", "Rows per page", min(25L, limits$max_rows), min = 1, max = limits$max_rows, step = 1)),
        shiny::column(4, shiny::numericInput("report_table_col", "First column", 1, min = 1, max = max(1, ncol(data)), step = 1))),
      rr_notice("Reduce the rows per page or change the first column to fit the preview budget. Table CSV remains available."))
    return(htmltools::tags$section(class = "rr-card", htmltools::tags$div(class = "rr-eyebrow", "RESULT DETAILS"),
      if (module == "results") shiny::selectInput("report_table", "Result table", choices, selected = table),
      htmltools::tags$h2(table), controls,
      if (is.null(window)) rr_notice("This saved table is unavailable for the selected source.", TRUE) else
        if (inherits(window, "error")) rr_notice(conditionMessage(window), TRUE) else rr_formatted_table(window$data),
      rr_notice(if (!is.null(report$table_scope[[table]])) report$table_scope[[table]] else
        "Browse every saved row and column using pages. Table CSV saves the entire table with full numeric precision, independently of the current page.")))
  }
  htmltools::tagList(
    htmltools::tags$div(class = "rr-report-hero", htmltools::tags$div(
      htmltools::tags$div(class = "rr-eyebrow", "RESULT OVERVIEW"), htmltools::tags$h2(report$title),
      htmltools::tags$p(report$subtitle)), htmltools::tags$span(class = "rr-result-badge", "FROM YOUR OBJECT")),
    rr_metric_cards(report$metrics),
    htmltools::tags$div(class = "rr-insights", lapply(report$highlights, function(text)
      htmltools::tags$p(class = "rr-insight", text))),
    htmltools::tags$details(class = "rr-context", htmltools::tags$summary("How to read this result"),
      htmltools::tags$ul(class = "rr-reading-notes", lapply(report$notes, htmltools::tags$li))))
}

rr_summary_ui <- function(r) {
  metrics <- list("Values inspected" = r$used, "Total values" = r$population, Missing = r$missing,
    "Non-finite (excl. NA)" = if (is.null(r$non_finite)) "-" else r$non_finite)
  htmltools::tagList(htmltools::tags$h3("Variable profile"), rr_metric_cards(metrics),
    rr_notice(if (isTRUE(r$sampled)) paste("Statistics use", r$used, "evenly spaced sampled values.") else "All values included within the configured limit.") ,
    if (length(r$statistics)) rr_formatted_table(data.frame(statistic = names(r$statistics), value = as.numeric(r$statistics))),
    if (length(r$time_range)) htmltools::tags$p(paste("Time range:", paste(r$time_range, collapse = " to "))),
    if (length(r$frequencies)) rr_formatted_table(data.frame(category = names(r$frequencies), count = as.numeric(r$frequencies))),
    htmltools::tags$details(htmltools::tags$summary("Inspect sampled values"), rr_table(r$preview)))
}

rr_report_html <- function(report, plot = NULL, width = 10, height = 6, dpi = 130) {
  chart <- NULL
  if (!is.null(plot) && inherits(plot, "ggplot")) {
    image_path <- tempfile(fileext = ".png")
    on.exit(unlink(image_path), add = TRUE)
    ggplot2::ggsave(image_path, rr_canvas_plot(plot, width * dpi, dpi = dpi), width = width, height = height, dpi = dpi, bg = NULL)
    encoded <- jsonlite::base64_enc(readBin(image_path, "raw", n = file.info(image_path)$size))
    chart <- htmltools::tags$img(src = paste0("data:image/png;base64,", encoded), style = "width:100%;height:auto;", alt = "Selected result chart")
  }
  css <- paste(readLines(system.file("www", "workspace.css", package = "RResult"), warn = FALSE), collapse = "\n")
  as.character(htmltools::tags$html(lang = "en", htmltools::tags$head(htmltools::tags$meta(charset = "utf-8"),
    htmltools::tags$title(report$title), htmltools::tags$style(htmltools::HTML(css))),
    htmltools::tags$body(htmltools::tags$main(class = "rr-main",
      htmltools::tags$p(class = "rr-eyebrow", "RRESULT / SAVED RESULT REPORT"), rr_result_ui(report), chart,
      lapply(names(report$tables), function(name) htmltools::tags$section(class = "rr-card",
        htmltools::tags$h2(name), if (!is.null(report$table_scope[[name]])) rr_notice(report$table_scope[[name]]),
        rr_formatted_table(report$tables[[name]]))),
      htmltools::tags$p(class = "rr-footer", "Saved from the supplied R object. Saved result tables include all rows; named windows and source-data previews retain their stated scope. This file can be opened offline.")))))
}

