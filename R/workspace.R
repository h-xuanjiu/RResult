# Shiny presentation ----------------------------------------------------------

rr_notice <- function(text, error = FALSE) {
  htmltools::tags$div(class = if (error) "rr-notice rr-error" else "rr-notice", text)
}

rr_table <- function(data, labels = names(data), row_labels = NULL) {
  if (is.null(data) || !nrow(data)) return(rr_notice("No matching rows. Adjust the range or query."))
  cell <- function(value) {
    value <- as.character(value)
    if (is.na(value)) value <- "NA"
    if (nchar(value) <= 160L) return(htmltools::tags$td(value))
    htmltools::tags$td(htmltools::tags$details(class = "rr-cell-details",
      htmltools::tags$summary(paste0(substr(value, 1L, 160L), "\u2026 [full value]")),
      htmltools::tags$pre(style = "white-space:pre-wrap;overflow-wrap:anywhere;max-width:70vw", value)))
  }
  htmltools::tags$div(class = "rr-table-wrap", htmltools::tags$table(class = "rr-table",
    htmltools::tags$thead(htmltools::tags$tr(if (!is.null(row_labels)) htmltools::tags$th("Row"), lapply(labels, htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(data)), function(i) htmltools::tags$tr(
      if (!is.null(row_labels)) cell(row_labels[i]),
      lapply(seq_len(ncol(data)), function(j) cell(data[[j]][i])))))))
}

rr_metrics <- function(info) {
  metrics <- list(Class = paste(info$class, collapse = " / "),
                  Dimensions = if (length(info$dim)) paste(info$dim, collapse = " x ") else paste("Length", info$length),
                  Storage = if (!is.null(info$storage_type)) info$storage_type else info$typeof,
                  Memory = if (is.na(info$size_bytes)) "Not traversed" else paste(format(round(info$size_bytes / 1024^2, 3), trim = TRUE), "MiB"))
  htmltools::tags$div(class = "rr-metrics", lapply(names(metrics), function(name)
    htmltools::tags$div(class = "rr-metric", htmltools::tags$span(name), htmltools::tags$strong(metrics[[name]]))))
}

rr_workspace_ui <- function(info, adapter, view) {
  asset_files <- system.file("www", c("workspace.css", "workspace.js"), package = "RResult")
  asset_stamp <- floor(max(as.numeric(file.info(asset_files)$mtime), na.rm = TRUE)) %% 100000000
  dep <- htmltools::htmlDependency("rresult-workspace", paste0("0.1.0.", asset_stamp),
    src = c(file = system.file("www", package = "RResult")), stylesheet = "workspace.css", script = "workspace.js")
  header <- htmltools::tags$header(class = "rr-header",
    htmltools::tags$div(htmltools::tags$div(class = "rr-brand", "R", htmltools::tags$span("Result")),
      htmltools::tags$p("Live object workspace")),
    htmltools::tags$div(class = "rr-header-actions", htmltools::tags$span(class = "rr-local", "LOCAL SESSION"),
      shiny::checkboxInput("dark_mode", "Dark", FALSE), shiny::actionButton("close_workspace", "Close workspace", class = "rr-secondary")))
  explorer <- shiny::tabPanel("Object explorer", value = "explorer",
    htmltools::tags$div(class = "rr-explorer-grid",
      htmltools::tags$aside(class = "rr-card rr-tree",
        htmltools::tags$div(class = "rr-eyebrow", "OBJECT STRUCTURE"), shiny::textOutput("breadcrumb"),
        shiny::fluidRow(shiny::column(6, shiny::actionButton("tree_up", "Up", class = "rr-secondary")),
                        shiny::column(6, shiny::actionButton("tree_root", "Root", class = "rr-secondary"))),
        shiny::numericInput("child_offset", "First child", value = 1, min = 1, step = 1),
        shiny::uiOutput("tree_children"), shiny::uiOutput("node_meta")),
      htmltools::tags$section(class = "rr-card rr-browser", shiny::uiOutput("browser_ui"),
        shiny::uiOutput("query_status"), shiny::uiOutput("distribution_navigation"),
        shiny::conditionalPanel("output.preview_ready === 'yes'",
          shiny::uiOutput("distribution_scope"),
          rr_style_controls("preview_"),
          shiny::plotOutput("preview_plot", height = "auto")),
        htmltools::tags$details(id = "explorer_statistics", class = "rr-explorer-details",
          htmltools::tags$summary("Column statistics and inspected values"), shiny::uiOutput("summary_output")),
        htmltools::tags$details(id = "explorer_table", class = "rr-explorer-details",
          htmltools::tags$summary("Data table, pages and CSV"),
          shiny::actionButton("query_table", "Load / refresh table", class = "rr-secondary"),
          shiny::uiOutput("name_results"), shiny::uiOutput("query_navigation"), shiny::uiOutput("table_controls"),
          shiny::uiOutput("preview_table"), shiny::uiOutput("query_downloads")))))
  tabs <- list(explorer)
  if (!is.null(adapter) && view != "explore") {
    report <- shiny::tabPanel(adapter$name, value = "report",
      htmltools::tags$section(class = "rr-report", shiny::uiOutput("report_modules"),
        shiny::uiOutput("report_output"),
        shiny::conditionalPanel("!['results','interpretation'].includes(input.report_module)", rr_chart_panel(imported = identical(adapter$kind, "ggplot"))),
        htmltools::tags$div(class = "rr-report-downloads", shiny::uiOutput("report_downloads"))))
    tabs <- c(list(report), tabs)
  }
  ui <- shiny::fluidPage(header, htmltools::tags$main(class = "rr-main",
    if (is.null(adapter)) htmltools::tags$div(class = "rr-intro", htmltools::tags$div(class = "rr-eyebrow", "ANY R OBJECT / LIVE INSPECTION"),
      htmltools::tags$h1("Explore what is already in R."), htmltools::tags$p("Small windows. Real calculations. Your local object.")),
    htmltools::tags$details(class = "rr-object-details", htmltools::tags$summary(paste("Object:", paste(info$class, collapse = " / "), "| Inspect metadata")), rr_metrics(info)),
    do.call(shiny::tabsetPanel, c(tabs, list(id = "workspace_page", selected = if (!is.null(adapter) && view != "explore") "report" else "explorer")))),
    htmltools::tags$footer(class = "rr-footer", "RResult 0.1.0 | Local queries only | No automatic synchronization with reassigned R variables",
      htmltools::tags$details(htmltools::tags$summary(paste(nrow(rresult_supported()), "standard report families supported")), rr_table(rresult_supported()))))
  htmltools::attachDependencies(ui, dep, append = TRUE)
}

rr_matrix_controls <- function(x, limits) {
  dim <- rr_dims(x)
  htmltools::tagList(htmltools::tags$h2("Matrix window"),
    rr_notice(paste("Select a range, then query. Maximum", limits$max_rows, "rows,", limits$max_cols, "columns and", limits$max_cells, "cells.")),
    shiny::fluidRow(
      shiny::column(3, shiny::numericInput("row_start", "Starting row", 1, min = 1, max = max(1, dim[1L]))),
      shiny::column(3, shiny::numericInput("n_rows", "Number of rows", max(1, min(20, dim[1L], limits$max_rows)), min = 1, max = limits$max_rows)),
      shiny::column(3, shiny::numericInput("col_start", "Starting column", 1, min = 1, max = max(1, dim[2L]))),
      shiny::column(3, shiny::numericInput("n_cols", "Number of columns", max(1, min(10, dim[2L], limits$max_cols)), min = 1, max = limits$max_cols))),
    htmltools::tags$div(class = "rr-selection-actions",
      shiny::actionButton("choose_rows", "Choose rows in batches", class = "rr-secondary"),
      shiny::actionButton("choose_columns", "Choose columns in batches", class = "rr-secondary")),
    shiny::uiOutput("matrix_selection_status"),
    htmltools::tags$details(htmltools::tags$summary("Select exact row / column names"),
      rr_notice("Optional: paste one name per line. Pasted names replace the range or batch selection for that axis. Duplicate names select every occurrence; the batch picker can select them individually."),
      shiny::fluidRow(shiny::column(6, shiny::textAreaInput("row_names", "Exact row names", rows = 3)),
                      shiny::column(6, shiny::textAreaInput("col_names", "Exact column names", rows = 3))),
      shiny::fluidRow(shiny::column(4, shiny::selectInput("name_axis", "Find names in", c("rows", "columns"))),
                      shiny::column(5, shiny::textInput("name_pattern", "Literal name search")),
                      shiny::column(3, shiny::actionButton("search_names", "Find names")))),
    shiny::actionButton("query", "Query window", class = "rr-primary"),
    shiny::checkboxInput("show_heatmap", "Show heatmap of selected window", TRUE),
    shiny::fluidRow(shiny::column(6, shiny::numericInput("color_low", "Colour minimum (optional)", NA)),
                    shiny::column(6, shiny::numericInput("color_high", "Colour maximum (optional)", NA))))
}

rr_column_choices <- function(x) {
  index <- seq_along(rr_plain(x))
  nm <- names(rr_plain(x))[index]
  stats::setNames(as.character(index), paste0(index, ": ", rr_text(nm, 60L)))
}

rr_df_controls <- function(x, limits) {
  n <- rr_df_nrow(x)
  htmltools::tagList(htmltools::tags$h2("Data frame"),
    shiny::uiOutput("column_picker"),
    htmltools::tags$div(class = "rr-selection-actions",
      shiny::actionButton("choose_columns", "Choose columns in batches", class = "rr-secondary")),
    htmltools::tags$details(htmltools::tags$summary("Table search, rows and paging"),
    shiny::fluidRow(shiny::column(6, shiny::numericInput("row_start", "First search row", 1, min = 1)),
      shiny::column(6, shiny::numericInput("n_rows", "Rows in search range", max(1, min(n, limits$max_scan_rows)), min = 1, max = limits$max_scan_rows))),
    shiny::fluidRow(shiny::column(5, shiny::textInput("df_search", "Literal text search")),
      shiny::column(3, shiny::numericInput("sort_col", "Sort by column index (0 = none)", 0, min = 0)),
      shiny::column(4, shiny::checkboxInput("sort_desc", "Descending", FALSE))),
    shiny::fluidRow(shiny::column(6, shiny::numericInput("df_page", "Page", 1, min = 1)),
                    shiny::column(6, shiny::numericInput("df_page_size", "Rows per page", min(25, limits$max_rows), min = 1, max = limits$max_rows))),
    shiny::actionButton("query", "Query page", class = "rr-primary")),
    htmltools::tags$h3("Plots of selected columns"),
    shiny::uiOutput("vector_plot_picker"),
    shiny::actionButton("make_plot", "Draw selected columns", class = "rr-primary"))
}

rr_value_controls <- function(x, limits) {
  n <- length(rr_plain(x))
  htmltools::tagList(htmltools::tags$h2("Atomic values"),
    rr_notice("Query any indexed window. Arrays use column-major order. Statistics and plots are inspected separately."),
    shiny::fluidRow(
      shiny::column(6, shiny::numericInput("value_start", "Starting value", 1, min = 1, max = max(1L, n))),
      shiny::column(6, shiny::numericInput("value_count", "Number of values", max(1L, min(25L, n, limits$max_rows)), min = 1, max = limits$max_rows))),
    shiny::actionButton("query", "Query values", class = "rr-primary"),
    shiny::uiOutput("vector_plot_picker"),
    shiny::actionButton("make_plot", "Inspect distribution", class = "rr-secondary"))
}

rr_query_csv_data <- function(x, result) {
  kind <- rr_kind(x)
  if (kind == "data_frame") {
    columns <- rr_plain(x)[result$col_indices]
    values <- lapply(columns, function(column) {
      if (rr_simple(column) && is.null(attr(column, "dim", exact = TRUE))) column[result$row_indices]
      else rr_cell_text(column, result$row_indices)
    })
    names(values) <- names(columns)
    data <- as.data.frame(values, optional = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    if (!length(values)) data <- data.frame(row.names = seq_along(result$row_indices))
    rownames(data) <- rr_df_row_names(x, result$row_indices)
    return(data)
  }
  if (kind %in% c("matrix", "sparse_matrix")) {
    data <- result$data
    if (is.null(rownames(data))) rownames(data) <- as.character(result$row_indices)
    if (is.null(colnames(data))) colnames(data) <- paste0("Column ", result$col_indices)
    return(data)
  }
  value <- if (kind == "array") result$value else x[result$indices]
  data.frame(index = result$indices, value = value)
}

rr_workspace_server <- function(input, output, session, x, limits, adapter = NULL, metadata = NULL, sample_id = NULL) {
  # x is a session-local binding, pointing to the app's original object.
  path <- shiny::reactiveVal(list())
  query_result <- shiny::reactiveVal(NULL)
  query_error <- shiny::reactiveVal(NULL)
  names_result <- shiny::reactiveVal(NULL)
  plot_result <- shiny::reactiveVal(NULL)
  summary_result <- shiny::reactiveVal(NULL)
  distribution_source <- shiny::reactiveVal(NULL)
  matrix_rows <- shiny::reactiveVal(NULL)
  matrix_cols <- shiny::reactiveVal(NULL)
  df_columns <- shiny::reactiveVal(NULL)
  node <- shiny::reactive(rr_resolve(x, path(), limits))
  node_info <- shiny::reactive(rr_info(node(), limits))
  selected_columns <- shiny::reactive({
    if (node_info()$kind != "data_frame") return(NULL)
    cols <- df_columns()
    if (is.null(cols)) cols <- if (is.null(input$df_cols)) seq_len(min(10L, limits$max_cols, length(rr_plain(node())))) else as.numeric(input$df_cols)
    unique(rr_indices(cols, length(rr_plain(node())), limits$max_cols, "Column"))
  })
  reset <- function() {
    query_result(NULL); query_error(NULL); names_result(NULL); plot_result(NULL); summary_result(NULL)
    distribution_source(NULL)
    matrix_rows(NULL); matrix_cols(NULL); df_columns(NULL); bulk_picker$reset()
    shiny::updateNumericInput(session, "child_offset", value = 1)
  }
  shiny::observeEvent(input$dark_mode, session$sendCustomMessage("rresult-theme", isTRUE(input$dark_mode)))
  shiny::observeEvent(input$tree_root, { path(list()); reset() })
  shiny::observeEvent(input$tree_up, { if (length(path())) path(utils::head(path(), -1L)); reset() })
  shiny::observeEvent(input$tree_open, {
    tryCatch({
      key <- rr_integer(as.numeric(input$tree_child), "child index", upper = max(1, rr_child_count(node())))
      candidate <- c(path(), list(key))
      rr_resolve(x, candidate, limits)
      path(candidate); reset()
    }, error = function(e) query_error(conditionMessage(e)))
  })
  output$breadcrumb <- shiny::renderText(paste(c("Root", unlist(path())), collapse = " / "))
  output$tree_children <- shiny::renderUI({
    children <- tryCatch(rr_children(node(), limits, length(path()), if (is.null(input$child_offset)) 1L else input$child_offset), error = function(e) e)
    if (inherits(children, "error")) return(rr_notice(conditionMessage(children), TRUE))
    if (!length(children$nodes)) return(rr_notice(children$note))
    choices <- stats::setNames(vapply(children$nodes, function(n) as.character(n$index), character(1)),
      vapply(children$nodes, function(n) paste(n$index, n$label, "|", n$info$kind), character(1)))
    htmltools::tagList(shiny::selectInput("tree_child", "Child / slot", choices),
      shiny::actionButton("tree_open", "Open child", class = "rr-primary"), rr_notice(children$note))
  })
  output$node_meta <- shiny::renderUI({
    info <- node_info()
    htmltools::tagList(htmltools::tags$hr(), htmltools::tags$h3("Node properties"),
      htmltools::tags$p(paste(info$class, collapse = " / ")), htmltools::tags$p(info$size_note),
      htmltools::tags$details(htmltools::tags$summary("Attributes and structure"),
        htmltools::tags$pre(jsonlite::toJSON(info, auto_unbox = TRUE, pretty = TRUE, na = "string", null = "null"))))
  })
  output$browser_ui <- shiny::renderUI({
    kind <- node_info()$kind
    if (kind == "ggplot") return(htmltools::tagList(htmltools::tags$h2("Existing ggplot"),
      rr_notice("Open this object with rresult(plot) for the original chart, layer and mapping inspection, source-data previews and downloads.")))
    if (kind %in% c("matrix", "sparse_matrix")) return(rr_matrix_controls(node(), limits))
    if (kind == "data_frame") return(rr_df_controls(node(), limits))
    if (kind %in% c("vector", "array")) return(rr_value_controls(node(), limits))
    htmltools::tagList(htmltools::tags$h2("Object structure"), rr_notice("Choose a child or slot from the structure panel. Unsafe bindings and unfamiliar atomic classes remain metadata only."))
  })
  output$vector_plot_picker <- shiny::renderUI({
    value <- node()
    if (node_info()$kind == "data_frame") {
      selection <- rr_explorer_columns(value, selected_columns(), limits)
      choices <- rr_explorer_distribution_choices(selection)
      if (!length(choices)) return(rr_notice("Select numeric, categorical or time columns to draw. Other column types remain available in the data table."))
      previous <- shiny::isolate(input$plot_type)
      return(htmltools::tagList(
        shiny::selectInput("plot_type", "Distribution chart", choices,
          selected = if (length(previous) && previous %in% choices) previous else "auto"),
        if (length(selection$columns) > 1L && all(selection$types == "numeric"))
          shiny::selectInput("distribution_layout", "Box / violin layout", c("Compare columns" = "compare", "Separate plots" = "separate"),
            selected = { old <- shiny::isolate(input$distribution_layout); if (length(old) && old %in% c("compare", "separate")) old else "compare" }),
        shiny::checkboxInput("distribution_points", "Show individual observations in box / violin plots",
          !identical(shiny::isolate(input$distribution_points), FALSE)),
        if (length(selection$columns) > 6L) shiny::numericInput("distribution_page", "Chart page (6 columns per page)", 1L,
          min = 1L, max = ceiling(length(selection$columns) / 6L), step = 1L),
        if (length(selection$omitted)) rr_notice(paste("Available in the table, without a compatible distribution chart:", paste(selection$omitted, collapse = "; ")))))
    }
    if (rr_kind(value) == "array") { value <- rr_plain(value); attributes(value) <- NULL }
    choices <- rr_data_choices(value, limits)
    htmltools::tagList(shiny::selectInput("plot_type", "Compatible distribution plots", c("Automatic" = "auto", choices)),
      if (!length(choices)) rr_notice("This value has a preview but no compatible statistical chart."))
  })
  output$column_picker <- shiny::renderUI({
    shiny::req(node_info()$kind == "data_frame")
    choices <- rr_column_choices(node())
    initial <- utils::head(choices, min(10L, limits$max_cols))
    session$onFlushed(function() shiny::updateSelectizeInput(session, "df_cols", choices = choices,
      selected = unname(initial), server = TRUE), once = TRUE)
    shiny::selectizeInput("df_cols", paste0("Columns: search any name or index (up to ", limits$max_cols, ")"),
      initial, selected = unname(initial), multiple = TRUE,
      options = list(plugins = list("remove_button"), maxItems = limits$max_cols))
  })
  parse_names <- function(value) {
    if (is.null(value) || !nzchar(trimws(value))) return(NULL)
    if (nchar(value, type = "bytes") > 8192L) stop("Name query is too long.", call. = FALSE)
    value <- trimws(strsplit(value, "\n", fixed = TRUE)[[1L]])
    value[nzchar(value)]
  }
  shiny::observeEvent(input$df_cols, df_columns(as.numeric(input$df_cols)), ignoreNULL = FALSE, ignoreInit = TRUE)
  shiny::observeEvent(selected_columns(), {
    current <- query_result()
    if (!is.null(current) && node_info()$kind == "data_frame" && !identical(current$col_indices, selected_columns())) query_result(NULL)
    source <- distribution_source()
    if (!is.null(source) && !identical(source$selection, selected_columns())) {
      distribution_source(NULL); plot_result(NULL); summary_result(NULL)
    }
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$row_names, {
    if (length(input$row_names) && any(nzchar(trimws(input$row_names)))) matrix_rows(NULL)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$col_names, {
    if (length(input$col_names) && any(nzchar(trimws(input$col_names)))) matrix_cols(NULL)
  }, ignoreInit = TRUE)
  bulk_picker <- rr_bulk_picker(input, output, session, node, limits,
    selected = function(axis) {
      if (node_info()$kind == "data_frame") {
        if (!is.null(df_columns())) return(df_columns())
        if (!is.null(input$df_cols)) return(as.numeric(input$df_cols))
        return(seq_len(min(10L, limits$max_cols, length(rr_plain(node())))))
      }
      rows <- identical(axis, "rows")
      selection <- if (rows) matrix_rows() else matrix_cols()
      if (!is.null(selection)) return(selection)
      names <- parse_names(input[[if (rows) "row_names" else "col_names"]])
      cap <- if (rows) limits$max_rows else limits$max_cols
      if (!is.null(names)) return(rr_name_select(rr_dimnames(node())[[if (rows) 1L else 2L]], names, cap, limits)$index)
      total <- rr_dims(node())[if (rows) 1L else 2L]
      start <- input[[if (rows) "row_start" else "col_start"]]
      count <- input[[if (rows) "n_rows" else "n_cols"]]
      if (is.null(start)) start <- 1L
      if (is.null(count)) count <- max(1L, min(if (rows) 20L else 10L, total, cap))
      rr_window(start, count, total, cap, if (rows) "Row" else "Column")
    },
    on_apply = function(axis, indices) {
      query_result(NULL); query_error(NULL)
      if (node_info()$kind == "data_frame") {
        df_columns(indices)
        shiny::updateSelectizeInput(session, "df_cols", choices = rr_column_choices(node()),
          selected = as.character(indices), server = TRUE)
        shiny::updateNumericInput(session, "df_page", value = 1L)
      } else if (identical(axis, "rows")) {
        matrix_rows(indices)
        shiny::updateTextAreaInput(session, "row_names", value = "")
      } else {
        matrix_cols(indices)
        shiny::updateTextAreaInput(session, "col_names", value = "")
      }
    })
  shiny::observeEvent(input$choose_rows, {
    if (!node_info()$kind %in% c("matrix", "sparse_matrix")) return()
    tryCatch(bulk_picker$open("rows"), error = function(e) query_error(conditionMessage(e)))
  })
  shiny::observeEvent(input$choose_columns, {
    if (!node_info()$kind %in% c("matrix", "sparse_matrix", "data_frame")) return()
    tryCatch(bulk_picker$open("columns"), error = function(e) query_error(conditionMessage(e)))
  })
  shiny::observeEvent(input$use_row_range, {
    matrix_rows(NULL)
    query_result(NULL)
    shiny::updateTextAreaInput(session, "row_names", value = "")
  })
  shiny::observeEvent(input$use_col_range, {
    matrix_cols(NULL)
    query_result(NULL)
    shiny::updateTextAreaInput(session, "col_names", value = "")
  })
  output$matrix_selection_status <- shiny::renderUI({
    shiny::req(node_info()$kind %in% c("matrix", "sparse_matrix"))
    rows <- matrix_rows(); cols <- matrix_cols()
    htmltools::tagList(
      if (!is.null(rows)) htmltools::tags$div(class = "rr-selection-summary",
        htmltools::tags$span(paste(length(rows), "rows selected. Numeric row range is inactive.")),
        shiny::actionButton("use_row_range", "Use row range", class = "rr-secondary")),
      if (!is.null(cols)) htmltools::tags$div(class = "rr-selection-summary",
        htmltools::tags$span(paste(length(cols), "columns selected. Numeric column range is inactive.")),
        shiny::actionButton("use_col_range", "Use column range", class = "rr-secondary")))
  })
  perform_query <- function(overrides = list()) {
    query_error(NULL)
    if (node_info()$kind %in% c("matrix", "sparse_matrix")) { plot_result(NULL); summary_result(NULL) }
    value <- function(name) if (name %in% names(overrides)) overrides[[name]] else input[[name]]
    tryCatch(shiny::withProgress(message = "Querying local object", value = 0.3, {
      kind <- node_info()$kind
      result <- if (kind %in% c("matrix", "sparse_matrix")) rr_matrix(node(), limits,
        row_start = value("row_start"), n_rows = value("n_rows"), col_start = value("col_start"), n_cols = value("n_cols"),
        rows = if (is.null(parse_names(input$row_names))) matrix_rows() else NULL,
        cols = if (is.null(parse_names(input$col_names))) matrix_cols() else NULL,
        row_names = parse_names(input$row_names), col_names = parse_names(input$col_names)) else if (kind == "data_frame") rr_df(node(), limits,
        row_start = input$row_start, n_rows = input$n_rows,
        cols = selected_columns(),
        page = value("df_page"), page_size = input$df_page_size, search = input$df_search,
        sort_col = if (is.null(input$sort_col) || input$sort_col == 0) NULL else input$sort_col, decreasing = input$sort_desc) else
        if (kind %in% c("vector", "array")) rr_values(node(), limits, start = value("value_start"), count = value("value_count")) else stop("Choose a tabular or atomic node.")
      query_result(result)
      shiny::updateNumericInput(session, "table_page", value = 1)
      shiny::incProgress(0.7)
    }), error = function(e) { query_result(NULL); query_error(conditionMessage(e)) })
  }
  shiny::observeEvent(input$query, perform_query())
  shiny::observeEvent(input$query_table, perform_query())
  shiny::observeEvent(input$explorer_table_open, {
    if (isTRUE(input$explorer_table_open) && node_info()$kind == "data_frame" && is.null(query_result())) perform_query()
  })
  navigate <- function(direction, axis = "rows") {
    r <- query_result()
    if (is.null(r)) return(invisible(NULL))
    kind <- node_info()$kind
    if (kind == "data_frame") {
      page <- max(1L, min(r$pages, r$page + direction))
      shiny::updateNumericInput(session, "df_page", value = page)
      return(perform_query(list(df_page = page)))
    }
    if (kind %in% c("vector", "array")) {
      if (!length(r$indices)) return(invisible(NULL))
      count <- length(r$indices)
      start <- max(1L, min(max(1L, r$population - count + 1L), min(r$indices) + direction * count))
      shiny::updateNumericInput(session, "value_start", value = start)
      return(perform_query(list(value_start = start)))
    }
    key <- if (axis == "rows") "row" else "col"
    index <- r[[paste0(key, "_indices")]]
    names_input <- input[[paste0(key, "_names")]]
    selected <- if (axis == "rows") matrix_rows() else matrix_cols()
    if (!length(index) || !is.null(parse_names(names_input)) || !is.null(selected)) return(invisible(NULL))
    total <- r$total_dim[if (axis == "rows") 1L else 2L]
    count <- length(index)
    start <- max(1L, min(max(1L, total - count + 1L), min(index) + direction * count))
    shiny::updateNumericInput(session, paste0(key, "_start"), value = start)
    perform_query(stats::setNames(list(start), paste0(key, "_start")))
  }
  shiny::observeEvent(input$query_previous, navigate(-1L))
  shiny::observeEvent(input$query_next, navigate(1L))
  shiny::observeEvent(input$query_previous_columns, navigate(-1L, "columns"))
  shiny::observeEvent(input$query_next_columns, navigate(1L, "columns"))
  output$query_navigation <- shiny::renderUI({
    r <- query_result()
    if (is.null(r)) return(NULL)
    button <- function(id, label, enabled) shiny::actionButton(id, label, class = "rr-secondary",
      disabled = !enabled)
    kind <- node_info()$kind
    if (kind == "data_frame") return(htmltools::tagList(
      button("query_previous", "Previous page", r$page > 1L),
      button("query_next", "Next page", r$page < r$pages),
      htmltools::tags$span(paste(" Page", r$page, "of", r$pages))))
    if (kind %in% c("vector", "array")) return(htmltools::tagList(
      button("query_previous", "Previous values", length(r$indices) > 0L && min(r$indices) > 1L),
      button("query_next", "Next values", length(r$indices) > 0L && max(r$indices) < r$population)))
    rows <- r$row_indices; cols <- r$col_indices
    row_range <- is.null(parse_names(input$row_names)) && is.null(matrix_rows())
    col_range <- is.null(parse_names(input$col_names)) && is.null(matrix_cols())
    htmltools::tagList(
      button("query_previous", "Previous rows", row_range && length(rows) > 0L && min(rows) > 1L),
      button("query_next", "Next rows", row_range && length(rows) > 0L && max(rows) < r$total_dim[1L]),
      button("query_previous_columns", "Previous columns", col_range && length(cols) > 0L && min(cols) > 1L),
      button("query_next_columns", "Next columns", col_range && length(cols) > 0L && max(cols) < r$total_dim[2L]))
  })
  output$query_downloads <- shiny::renderUI({
    if (is.null(query_result())) return(NULL)
    shiny::downloadButton("download_query", "Query window CSV")
  })
  output$download_query <- shiny::downloadHandler(filename = function() "RResult-window.csv", content = function(file) {
    r <- query_result(); shiny::req(r)
    data <- rr_query_csv_data(node(), r)
    rr_write_csv(data, file, row.names = node_info()$kind %in% c("data_frame", "matrix", "sparse_matrix"), na = "", fileEncoding = "UTF-8")
  })
  shiny::observeEvent(input$search_names, {
    tryCatch({ names_result(rr_name_search(node(), input$name_axis, input$name_pattern, limits)); query_error(NULL) },
      error = function(e) { names_result(NULL); query_error(conditionMessage(e)) })
  })
  output$name_results <- shiny::renderUI({
    r <- names_result()
    if (is.null(r)) return(NULL)
    htmltools::tagList(rr_notice(paste("Name search scanned", r$scanned, "names.", if (r$truncated) "Results / scan truncated; refine search or use indices.")), rr_table(r$matches))
  })
  output$query_status <- shiny::renderUI({
    if (!is.null(query_error())) return(rr_notice(query_error(), TRUE))
    r <- query_result()
    if (is.null(r)) return(if (is.null(plot_result())) rr_notice("Ready. Choose a node or run a query to load a bounded preview.") else NULL)
    messages <- c(r$scope, if (!is.null(r$matched_rows)) paste(r$matched_rows, "matching rows;", r$pages, "pages."),
      if (length(r$missing_rows)) paste("Missing row names:", paste(rr_text(r$missing_rows), collapse = ", ")),
      if (length(r$missing_cols)) paste("Missing column names:", paste(rr_text(r$missing_cols), collapse = ", ")),
      if (length(r$duplicate_rows)) "Duplicate row names: every matched occurrence selected.",
      if (length(r$duplicate_cols)) "Duplicate column names: every matched occurrence selected.")
    rr_notice(paste(messages, collapse = " "))
  })
  output$table_controls <- shiny::renderUI({
    r <- query_result()
    if (is.null(r) || !node_info()$kind %in% c("matrix", "sparse_matrix")) return(NULL)
    labels <- colnames(r$data)
    if (is.null(labels)) labels <- as.character(r$col_indices)
    choices <- stats::setNames(as.character(seq_along(labels)), paste(seq_along(labels), rr_text(labels)))
    htmltools::tagList(shiny::fluidRow(shiny::column(3, shiny::numericInput("table_page", "Window page", 1, min = 1, max = max(1L, ceiling(nrow(r$data) / 25L)))),
      shiny::column(4, shiny::textInput("table_search", "Search selected window")),
      shiny::column(3, shiny::selectInput("table_sort", "Sort within window", c("None" = "0", choices))),
      shiny::column(2, shiny::checkboxInput("table_desc", "Descending", FALSE))),
      shiny::actionButton("table_previous", "Previous window page", class = "rr-secondary"),
      shiny::actionButton("table_next", "Next window page", class = "rr-secondary"))
  })
  shiny::observeEvent(input$table_previous, {
    page <- if (is.null(input$table_page)) 1L else input$table_page
    shiny::updateNumericInput(session, "table_page", value = max(1L, page - 1L))
  })
  shiny::observeEvent(input$table_next, {
    r <- query_result(); shiny::req(r$data)
    page <- if (is.null(input$table_page)) 1L else input$table_page
    keep <- rep(TRUE, nrow(r$data))
    search <- input$table_search
    if (!is.null(search) && nzchar(search)) {
      keep[] <- FALSE
      for (j in seq_len(ncol(r$data))) keep <- keep | grepl(search, rr_cell_text(r$data[, j], seq_len(nrow(r$data))), fixed = TRUE)
    }
    shiny::updateNumericInput(session, "table_page", value = min(max(1L, ceiling(sum(keep) / 25L)), page + 1L))
  })
  shiny::observeEvent(input$table_search, shiny::updateNumericInput(session, "table_page", value = 1L), ignoreInit = TRUE)
  output$preview_table <- shiny::renderUI({
    r <- query_result()
    if (is.null(r)) return(NULL)
    if (node_info()$kind %in% c("vector", "array")) return(rr_table(data.frame(index = r$indices,
      value = if (node_info()$kind == "array") rr_cell_text(r$value, seq_along(r$value)) else rr_cell_text(node(), r$indices))))
    if (node_info()$kind == "data_frame") {
      rows <- as.character(r$row_indices)
      labels <- r$row_labels
      if (!is.null(labels)) rows <- ifelse(labels == rows, rows, paste0(rows, ": ", labels))
      return(htmltools::tagList(
      rr_table(r$data, r$column_labels, rows),
      htmltools::tags$details(htmltools::tags$summary("Column types and sampled missingness"),
        htmltools::tags$pre(jsonlite::toJSON(r$column_profile, auto_unbox = TRUE, pretty = TRUE, na = "string")))))
    }
    m <- r$data
    labels <- colnames(m)
    if (is.null(labels)) labels <- paste0("Column ", r$col_indices)
    values <- lapply(seq_len(ncol(m)), function(j) rr_cell_text(m[, j], seq_len(nrow(m))))
    names(values) <- if (length(values)) paste0("c", seq_along(values)) else character()
    dat <- as.data.frame(values, stringsAsFactors = FALSE)
    index <- seq_len(nrow(m))
    search <- input$table_search
    if (!is.null(search) && nzchar(search)) {
      shiny::validate(shiny::need(nchar(search, type = "bytes") <= 256L, "Search is limited to 256 bytes."))
      keep <- rep(FALSE, nrow(m))
      for (column in dat) keep <- keep | grepl(search, column, fixed = TRUE)
      index <- index[keep]
    }
    sort <- suppressWarnings(as.numeric(input$table_sort))
    if (length(sort) == 1L && !is.na(sort) && sort > 0) {
      sort <- rr_integer(sort, "window sort column", upper = max(1, ncol(m)))
      column <- m[index, sort]
      shiny::validate(shiny::need(!is.complex(column) && typeof(column) != "raw", "This matrix type cannot be sorted."))
      index <- index[order(column, decreasing = isTRUE(input$table_desc), na.last = TRUE)]
    }
    matched <- length(index)
    page <- if (is.null(input$table_page)) 1L else rr_integer(input$table_page, "window page", upper = 1000000)
    first <- (as.double(page) - 1) * 25 + 1
    index <- if (first > length(index)) integer() else index[seq.int(first, min(length(index), first + 24L))]
    rn <- rownames(m)
    rows <- if (is.null(rn)) as.character(r$row_indices[index]) else paste0(r$row_indices[index], ": ", rn[index])
    htmltools::tagList(rr_notice(paste("Window page", page, "of", max(1L, ceiling(matched / 25L)))),
      rr_table(dat[index, , drop = FALSE], labels, rows))
  })
  draw_distributions <- function(page = NULL, focus = FALSE) {
    query_error(NULL)
    tryCatch({
      selection <- selected_columns()
      supported <- rr_explorer_columns(node(), selection, limits)
      pages <- max(1L, ceiling(length(supported$columns) / 6L))
      if (is.null(page)) page <- if (is.null(input$distribution_page)) 1L else min(as.numeric(input$distribution_page), pages)
      result <- rr_explorer_distributions(node(), selection, limits,
        type = if (is.null(input$plot_type)) "auto" else input$plot_type,
        layout = if (is.null(input$distribution_layout)) "compare" else input$distribution_layout,
        page = page, show_points = !identical(input$distribution_points, FALSE))
      plot_result(result)
      distribution_source(list(selection = selection))
      summary_result(list(kind = "columns", columns = result$columns, population = result$total_rows))
      if (isTRUE(focus)) session$onFlushed(function() session$sendCustomMessage("rresult-preview-focus", TRUE), once = TRUE)
      if (!is.null(input$distribution_page) && !identical(as.integer(input$distribution_page), result$page))
        shiny::updateNumericInput(session, "distribution_page", value = result$page)
    }, error = function(e) { plot_result(NULL); summary_result(NULL); query_error(conditionMessage(e)) })
  }
  shiny::observeEvent(input$distribution_page, {
    r <- plot_result()
    if (node_info()$kind == "data_frame" && inherits(r, "rr_explorer_distributions") &&
        !identical(as.numeric(input$distribution_page), as.numeric(r$page))) draw_distributions(as.numeric(input$distribution_page))
  })
  shiny::observeEvent(input$distribution_previous, {
    r <- plot_result()
    if (inherits(r, "rr_explorer_distributions") && r$page > 1L) draw_distributions(r$page - 1L)
  })
  shiny::observeEvent(input$distribution_next, {
    r <- plot_result()
    if (inherits(r, "rr_explorer_distributions") && r$page < r$pages) draw_distributions(r$page + 1L)
  })
  output$distribution_navigation <- shiny::renderUI({
    r <- plot_result()
    if (!inherits(r, "rr_explorer_distributions") || r$pages <= 1L) return(NULL)
    htmltools::tags$div(class = "rr-selection-actions",
      shiny::actionButton("distribution_previous", "Previous plots", disabled = r$page == 1L),
      htmltools::tags$span(paste("Chart page", r$page, "of", r$pages, "|", r$total_columns, "selected columns")),
      shiny::actionButton("distribution_next", "Next plots", disabled = r$page == r$pages))
  })
  output$distribution_scope <- shiny::renderUI({
    r <- plot_result()
    if (!inherits(r, "rr_explorer_distributions")) return(NULL)
    rr_notice(paste("Shown:", paste(r$labels, collapse = "; "), "|", r$total_rows,
      "source rows per column. Table search and paging do not filter these plots.",
      if (length(r$omitted)) paste("Without a compatible chart:", paste(r$omitted, collapse = "; "))))
  })
  output$preview_ready <- shiny::renderText({
    if (!is.null(plot_result()) || (node_info()$kind %in% c("matrix", "sparse_matrix") &&
        isTRUE(input$show_heatmap) && !is.null(query_result()))) "yes" else "no"
  })
  shiny::outputOptions(output, "preview_ready", suspendWhenHidden = FALSE)
  shiny::observeEvent(input$make_plot, {
    query_error(NULL)
    if (node_info()$kind == "data_frame") return(draw_distributions(focus = TRUE))
    tryCatch({
      value <- node()
      result <- rr_summary(value, limits)
      sample <- rr_extract(value, limits)
      result$preview <- data.frame(index = sample$indices, value =
        if (!is.null(attr(value, "dim", exact = TRUE))) rr_cell_text(sample$value, seq_along(sample$value)) else rr_cell_text(value, sample$indices))
      summary_result(result)
      if (typeof(value) %in% c("complex", "raw")) {
        plot_result(NULL)
      } else plot_result(rr_plot(value, if (is.null(input$plot_type)) "auto" else input$plot_type, limits))
    }, error = function(e) { plot_result(NULL); query_error(conditionMessage(e)) })
  })
  output$summary_output <- shiny::renderUI({
    r <- summary_result()
    if (is.null(r)) return(NULL)
    if (identical(r$kind, "columns")) return(htmltools::tagList(
      rr_notice(paste("Complete-column statistics for the variables on this chart page;", r$population,
        "source rows per column. Numeric statistics exclude missing and non-finite values.")),
      rr_formatted_table(rr_explorer_statistics(node(), r$columns))))
    rr_summary_ui(r)
  })
  output$preview_plot <- shiny::renderPlot({
    if (isTRUE(input$show_heatmap) && node_info()$kind %in% c("matrix", "sparse_matrix")) {
      r <- query_result()
      shiny::req(r)
      range <- c(input$color_low, input$color_high)
      if (!length(range) || all(is.na(range))) range <- NULL
      p <- tryCatch(rr_plot(node(), "heatmap", limits, rows = r$row_indices, cols = r$col_indices, color_range = range), error = function(e) e)
      shiny::validate(shiny::need(!inherits(p, "error"), if (inherits(p, "error")) conditionMessage(p) else ""))
      return(rr_apply_style(p, rr_style_input(input, "preview_")))
    }
    shiny::req(plot_result())
    if (inherits(plot_result(), "rr_explorer_distributions"))
      return(rr_draw_distributions(plot_result(), rr_style_input(input, "preview_")))
    rr_apply_style(plot_result(), rr_style_input(input, "preview_"))
  }, height = function() {
    r <- plot_result()
    if (inherits(r, "rr_explorer_distributions")) r$height else 430L
  })
  shiny::outputOptions(output, "preview_plot", suspendWhenHidden = FALSE)
  output$report_modules <- shiny::renderUI({
    shiny::req(adapter)
    if (!is.null(adapter$kind)) {
      modules <- c("Overview" = "overview", "Chart studio" = "charts", "Result tables" = "results", "Read the result" = "interpretation")
      if (adapter$kind == "lm") modules <- c(modules, "Coefficients" = "coefficients", "Diagnostics" = "diagnostics")
    } else modules <- names(adapter$modules)
    htmltools::tags$div(class = "rr-module-nav", shiny::radioButtons("report_module", NULL, modules, inline = TRUE))
  })
  model_report <- shiny::reactive({ shiny::req(identical(attr(x, "class", exact = TRUE), "lm")); rr_lm(x, limits) })
  bio_container <- !is.null(adapter$kind) && adapter$kind %in% .rr_bio_containers
  bio_cache <- if (bio_container) rr_bio_source_cache(x, limits) else NULL
  bio_source <- shiny::reactive({
    shiny::req(bio_container)
    options <- rr_bio_options_input(input, adapter$kind, x)
    tryCatch(bio_cache$get(options$assay, options$layer), error = function(e) e)
  })
  bio_report <- function(tables = character()) {
    source <- bio_source()
    if (inherits(source, "error")) return(source)
    tryCatch(rr_bio_container_report(x, adapter$kind, limits, source = source, tables = tables), error = function(e) e)
  }
  built_report <- shiny::reactive({
    shiny::req(adapter$kind)
    if (bio_container) return(bio_report())
    options <- if (adapter$kind %in% .rr_bio_families) rr_bio_options_input(input, adapter$kind, x) else list()
    tryCatch(do.call(rr_report, c(list(x = x, limits = limits, metadata = metadata, sample_id = sample_id), options)), error = function(e) e)
  })
  table_report <- shiny::reactive({
    r <- built_report()
    if (!bio_container || inherits(r, "error")) return(r)
    name <- input$report_table
    if (is.null(name) || !name %in% r$table_choices) name <- r$table_choices[1L]
    bio_report(name)
  })
  full_report <- function() if (bio_container) bio_report(NULL) else built_report()
  plugin_result <- shiny::reactive({
    shiny::req(adapter, is.null(adapter$kind), input$report_module %in% names(adapter$modules))
    tryCatch(rr_budget(adapter$modules[[input$report_module]](x, limits), limits), error = function(e) e)
  })
  output$report_output <- shiny::renderUI({
    shiny::req(adapter, input$report_module)
    module <- input$report_module
    if (!is.null(adapter$kind)) {
      if (module == "charts") return(NULL)
      if (module == "diagnostics") return(rr_notice("Inspect model assumptions and influence. The chart selector below changes the diagnostic view."))
      r <- if (bio_container && module == "results") table_report() else built_report()
      return(rr_result_ui(r, module, if (module == "coefficients") "Coefficients" else input$report_table, limits,
        page = if (is.null(input$report_table_page)) 1L else input$report_table_page,
        page_size = if (is.null(input$report_table_size)) min(25L, limits$max_rows) else input$report_table_size,
        column_start = if (is.null(input$report_table_col)) 1L else input$report_table_col))
    }
    value <- plugin_result()
    if (inherits(value, "error")) return(rr_notice(conditionMessage(value), TRUE))
    if (inherits(value, "ggplot")) return(rr_notice("Plot shown below."))
    if (is.data.frame(value)) return(rr_formatted_table(value))
    if (is.character(value)) return(htmltools::tags$ul(lapply(value, htmltools::tags$li)))
    htmltools::tags$pre(jsonlite::toJSON(value, auto_unbox = TRUE, pretty = TRUE, na = "string", null = "null"))
  })
  output$report_chart_controls <- shiny::renderUI({
    shiny::req(adapter$kind)
    r <- built_report()
    if (inherits(r, "error")) return(NULL)
    axis_controls <- NULL
    if (adapter$kind %in% .rr_ordination_families) axis_controls <- rr_ordination_controls(x, metadata, sample_id, limits, input = input)
    if (adapter$kind %in% .rr_bio_families) axis_controls <- rr_bio_controls(x, adapter$kind, limits, input,
      source = if (bio_container) bio_source() else NULL)
    if (adapter$kind == "data_frame") {
      axis_controls <- rr_data_axes_controls(x, limits)
    }
    htmltools::tagList(htmltools::tags$div(class = "rr-eyebrow", "VISUAL EVIDENCE"),
      if (adapter$kind == "data_frame") htmltools::tagList(axis_controls, shiny::uiOutput("data_chart_picker")) else
        htmltools::tagList(shiny::conditionalPanel("!['coefficients','diagnostics'].includes(input.report_module)",
          shiny::selectInput("report_plot_type", "Chart", r$plots, selected = { previous <- shiny::isolate(input$report_plot_type); if (length(previous) && previous %in% r$plots) previous else unname(r$plots[1L]) })), axis_controls),
      shiny::conditionalPanel("['histogram','freqpoly','bin2d'].includes(input.report_plot_type)", shiny::sliderInput("report_bins", "Bins", 2, 80, 30)),
      shiny::conditionalPanel("['boxplot','violin','smooth'].includes(input.report_plot_type)", shiny::checkboxInput("report_points", "Overlay individual observations", TRUE)),
      shiny::conditionalPanel("input.report_module !== 'diagnostics' && ['survival','cumulative_event','coefficients','rate_ratios','hazard_ratios','contrasts','marginal_means','smooth','smooths'].includes(input.report_plot_type)", shiny::checkboxInput("report_ci", "Show available uncertainty intervals", TRUE)),
      if (adapter$kind == "gam" && length(rr_gam_smooth_choices(x))) shiny::conditionalPanel("input.report_plot_type === 'smooths'",
        shiny::selectInput("gam_smooth_term", "Smooth term", rr_gam_smooth_choices(x))),
      if (adapter$kind == "DESeqResults") htmltools::tagList(
        shiny::numericInput("de_padj_cutoff", "Display adjusted-P cutoff", if (length(S4Vectors::metadata(x)$alpha)) S4Vectors::metadata(x)$alpha else .05, min = .000001, max = 1, step = .01),
        shiny::numericInput("de_lfc_cutoff", "Display absolute log2-fold-change cutoff", 0, min = 0, step = .25)),
      if (adapter$kind == "survfit") shiny::conditionalPanel("input.report_plot_type !== 'risk_table'", shiny::checkboxInput("report_censor", "Show censor marks", TRUE)),
      shiny::conditionalPanel("input.report_plot_type === 'summary_bar'", shiny::selectInput("report_aggregation", "Bar statistic", c("Mean" = "mean", "Median" = "median", "Sum" = "sum"))),
      if (adapter$kind == "hclust") shiny::conditionalPanel("input.report_plot_type === 'cut_clusters'",
        shiny::numericInput("cluster_k", "Number of clusters", min(3L, length(x$order), limits$max_rows), min = 1, max = min(length(x$order), limits$max_rows, limits$max_plot_points), step = 1)),
      if (adapter$kind %in% c("lm", "glm", "negbin", "gam", "summary.lm", "summary.glm", "coxph", "survreg", "merMod", "lme")) shiny::conditionalPanel("input.report_module !== 'diagnostics' && input.report_plot_type === 'coefficients'",
        shiny::checkboxInput("include_intercept", "Include intercept", adapter$kind %in% c("lm", "glm") && length(x$coefficients) == 1L),
        if ((adapter$kind == "negbin" && x$family$link == "log") || (adapter$kind == "glm" && ((x$family$family == "binomial" && x$family$link == "logit") ||
          (x$family$family == "poisson" && x$family$link == "log"))))
          shiny::radioButtons("effect_scale", "Coefficient scale", c("Link scale" = "link", "Ratio scale" = "ratio"), inline = TRUE)),
      if (adapter$kind == "lm") shiny::conditionalPanel("input.report_module === 'diagnostics'",
        shiny::selectInput("diagnostic_type", "Diagnostic plot", c("residuals", "qq", "scale_location", "cooks"))),
      htmltools::tags$p(class = "rr-chart-hint", if (adapter$kind == "ggplot") "Keep original styling, or use Style settings to make optional changes." else "Use Style settings above the chart for colours, fonts, axes and layout."), rr_export_controls(imported = identical(adapter$kind, "ggplot")))
  })
  # These controls create the inputs needed to render the first chart.
  # Initialize them even while their conditional panel is initially hidden.
  if (!is.null(adapter$kind)) shiny::outputOptions(output, "report_chart_controls", suspendWhenHidden = FALSE)
  shiny::observeEvent(input$report_module, {
    if (identical(adapter$kind, "lm") && identical(input$report_module, "coefficients"))
      shiny::updateSelectInput(session, "report_plot_type", selected = "coefficients")
  })
  shiny::observeEvent(input$report_axis_x, {
    if (adapter$kind %in% .rr_ordination_families) {
      o <- rr_ordination(x)
      axes <- seq_len(o$axes); axes <- axes[axes != as.integer(input$report_axis_x)]
      if (length(axes)) {
        selected <- shiny::isolate(input$report_axis_y)
        if (is.null(selected) || !as.numeric(selected) %in% axes) selected <- axes[1L]
        shiny::updateSelectInput(session, "report_axis_y", choices = stats::setNames(axes, paste("Axis", axes)), selected = selected)
      }
      return()
    }
    if (!identical(adapter$kind, "data_frame")) return()
    types <- rr_plot_columns(x, limits)
    cols <- as.integer(names(types)); cols <- cols[cols != as.integer(input$report_axis_x)]
    labels <- stats::setNames(cols, paste0(cols, ": ", rr_text(names(rr_plain(x))[cols], 38L), " [", types[as.character(cols)], "]"))
    selected <- shiny::isolate(input$report_axis_y)
    if (is.null(selected) || !as.numeric(selected) %in% c(0, cols)) selected <- 0
    shiny::updateSelectInput(session, "report_axis_y", choices = c("None: distribution" = 0, labels), selected = selected)
  }, ignoreInit = TRUE)
  data_chart_choices <- shiny::reactive({
    shiny::req(identical(adapter$kind, "data_frame"))
    options <- rr_data_options_input(input, x, limits)
    c(do.call(rr_data_choices, c(list(x = x, limits = limits, axis_x = input$report_axis_x, axis_y = input$report_axis_y), options)),
      if (length(rr_numeric_columns(x, limits)) >= 2L) c("Numeric correlations" = "correlation"), "Missingness by variable" = "missingness")
  })
  output$data_chart_picker <- shiny::renderUI({
    choices <- data_chart_choices()
    previous <- shiny::isolate(input$report_plot_type)
    selected <- if (length(previous) && previous %in% choices) previous else unname(choices[1L])
    htmltools::tagList(shiny::selectInput("report_plot_type", "Compatible charts", choices, selected = selected),
      htmltools::tags$p(class = "rr-chart-hint", "Chart choices follow the selected variables. Grouping and variable roles lets you use numeric codes as categories for this chart."))
  })
  report_base_chart <- shiny::reactive({
    shiny::req(adapter)
    if (bio_container) shiny::req(input$report_plot_type)
    if (is.null(adapter$kind)) return(plugin_result())
    r <- built_report()
    if (inherits(r, "error")) return(r)
    if (!length(r$plots)) return(NULL)
    type <- if (is.null(input$report_plot_type)) unname(r$plots[1L]) else input$report_plot_type
    if (adapter$kind %in% .rr_bio_families && !type %in% r$plots) type <- unname(r$plots[1L])
    if (identical(adapter$kind, "data_frame") && !type %in% data_chart_choices()) type <- unname(data_chart_choices()[1L])
    if (identical(input$report_module, "coefficients")) type <- "coefficients"
    if (identical(input$report_module, "diagnostics")) type <- if (is.null(input$diagnostic_type)) "residuals" else input$diagnostic_type
    options <- if (adapter$kind %in% .rr_bio_families) rr_bio_plot_options(input, adapter$kind, if (bio_container) NULL else x) else if (identical(adapter$kind, "data_frame")) rr_data_options_input(input, x, limits) else
      if (adapter$kind %in% .rr_ordination_families) list(metadata = metadata, sample_id = sample_id,
        colour_by = input$ord_colour, shape_by = input$ord_shape, facet_by = input$ord_facet,
        sample_labels = isTRUE(input$ord_labels), group_hulls = isTRUE(input$ord_hulls),
        max_labels = if (is.null(input$ord_max_labels)) 20L else input$ord_max_labels,
        top_variables = if (is.null(input$ord_top_variables)) 12L else input$ord_top_variables) else list()
    if (bio_container) options$bio_source <- bio_source()
    tryCatch(do.call(rr_report_plot, c(list(x = x, type = type, limits = limits,
      axis_x = input$report_axis_x, axis_y = input$report_axis_y,
      include_intercept = isTRUE(input$include_intercept) || (adapter$kind %in% c("lm", "glm") && length(x$coefficients) == 1L && is.null(input$include_intercept)),
      effect_scale = if (is.null(input$effect_scale)) "link" else input$effect_scale,
      aggregation = if (is.null(input$report_aggregation)) "mean" else input$report_aggregation,
      bins = if (is.null(input$report_bins)) 30L else input$report_bins,
      show_points = is.null(input$report_points) || isTRUE(input$report_points), clusters = input$cluster_k,
      show_ci = is.null(input$report_ci) || isTRUE(input$report_ci), show_censor = is.null(input$report_censor) || isTRUE(input$report_censor),
      facet_scales = if (is.null(input$report_facet_scales)) "fixed" else input$report_facet_scales,
      smooth_term = input$gam_smooth_term, padj_cutoff = input$de_padj_cutoff,
      lfc_cutoff = if (is.null(input$de_lfc_cutoff)) 0 else input$de_lfc_cutoff), options)), error = function(e) e)
  })
  report_chart <- shiny::reactive({
    p <- report_base_chart()
    if (inherits(p, "error") || !inherits(p, "ggplot")) return(p)
    if (identical(adapter$kind, "ggplot")) return(tryCatch(rr_edit_ggplot(p, rr_ggplot_style_input(input)), error = function(e) e))
    tryCatch(rr_apply_style(p, rr_style_input(input, "report_")), error = function(e) e)
  })
  output$report_chart_empty <- shiny::renderUI({
    shiny::req(adapter$kind)
    r <- built_report()
    if (!inherits(r, "error") && !length(r$plots)) rr_notice("This saved object has no available numeric chart. Its original values are available in Result tables.") else NULL
  })
  if (!is.null(adapter$kind)) shiny::outputOptions(output, "report_chart_empty", suspendWhenHidden = FALSE)
  # Empty outputs may initially be reported as hidden by the browser. Let the
  # report initialize, but only draw once its controls exist and its chart is in
  # use. Keep report_chart() callable separately for downloads and inspection.
  report_plot_ready <- function() {
    if (is.null(adapter)) return(FALSE)
    if (!is.null(input$workspace_page) && input$workspace_page != "report") return(FALSE)
    if (is.null(adapter$kind)) return(TRUE)
    !is.null(input$report_module) && !is.null(input$report_plot_type) &&
      !input$report_module %in% c("results", "interpretation")
  }
  output$report_plot <- shiny::renderPlot({
    shiny::req(report_plot_ready())
    p <- report_chart()
    shiny::validate(shiny::need(!inherits(p, "error"), if (inherits(p, "error")) conditionMessage(p) else ""))
    shiny::req(inherits(p, "ggplot"))
    rr_canvas_plot(p, session$clientData$output_report_plot_width)
  }, height = function() {
    if (!report_plot_ready()) return(600)
    if (isTRUE(input$chart_expanded)) return(max(340, min(1100, if (is.null(input$rr_viewport_height)) 800 else input$rr_viewport_height - 170)))
    rr_canvas_height(report_chart(), session$clientData$output_report_plot_width, if (is.null(input$report_height)) 600 else input$report_height, auto = if (is.null(input$report_auto_height)) !identical(adapter$kind, "ggplot") else input$report_auto_height)
  }, res = 110)
  if (!is.null(adapter$kind)) shiny::outputOptions(output, "report_plot", suspendWhenHidden = FALSE)
  output$report_downloads <- shiny::renderUI({
    shiny::req(adapter$kind)
    htmltools::tagList(shiny::downloadButton("download_results", "Table CSV"), shiny::downloadButton("download_plot", "Chart PNG"), shiny::downloadButton("download_plot_pdf", "Vector PDF"),
      shiny::downloadButton("download_report", "Save report HTML"),
      htmltools::tags$span("Save the displayed results for your next step."))
  })
  output$download_results <- shiny::downloadHandler(filename = function() "RResult-results.csv", content = function(file) {
    r <- table_report(); shiny::req(!inherits(r, "error"))
    name <- input$report_table
    if (is.null(name) || !name %in% names(r$tables)) name <- names(r$tables)[1L]
    if (identical(input$report_module, "coefficients")) name <- "Coefficients"
    rr_report_csv(r, name, file)
  })
  shiny::observeEvent(list(input$report_table, input$report_module), {
    shiny::updateNumericInput(session, "report_table_page", value = 1)
    shiny::updateNumericInput(session, "report_table_col", value = 1)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$report_table_prev, {
    current <- if (is.null(input$report_table_page)) 1L else input$report_table_page
    shiny::updateNumericInput(session, "report_table_page", value = max(1L, current - 1L))
  })
  shiny::observeEvent(input$report_table_next, {
    r <- table_report()
    shiny::req(!inherits(r, "error"))
    name <- if (identical(input$report_module, "coefficients")) "Coefficients" else input$report_table
    if (is.null(name) || !name %in% names(r$tables)) name <- names(r$tables)[1L]
    size <- if (is.null(input$report_table_size)) min(25L, limits$max_rows) else input$report_table_size
    current <- if (is.null(input$report_table_page)) 1L else input$report_table_page
    first_col <- if (is.null(input$report_table_col)) 1L else input$report_table_col
    window <- tryCatch(rr_report_page(r$tables[[name]], limits, current, size, first_col), error = function(e) NULL)
    shiny::req(!is.null(window))
    shiny::updateNumericInput(session, "report_table_page", value = min(window$pages, window$page + 1L))
  })
  output$download_plot <- shiny::downloadHandler(filename = function() "RResult-chart.png", content = function(file) {
    p <- report_chart(); shiny::req(inherits(p, "ggplot"))
    size <- rr_export_dimensions(input)
    p <- rr_canvas_plot(p, size$width * size$dpi, dpi = size$dpi)
    do.call(ggplot2::ggsave, c(list(filename = file, plot = p, device = "png", bg = NULL), size))
  })
  output$download_plot_pdf <- shiny::downloadHandler(filename = function() "RResult-chart.pdf", content = function(file) {
    p <- report_chart(); shiny::req(inherits(p, "ggplot"))
    size <- rr_export_dimensions(input)
    ggplot2::ggsave(file, rr_canvas_plot(p, size$width * 110), device = "pdf", width = size$width, height = size$height, bg = NULL)
  })
  output$download_report <- shiny::downloadHandler(filename = function() "RResult-report.html", content = function(file) {
    r <- full_report(); shiny::req(!inherits(r, "error"))
    p <- report_chart()
    size <- rr_export_dimensions(input)
    writeLines(rr_report_html(r, if (inherits(p, "ggplot")) p else NULL, width = size$width, height = size$height, dpi = size$dpi), file, useBytes = TRUE)
  })
  session$onSessionEnded(function() {
    if (!is.null(bio_cache)) bio_cache$clear()
    x <<- NULL
    metadata <<- NULL
    query_result(NULL); names_result(NULL); plot_result(NULL); summary_result(NULL)
    distribution_source(NULL)
    matrix_rows(NULL); matrix_cols(NULL); df_columns(NULL); bulk_picker$reset()
  })
  invisible(list(node = node, node_info = node_info, path = path, query_result = query_result, query_error = query_error,
                 names_result = names_result, summary_result = summary_result, model_report = model_report,
                 built_report = built_report, table_report = table_report, full_report = full_report,
                 bio_source = bio_source, report_chart = report_chart, bulk_picker = bulk_picker,
                 matrix_rows = matrix_rows, matrix_cols = matrix_cols, df_columns = df_columns,
                 selected_columns = selected_columns, plot_result = plot_result))
}

rr_app <- function(x, view, limits, stop_on_close, metadata = NULL, sample_id = NULL) {
  holder <- new.env(parent = emptyenv())
  holder$x <- x
  holder$metadata <- metadata
  holder$sessions <- 0L
  adapter <- if (view == "explore") NULL else rr_adapter(x)
  if (view == "report" && is.null(adapter)) warning("No trusted adapter; using the generic explorer.", call. = FALSE)
  ui <- rr_workspace_ui(rr_info(x, limits), adapter, view)
  server <- function(input, output, session) {
    holder$sessions <- holder$sessions + 1L
    session$onSessionEnded(function() {
      holder$sessions <- max(0L, holder$sessions - 1L)
      if (stop_on_close && holder$sessions == 0L) shiny::stopApp(invisible(NULL))
    })
    shiny::observeEvent(input$close_workspace, {
      if (stop_on_close && holder$sessions <= 1L) shiny::stopApp(invisible(NULL)) else session$close()
    })
    rr_workspace_server(input, output, session, holder$x, limits, adapter, holder$metadata, sample_id)
  }
  app <- shiny::shinyApp(ui, server, onStart = function() {
    shiny::onStop(function() { holder$x <- NULL; holder$metadata <- NULL })
  }, options = list(host = "127.0.0.1"))
  # Remove the factory's formal binding: the holder owns the app reference.
  rm(x, metadata, envir = environment())
  app
}




