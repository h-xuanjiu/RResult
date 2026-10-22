# Existing ggplots: preserve graphics, inspect bounded metadata ---------------

rr_is_ggplot <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  identical(cls, c("gg", "ggplot")) ||
    identical(cls, c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"))
}

rr_ggplot_text <- function(x, limit = 500L) {
  if (is.null(x)) return("")
  if (is.character(x)) return(rr_text(paste(utils::head(x, 5L), collapse = " "), limit))
  if (is.language(x) || is.expression(x)) return(rr_text(paste(deparse(x, width.cutoff = 80L, nlines = 5L), collapse = " "), limit))
  paste0("<", typeof(x), ">")
}

rr_ggplot_mapping <- function(mapping, source, cap) {
  nm <- utils::head(names(mapping), cap)
  if (!length(nm)) return(data.frame(source = character(), aesthetic = character(), expression = character()))
  data.frame(source = source, aesthetic = nm,
    expression = vapply(nm, function(key) rr_ggplot_text(mapping[[key]]), character(1)), stringsAsFactors = FALSE)
}

rr_ggplot_data_preview <- function(data, limits) {
  if (!identical(rr_kind(data), "data_frame")) return(NULL)
  nc <- min(length(rr_plain(data)), limits$max_cols, limits$max_cells)
  if (!nc) return(data.frame())
  nr <- min(limits$max_rows, floor(limits$max_cells / nc))
  page <- rr_df(data, limits, cols = seq_len(nc), page_size = nr)
  result <- page$data
  names(result) <- make.unique(rr_text(page$column_labels))
  attr(result, "rresult_scope") <- paste("Source-data preview:", nrow(result), "of", rr_df_nrow(data),
    "rows and", ncol(result), "of", length(rr_plain(data)), "columns. CSV exports this preview; the original chart still uses its full data.")
  result
}

rr_ggplot_report <- function(x, limits) {
  n <- length(x$layers)
  count <- n
  index <- seq_len(count)
  base_df <- identical(rr_kind(x$data), "data_frame")
  layer_class <- function(layer, component) rr_text(class(layer[[component]])[1L])
  rows <- lapply(index, function(i) {
    layer <- x$layers[[i]]
    data <- layer$data
    inherited <- inherits(data, "waiver") || is.null(data)
    data.frame(layer = i, geometry = layer_class(layer, "geom"), statistic = layer_class(layer, "stat"),
      position = layer_class(layer, "position"), inherit_aes = isTRUE(layer$inherit.aes),
      data_source = if (inherited) "Global plot data" else if (is.function(data)) "Function evaluated by ggplot at render time" else "Layer data",
      source_rows = if (inherited && base_df) rr_df_nrow(x$data) else if (identical(rr_kind(data), "data_frame")) rr_df_nrow(data) else NA_integer_,
      stringsAsFactors = FALSE)
  })
  layers <- if (length(rows)) do.call(rbind, rows) else data.frame(layer = integer(), geometry = character(), statistic = character())
  mappings <- rr_ggplot_mapping(x$mapping, "Global", length(x$mapping))
  for (i in index) {
    mappings <- rbind(mappings, rr_ggplot_mapping(x$layers[[i]]$mapping, paste("Layer", i), length(x$layers[[i]]$mapping)))
  }
  tables <- list(Layers = layers, Mappings = mappings)
  data <- rr_ggplot_data_preview(x$data, limits)
  if (!is.null(data)) tables[["Data: global"]] <- data
  for (i in index) {
    data <- rr_ggplot_data_preview(x$layers[[i]]$data, limits)
    if (!is.null(data)) tables[[paste0("Data: layer ", i)]] <- data
  }
  facet <- class(x$facet)[1L]
  coord <- class(x$coordinates)[1L]
  title <- rr_ggplot_text(x$labels$title)
  rr_new_report("ggplot", if (nzchar(title)) title else "Your existing ggplot", "Original graphics with layer, mapping and data inspection",
    list(Layers = n, "Global data rows" = if (base_df) rr_df_nrow(x$data) else NA_integer_, Facets = facet, Coordinates = coord),
    c("The original layers, scales, coordinates, facets and theme are preserved by default.",
      paste(count, "of", n, "layers inspected; source-data previews follow the configured row and column limits.")),
    tables, c("Original ggplot" = "original"),
    c("Rendering uses the plot's full data and its own statistical transformations. Preview limits bound inspection tables, not ggplot's rendering computation.",
      "Global and layer data are inspected separately. Layer-data functions and aesthetic expressions are not evaluated for the metadata report; ggplot evaluates them when drawing.",
      "Chart appearance changes are optional. Mapped point/line aesthetics are retained. Colour/fill scales change only when explicitly requested; transformations, limits and labels are retained.",
      "CSV saves the selected inspection table. PNG and offline HTML use the currently displayed plot."), limits)
}

rr_edit_ggplot <- function(x, style = list()) {
  if (!rr_is_ggplot(x)) stop("Expected a standard ggplot object.", call. = FALSE)
  if (!length(style)) return(x)
  s <- rr_style(style)
  active <- names(style)
  out <- x + ggplot2::theme()
  if ("theme" %in% active) out <- out + rr_style_theme(s$theme)(base_size = s$base_size)
  out <- rr_style_layers(out, s, active)
  out <- rr_style_scales(out, s, active)
  out <- rr_style_details(out, s, active)
  attr(out, "rresult_style") <- s
  out
}
