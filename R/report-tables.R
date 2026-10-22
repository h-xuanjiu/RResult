# Only a page crosses the presentation boundary. CSV uses the source table.
rr_report_page <- function(data, limits, page = 1L, page_size = min(25L, limits$max_rows), column_start = 1L) {
  page <- rr_integer(as.numeric(page), "Table page", upper = .Machine$integer.max)
  page_size <- rr_integer(as.numeric(page_size), "Rows per page", upper = limits$max_rows)
  column_start <- rr_integer(as.numeric(column_start), "First table column", upper = .Machine$integer.max)
  column_start <- min(column_start, max(1L, ncol(data)))
  nc <- min(limits$max_cols, max(1L, limits$max_cells %/% page_size))
  cols <- if (!ncol(data)) integer() else seq.int(column_start, min(ncol(data), column_start + nc - 1L))
  page_size <- min(page_size, max(1L, limits$max_cells %/% max(1L, length(cols))))
  pages <- max(1L, ceiling(nrow(data) / page_size))
  page <- min(page, pages)
  start <- (page - 1) * page_size + 1
  rows <- if (!nrow(data)) integer() else seq.int(start, min(nrow(data), start + page_size - 1L))
  value <- data[rows, cols, drop = FALSE]
  # Long text stays available; fail with a useful limit error rather than
  # silently changing values when a single page is unusually large.
  rr_budget(list(data = value, rows = rows, cols = cols, page = page, pages = pages,
    page_size = page_size, total_rows = nrow(data), total_cols = ncol(data)), limits)
}

rr_report_csv <- function(report, table, file) {
  if (is.null(table) || !table %in% names(report$tables)) table <- names(report$tables)[1L]
  if (!length(table)) stop("No saved result table is available.", call. = FALSE)
  rr_write_csv(report$tables[[table]], file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}
