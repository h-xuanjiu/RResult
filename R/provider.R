# Live data providers ---------------------------------------------------------

rr_window <- function(start, count, total, cap, label) {
  start <- rr_integer(start, paste(label, "start"), upper = max(total, 1))
  count <- rr_integer(count, paste(label, "count"), upper = cap)
  if (!total) return(integer())
  if (start + count - 1 > total) stop(label, " range is out of bounds.", call. = FALSE)
  seq.int(start, start + count - 1L)
}

rr_indices <- function(index, total, cap, label) {
  if (!is.numeric(index) || anyNA(index) || any(!is.finite(index)) ||
      any(index != floor(index)) || any(index < 1 | index > total) || length(index) > cap) {
    stop(label, " indices are invalid or exceed the request limit.", call. = FALSE)
  }
  as.integer(index)
}

rr_dimnames <- function(x) {
  if (rr_sparse(x)) methods::slot(x, "Dimnames") else attr(x, "dimnames", exact = TRUE)
}

rr_name_select <- function(labels, query, cap, limits) {
  if (!is.character(query) || anyNA(query) || length(query) > 100L ||
      sum(nchar(query, type = "bytes")) > 8192) stop("Invalid name query (maximum 100 names / 8192 bytes).", call. = FALSE)
  if (is.null(labels)) return(list(index = integer(), missing = query, duplicates = character(), scanned = 0L))
  if (length(labels) > limits$max_name_scan) stop("Name lookup exceeds max_name_scan; use numeric ranges or raise the bounded limit.", call. = FALSE)
  query <- unique(query)
  index <- integer()
  counts <- integer(length(query))
  # Chunking avoids full-sized match vectors and never sends the name set to UI.
  for (start in seq.int(1L, max(1L, length(labels)), by = 4096L)) {
    if (!length(labels)) break
    end <- min(length(labels), start + 4095L)
    hit <- match(labels[start:end], query)
    ok <- which(!is.na(hit))
    if (length(ok)) {
      counts <- counts + tabulate(hit[ok], nbins = length(query))
      if (length(index) + length(ok) > cap) stop("Name matches exceed the axis limit; duplicate names select every occurrence. Narrow the query.", call. = FALSE)
      index <- c(index, start + ok - 1L)
    }
  }
  list(index = index, missing = query[counts == 0L], duplicates = query[counts > 1L], scanned = length(labels))
}

rr_name_search <- function(x, axis, query, limits) {
  if (!rr_kind(x) %in% c("matrix", "sparse_matrix")) stop("Name search requires a matrix.", call. = FALSE)
  if (!is.character(query) || length(query) != 1L || is.na(query) || nchar(query, type = "bytes") > 256L) stop("Search must be one short literal string.", call. = FALSE)
  axis <- match.arg(axis, c("rows", "columns"))
  labels <- rr_dimnames(x)[[if (axis == "rows") 1L else 2L]]
  if (is.null(labels)) return(list(matches = data.frame(index = integer(), name = character()), scanned = 0L, truncated = FALSE))
  n <- min(length(labels), limits$max_name_scan)
  found <- integer()
  scanned <- 0L
  truncated <- n < length(labels)
  if (n) for (start in seq.int(1L, n, by = 4096L)) {
    end <- min(n, start + 4095L)
    hit <- which(!is.na(labels[start:end]) & grepl(query, labels[start:end], fixed = TRUE)) + start - 1L
    room <- limits$max_children - length(found)
    found <- c(found, utils::head(hit, room))
    scanned <- end
    if (length(hit) > room || (length(found) == limits$max_children && end < length(labels))) {
      truncated <- TRUE
      break
    }
  }
  rr_budget(list(matches = data.frame(index = found, name = rr_text(labels[found])),
                 scanned = scanned, truncated = truncated), limits)
}

rr_matrix <- function(x, limits, row_start = 1L, n_rows = NULL, col_start = 1L,
                      n_cols = NULL, rows = NULL, cols = NULL, row_names = NULL,
                      col_names = NULL) {
  if (!rr_kind(x) %in% c("matrix", "sparse_matrix")) stop("A supported matrix is required.", call. = FALSE)
  dims <- rr_dims(x)
  dn <- rr_dimnames(x)
  row_lookup <- col_lookup <- list(missing = character(), duplicates = character())
  if (!is.null(row_names)) {
    if (!is.null(rows)) stop("Use rows or row_names, not both.", call. = FALSE)
    row_lookup <- rr_name_select(dn[[1L]], row_names, limits$max_rows, limits)
    rows <- row_lookup$index
  }
  if (!is.null(col_names)) {
    if (!is.null(cols)) stop("Use cols or col_names, not both.", call. = FALSE)
    col_lookup <- rr_name_select(dn[[2L]], col_names, limits$max_cols, limits)
    cols <- col_lookup$index
  }
  if (is.null(rows)) rows <- rr_window(row_start, if (is.null(n_rows)) max(1L, min(limits$max_rows, dims[1L] - row_start + 1L)) else n_rows,
                                      dims[1L], limits$max_rows, "Row") else rows <- rr_indices(rows, dims[1L], limits$max_rows, "Row")
  if (is.null(cols)) cols <- rr_window(col_start, if (is.null(n_cols)) max(1L, min(limits$max_cols, dims[2L] - col_start + 1L)) else n_cols,
                                      dims[2L], limits$max_cols, "Column") else cols <- rr_indices(cols, dims[2L], limits$max_cols, "Column")
  cells <- as.double(length(rows)) * length(cols)
  if (cells > limits$max_cells) stop("Request exceeds max_cells. Select a smaller window.", call. = FALSE)
  bytes_per_cell <- switch(typeof(x), complex = 16, character = 16, 8)
  if (cells * bytes_per_cell > limits$max_preview_bytes) stop("Window exceeds max_preview_bytes before allocation.", call. = FALSE)
  if (rr_sparse(x)) {
    # The only dense conversion happens AFTER all three limits have passed.
    slice <- x[rows, cols, drop = FALSE]
    data <- as.matrix(slice)
  } else {
    data <- rr_plain(x)[rows, cols, drop = FALSE]
  }
  result <- list(data = data, row_indices = rows, col_indices = cols,
                 missing_rows = row_lookup$missing, missing_cols = col_lookup$missing,
                 duplicate_rows = row_lookup$duplicates, duplicate_cols = col_lookup$duplicates,
                 scope = "Only the requested matrix window; duplicate names select all occurrences.",
                 total_dim = dims)
  rr_budget(result, limits)
}

rr_df_nrow <- function(x) {
  # .row_names_info handles compact row names without expanding them.
  .row_names_info(x, type = 2L)
}

rr_df_row_names <- function(x, index) {
  # Select only the requested labels; automatic 1:n names stay compact.
  labels <- .row_names_info(x, type = 0L)
  if (is.integer(labels) && length(labels) == 2L && is.na(labels[1L]))
    return(as.character(index))
  as.character(labels[index])
}

rr_cell_text <- function(x, index, width = NULL) {
  if (rr_simple(x) && is.null(attr(x, "dim", exact = TRUE))) {
    cls <- attr(x, "class", exact = TRUE)
    v <- rr_plain(x)[index]
    attributes(v) <- NULL
    if (any(cls %in% c("factor", "ordered"))) v <- attr(x, "levels", exact = TRUE)[v]
    if (identical(cls, "Date")) v <- as.character(as.Date(v, origin = "1970-01-01"))
    if (identical(cls, c("POSIXct", "POSIXt"))) v <- format(as.POSIXct(v, origin = "1970-01-01", tz = "UTC"), tz = "UTC", usetz = TRUE)
    if (is.numeric(v)) {
      text <- format(v, digits = 7, trim = TRUE)
      text[is.na(v) & !is.nan(v)] <- "NA"
      text[is.nan(v)] <- "NaN"
    } else text <- as.character(v)
    return(if (is.null(width)) text else rr_text(text, width))
  }
  # List-columns and custom classes are described, never printed or flattened.
  if (typeof(x) == "list" && !isS4(x)) {
    return(vapply(index, function(i) {
      v <- rr_plain(x)[[i]]
      paste0("<", typeof(v), "; length ", rr_length(v), ">")
    }, character(1)))
  }
  rep(paste0("<", typeof(x), "; unsupported column>"), length(index))
}

# Explicit atomic windows make every value reachable without sending the source.
rr_values <- function(x, limits, start = 1L, count = NULL) {
  if (!rr_kind(x) %in% c("vector", "array")) stop("A supported atomic vector or array is required.", call. = FALSE)
  n <- length(rr_plain(x))
  if (is.null(count)) count <- max(1L, min(limits$max_rows, n - start + 1L))
  index <- rr_window(start, count, n, limits$max_rows, "Value")
  value <- rr_plain(x)[index]
  attributes(value) <- NULL
  cls <- attr(x, "class", exact = TRUE)
  if (any(cls %in% "factor")) value <- attr(x, "levels", exact = TRUE)[value]
  rr_budget(list(value = value, indices = index, population = n, used = length(index),
    sampled = FALSE, class = cls,
    scope = paste0("Values ", if (length(index)) min(index) else 0L, "-", if (length(index)) max(index) else 0L,
      " of ", n, "; exact indexed window. Arrays use column-major order.")), limits)
}

rr_df <- function(x, limits, row_start = 1L, n_rows = NULL, cols = NULL,
                  page = 1L, page_size = NULL, search = "", sort_col = NULL,
                  decreasing = FALSE) {
  if (rr_kind(x) != "data_frame") stop("A data frame is required.", call. = FALSE)
  columns <- rr_plain(x)
  total <- rr_df_nrow(x)
  if (is.null(cols)) cols <- seq_len(min(length(columns), limits$max_cols))
  cols <- rr_indices(cols, length(columns), limits$max_cols, "Column")
  if (is.null(n_rows)) n_rows <- max(1L, min(total - row_start + 1L, limits$max_scan_rows))
  scan <- rr_window(row_start, n_rows, total, limits$max_scan_rows, "Search row")
  if (!is.character(search) || length(search) != 1L || is.na(search) || nchar(search, type = "bytes") > 256L) stop("Search must be one short literal string.", call. = FALSE)
  if (nzchar(search)) {
    keep <- rep(FALSE, length(scan))
    for (j in cols) {
      column <- columns[[j]]
      if (rr_simple(column) && is.null(attr(column, "dim", exact = TRUE))) {
        # Search operates in chunks rather than materializing a whole character table.
        for (start in seq.int(1L, max(1L, length(scan)), by = 4096L)) {
          if (!length(scan)) break
          ix <- start:min(length(scan), start + 4095L)
          keep[ix] <- keep[ix] | grepl(search, rr_cell_text(column, scan[ix]), fixed = TRUE)
        }
      }
    }
    scan <- scan[keep]
  }
  if (!is.null(sort_col)) {
    sort_col <- rr_integer(sort_col, "sort_col", upper = max(1L, length(columns)))
    if (!sort_col %in% cols) stop("Sort column must be selected.", call. = FALSE)
    column <- columns[[sort_col]]
    if (!rr_simple(column) || !is.null(attr(column, "dim", exact = TRUE)) || typeof(column) %in% c("complex", "raw")) stop("This column cannot be sorted safely.", call. = FALSE)
    v <- rr_plain(column)[scan]
    attributes(v) <- NULL
    if (identical(attr(column, "class", exact = TRUE), "factor")) v <- attr(column, "levels", exact = TRUE)[v]
    scan <- scan[order(v, decreasing = isTRUE(decreasing), na.last = TRUE)]
  }
  page <- rr_integer(page, "page", upper = 1000000)
  page_size <- if (is.null(page_size)) limits$max_rows else rr_integer(page_size, "page_size", upper = limits$max_rows)
  if (page_size * length(cols) > limits$max_cells) stop("Page exceeds max_cells.", call. = FALSE)
  first <- (as.double(page) - 1) * page_size + 1
  indices <- if (first > length(scan)) integer() else scan[seq.int(first, min(length(scan), first + page_size - 1L))]
  values <- lapply(cols, function(j) rr_cell_text(columns[[j]], indices))
  names(values) <- if (length(cols)) paste0("c", cols) else character()
  data <- as.data.frame(values, stringsAsFactors = FALSE, optional = TRUE)
  if (!length(cols)) data <- data.frame(row.names = seq_along(indices))
  row_labels <- rr_df_row_names(x, indices)
  rownames(data) <- row_labels
  label <- names(columns)[cols]
  profile <- lapply(cols, function(j) {
    column <- columns[[j]]
    sample <- if (rr_simple(column)) rr_sample(column, limits) else NULL
    list(index = j, name = rr_text(names(columns)[j]),
      class = rr_text(utils::head(class(column), 10L)), typeof = typeof(column),
      sample_size = if (is.null(sample)) NA_integer_ else sample$used,
      missing_in_sample = if (is.null(sample)) NA_integer_ else sum(is.na(sample$value)),
      note = if (is.null(sample)) "List/custom/matrix column; no automatic summary" else "Missing count is for the bounded, evenly spaced column sample")
  })
  result <- list(data = data, row_indices = indices, row_labels = row_labels, col_indices = cols,
                 column_labels = rr_text(label), total_rows = total, matched_rows = length(scan),
                 column_profile = profile,
                 pages = max(1L, ceiling(length(scan) / page_size)), page = page,
                 scope = paste0("Search and sort apply only to rows ", row_start, "-", min(total, row_start + n_rows - 1),
                   " and selected columns. Text search uses complete cell values; list/custom columns are skipped."))
  rr_budget(result, limits)
}

rr_extract <- function(x, limits, path = NULL, ...) {
  x <- rr_resolve(x, path, limits)
  kind <- rr_kind(x)
  if (kind %in% c("matrix", "sparse_matrix")) return(rr_matrix(x, limits, ...))
  if (kind == "data_frame") return(rr_df(x, limits, ...))
  if (kind %in% c("vector", "array")) {
    args <- list(...)
    if (length(args)) return(do.call(rr_values, c(list(x = x, limits = limits), args)))
    s <- rr_sample(x, limits)
    s$value <- utils::head(s$value, limits$max_rows)
    s$indices <- utils::head(s$indices, limits$max_rows)
    s$used <- length(s$value)
    s$sampled <- s$used < s$population
    return(rr_budget(s, limits))
  }
  list(info = rr_info(x, limits), children = rr_children(x, limits, depth = length(path)))
}
