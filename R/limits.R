# Internal limits are checked at every provider boundary, including UI calls.
rr_integer <- function(x, name, lower = 1, upper = 1e6) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower || x > upper) {
    stop(name, " must be an integer between ", lower, " and ", upper, ".", call. = FALSE)
  }
  as.integer(x)
}

rr_limits <- function(max_rows = 100, max_cols = 50, max_depth = 4,
                      max_preview_bytes = 5 * 1024^2, max_cells = 10000,
                      max_children = 50, max_plot_points = 10000,
                      max_scan_rows = 100000, max_name_scan = 1000000) {
  list(max_rows = rr_integer(max_rows, "max_rows", upper = 10000),
       max_cols = rr_integer(max_cols, "max_cols", upper = 1000),
       max_depth = rr_integer(max_depth, "max_depth", upper = 20),
       max_preview_bytes = rr_integer(max_preview_bytes, "max_preview_bytes", 1024, 50 * 1024^2),
       max_cells = rr_integer(max_cells, "max_cells", upper = 1000000),
       max_children = rr_integer(max_children, "max_children", upper = 1000),
       max_plot_points = rr_integer(max_plot_points, "max_plot_points", 2, 100000),
       max_scan_rows = rr_integer(max_scan_rows, "max_scan_rows", upper = 1000000),
       max_name_scan = rr_integer(max_name_scan, "max_name_scan", upper = 10000000))
}

rr_text <- function(x, limit = 160L) {
  # Only call for known character values, never an arbitrary object's method.
  x <- ifelse(is.na(x), "NA", x)
  substr(x, 1L, limit)
}

rr_budget <- function(x, limits) {
  if (as.numeric(utils::object.size(x)) > limits$max_preview_bytes) {
    stop("Preview exceeds max_preview_bytes; request fewer rows or columns.", call. = FALSE)
  }
  x
}

rr_plain <- function(x) {
  if (isS4(x)) stop("S4 values require a supported adapter.", call. = FALSE)
  if (is.object(x)) unclass(x) else x
}

rr_simple <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  !isS4(x) && typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw") &&
    (is.null(cls) || identical(cls, "factor") || identical(cls, c("ordered", "factor")) ||
       identical(cls, "Date") || identical(cls, c("POSIXct", "POSIXt")))
}

rr_sparse <- function(x) {
  if (!isS4(x) || !requireNamespace("Matrix", quietly = TRUE)) return(FALSE)
  definition <- methods::getClassDef(class(x)[1L])
  !is.null(definition) && identical(definition@package, "Matrix") &&
    methods::is(x, "sparseMatrix")
}
