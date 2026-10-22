# CSV numeric fields need 17 significant digits to round-trip binary doubles.
# write.csv() otherwise uses a shorter internal format, regardless of digits.
rr_write_csv <- function(data, file, ...) {
  args <- list(...)
  requested_quote <- if ("quote" %in% names(args)) args$quote else TRUE
  # Remember the source character columns before encoding numeric fields.
  quote_columns <- if (is.data.frame(data)) which(vapply(data,
    function(value) is.character(value) || is.factor(value), logical(1))) else integer()
  encoded_columns <- integer()
  encode <- function(value) {
    text <- trimws(formatC(value, digits = 17L, format = "g", decimal.mark = "."))
    text[is.na(value)] <- NA_character_
    text
  }
  plain_double <- function(value) typeof(value) == "double" &&
    is.null(attr(value, "class", exact = TRUE))
  if (is.matrix(data)) {
    if (plain_double(data)) {
      encoded_columns <- seq_len(ncol(data))
      data <- matrix(encode(data), nrow = nrow(data), ncol = ncol(data), dimnames = dimnames(data))
    }
  } else if (is.data.frame(data)) {
    for (j in seq_along(data)) {
      value <- data[[j]]
      if (plain_double(value) && is.null(attr(value, "dim", exact = TRUE))) {
        encoded_columns <- c(encoded_columns, j)
        data[[j]] <- encode(value)
      }
    }
  }
  if (length(encoded_columns)) {
    # Numeric CSV fields stay unquoted, including for explicitly typed readers.
    if (isTRUE(requested_quote)) args$quote <- quote_columns
    else if (is.numeric(requested_quote)) args$quote <- setdiff(requested_quote, encoded_columns)
  }
  do.call(utils::write.csv, c(list(x = data, file = file), args))
}
