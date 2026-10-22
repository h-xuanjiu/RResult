# Object inspection -----------------------------------------------------------

rr_kind <- function(x) {
  if (rr_is_ggplot(x)) return("ggplot")
  if (rr_sparse(x)) return("sparse_matrix")
  if (isS4(x)) return("S4")
  d <- attr(x, "dim", exact = TRUE)
  cls <- attr(x, "class", exact = TRUE)
  if (typeof(x) == "list" && any(cls %in% c("data.frame", "tbl_df"))) return("data_frame")
  if (!is.null(d) && length(d) == 2L && rr_simple(x)) return("matrix")
  if (!is.null(d) && rr_simple(x)) return("array")
  if (typeof(x) == "list") return("list")
  if (typeof(x) == "environment") return("environment")
  if (typeof(x) %in% c("closure", "builtin", "special")) return("function")
  if (rr_simple(x)) return("vector")
  "opaque"
}

rr_length <- function(x) {
  if (rr_is_ggplot(x)) return(NA_real_)
  if (typeof(x) == "object" || (typeof(x) == "environment" && is.object(x))) return(NA_real_)
  if (isS4(x)) return(NA_real_)
  length(rr_plain(x))
}

rr_dims <- function(x) {
  if (rr_sparse(x)) return(methods::slot(x, "Dim"))
  if (!isS4(x) && typeof(x) == "list" && "data.frame" %in% attr(x, "class", exact = TRUE)) return(c(rr_df_nrow(x), length(rr_plain(x))))
  d <- attr(x, "dim", exact = TRUE)
  if (is.numeric(d)) d else NULL
}

rr_info <- function(x, limits) {
  if (rr_is_ggplot(x)) return(rr_budget(list(class = attr(x, "class", exact = TRUE),
    typeof = typeof(x), kind = "ggplot", length = NA_real_, dim = NULL, size_bytes = NA_real_,
    size_note = "Plot data and environments are not recursively traversed for memory estimation.",
    layers = length(x$layers), attributes = list(), attributes_truncated = FALSE,
    note = "Use the ggplot report for bounded layer, mapping and source-data inspection."), limits))
  kind <- rr_kind(x)
  attrs <- attributes(x)
  n <- rr_length(x)
  # object.size() recursively traverses lists, so composite sizes are omitted.
  payload_size <- kind %in% c("vector", "matrix", "array") && typeof(x) != "character"
  width <- switch(typeof(x), raw = 1, logical = 4, integer = 4, double = 8, complex = 16, 8)
  size <- if (payload_size) 56 + as.double(n) * width else NA_real_
  cls <- attr(x, "class", exact = TRUE)
  if (is.null(cls)) cls <- class(x)
  attrs_names <- utils::head(names(attrs), limits$max_children)
  attributes_info <- lapply(attrs_names, function(nm) {
    a <- attrs[[nm]]
    list(name = rr_text(nm), typeof = typeof(a), length = rr_length(a))
  })
  result <- list(class = rr_text(utils::head(cls, 20L)), typeof = typeof(x), kind = kind,
                 length = n, dim = rr_dims(x), size_bytes = size,
                 size_note = if (payload_size) "Atomic payload estimate; attributes and allocator overhead excluded" else "Not traversed; composite/character size is intentionally unavailable",
                 attributes = attributes_info, attributes_truncated = length(attrs) > limits$max_children)
  if (kind == "sparse_matrix") {
    sn <- methods::slotNames(x)
    result$sparse <- TRUE
    # Pattern matrices have no x slot; row-compressed storage uses j, not i.
    entry_slot <- if ("x" %in% sn) "x" else if ("i" %in% sn) "i" else "j"
    result$stored_entries <- length(methods::slot(x, entry_slot))
    # Numeric/index slot sizes are constant-time and do not densify.
    result$size_bytes <- sum(vapply(intersect(sn, c("x", "i", "j", "p", "Dim")),
      function(nm) {
        value <- methods::slot(x, nm)
        width <- switch(typeof(value), logical = 4, integer = 4, double = 8, complex = 16, 8)
        56 + as.double(length(value)) * width
      }, numeric(1)))
    result$size_note <- "Sparse storage slots (lower bound; names/attributes excluded)"
    result$storage_type <- if ("x" %in% sn) typeof(methods::slot(x, "x")) else "pattern"
  }
  if (kind == "function") {
    result$arguments <- rr_text(utils::head(names(formals(x)), limits$max_children))
    result$note <- "Arguments only; the function is never executed."
  }
  if (kind == "environment") {
    # ls() never forces promises or active bindings. Values are not fetched.
    bindings <- ls(envir = x, all.names = TRUE, sorted = FALSE)
    bindings <- utils::head(bindings, limits$max_children)
    result$bindings <- lapply(bindings, function(nm) list(name = rr_text(nm),
      active = bindingIsActive(nm, x), note = "Value not read: bindings may contain promises or mutable references"))
    result$note <- "Environment values are not forced. Binding listing uses base ls()."
  }
  if (kind == "opaque") result$note <- "Metadata only; custom summary, print, plot and conversion methods are not invoked."
  rr_budget(result, limits)
}

rr_child_count <- function(x) {
  switch(rr_kind(x), list = rr_length(x), data_frame = length(rr_plain(x)),
         S4 = length(methods::slotNames(x)), 0L)
}

rr_children <- function(x, limits, depth = 0L, offset = 1L) {
  if (depth >= limits$max_depth) return(list(nodes = list(), note = "Maximum tree depth reached.", total = rr_child_count(x)))
  total <- rr_child_count(x)
  if (!total) return(list(nodes = list(), note = "No safely expandable children.", total = 0L))
  offset <- rr_integer(offset, "child offset", upper = max(1, total))
  index <- seq.int(offset, min(total, offset + limits$max_children - 1L))
  kind <- rr_kind(x)
  labels <- if (kind == "S4") methods::slotNames(x) else names(rr_plain(x))
  nodes <- lapply(index, function(i) {
    label <- if (length(labels) >= i && !is.na(labels[i]) && nzchar(labels[i])) labels[i] else paste0("[[", i, "]]")
    value <- if (kind == "S4") methods::slot(x, labels[i]) else rr_plain(x)[[i]]
    list(index = i, label = rr_text(label), info = rr_info(value, limits), expandable = rr_child_count(value) > 0L)
  })
  rr_budget(list(nodes = nodes, total = total, offset = offset,
                 note = paste("Showing", length(index), "of", total, "children; expansion is on demand.")), limits)
}

rr_resolve <- function(x, path, limits) {
  if (is.null(path)) return(x)
  if (!is.list(path)) path <- as.list(path)
  if (length(path) > limits$max_depth) stop("Object path exceeds max_depth.", call. = FALSE)
  for (key in path) {
    kind <- rr_kind(x)
    if (!kind %in% c("list", "data_frame", "S4")) stop("Node cannot be safely expanded.", call. = FALSE)
    labels <- if (kind == "S4") methods::slotNames(x) else names(rr_plain(x))
    total <- rr_child_count(x)
    if (is.character(key) && length(key) == 1L && !is.na(key)) {
      hits <- which(labels == key)
      if (length(hits) != 1L) stop("Path name is missing or duplicated; use a numeric index.", call. = FALSE)
      key <- hits
    }
    key <- rr_integer(key, "path index", upper = max(total, 1))
    if (!total) stop("This node has no children.", call. = FALSE)
    x <- if (kind == "S4") methods::slot(x, labels[key]) else rr_plain(x)[[key]]
  }
  x
}

rr_sample <- function(x, limits) {
  if (!rr_simple(x)) stop("Only known atomic vectors can be summarized safely.", call. = FALSE)
  n <- length(rr_plain(x))
  index <- if (!n) integer() else unique(round(seq(1, n, length.out = min(n, limits$max_plot_points))))
  # Keep trusted date/factor formatting; omit large custom attributes.
  value <- rr_plain(x)[index]
  attributes(value) <- NULL
  cls <- attr(x, "class", exact = TRUE)
  if (identical(cls, "factor") || identical(cls, c("ordered", "factor"))) {
    # Do not attach an unbounded level set to the preview.
    value <- substr(attr(x, "levels", exact = TRUE)[value], 1L, 160L)
  }
  list(value = value, indices = index, sampled = n > length(index), population = n, used = length(index), class = cls)
}

rr_summary <- function(x, limits) {
  s <- rr_sample(x, limits)
  v <- s$value
  result <- list(population = s$population, used = s$used, sampled = s$sampled,
                 sampling = "Evenly spaced deterministic sample; missingness and statistics refer only to this sample",
                 missing = sum(is.na(v)), class = s$class)
  if (is.numeric(v) && !is.complex(v)) {
    finite <- v[is.finite(v)]
    result$non_finite <- sum(!is.finite(v) & !is.na(v))
    result$statistics <- if (length(finite)) c(min = min(finite), median = stats::median(finite),
      mean = mean(finite), max = max(finite), sd = stats::sd(finite)) else numeric()
  } else if (is.character(v) || is.logical(v)) {
    # Factor samples contain shortened preview labels. Count the original
    # sampled labels so display limits never merge distinct categories.
    categories <- if (any(s$class %in% "factor")) {
      attr(x, "levels", exact = TRUE)[rr_plain(x)[s$indices]]
    } else v
    freq <- sort(table(categories, useNA = "ifany"), decreasing = TRUE)
    result$frequencies <- utils::head(freq, limits$max_children)
    names(result$frequencies) <- make.unique(substr(names(result$frequencies), 1L, 80L))
    result$categories_truncated <- length(freq) > limits$max_children
  }
  if (any(s$class %in% c("Date", "POSIXct")) && length(result$statistics)) {
    range <- result$statistics[c("min", "max")]
    result$time_range <- if ("Date" %in% s$class) as.character(as.Date(range, origin = "1970-01-01")) else
      format(as.POSIXct(range, origin = "1970-01-01", tz = "UTC"), tz = "UTC", usetz = TRUE)
  }
  rr_budget(result, limits)
}
