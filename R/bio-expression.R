# Read all requested observations through bounded assay windows. Coordinates and
# summaries have a different scope from an object-explorer matrix preview.
rr_bio_expression_blocks <- function(source, genes, index, limits, consume) {
  size <- min(limits$max_plot_points, limits$max_cells) %/% length(genes)
  if (size < 1L) stop("Expression selection exceeds matrix display limits.", call. = FALSE)
  if (!length(index)) return(invisible(NULL))
  for (start in seq.int(1L, length(index), by = size)) {
    positions <- seq.int(start, min(length(index), start + size - 1L))
    consume(rr_bio_slice(source, genes, index[positions], limits), positions)
  }
  invisible(NULL)
}

rr_bio_expression_values <- function(source, gene, index, limits) {
  cache <- source$value_cache
  if (!is.null(cache) && identical(cache$gene, gene) && identical(cache$index, index) &&
      identical(cache$reader, source$read)) return(cache$value)
  value <- numeric(length(index))
  rr_bio_expression_blocks(source, gene, index, limits, function(m, positions) {
    value[positions] <<- as.numeric(m)
  })
  if (!is.null(cache)) {
    cache$gene <- gene; cache$index <- index; cache$reader <- source$read; cache$value <- value
  }
  value
}

rr_bio_expression_summary <- function(source, genes, index, group, limits) {
  groups <- levels(droplevels(group))
  codes <- match(as.character(group), groups)
  totals <- positives <- finite_counts <- matrix(0, length(genes), length(groups))
  rr_bio_expression_blocks(source, genes, index, limits, function(m, positions) {
    block_groups <- codes[positions]
    for (g in unique(block_groups)) {
      v <- m[, block_groups == g, drop = FALSE]
      finite <- is.finite(v)
      v[!finite] <- 0
      totals[, g] <<- totals[, g] + rowSums(v)
      positives[, g] <<- positives[, g] + rowSums(v > 0)
      finite_counts[, g] <<- finite_counts[, g] + rowSums(finite)
    }
  })
  do.call(rbind, lapply(seq_along(genes), function(i) {
    n <- finite_counts[i, ]
    data.frame(feature = source$features[genes[i]], group = groups,
      mean = ifelse(n > 0, totals[i, ] / n, NA_real_),
      above_zero = ifelse(n > 0, 100 * positives[i, ] / n, NA_real_),
      observations = n)
  }))
}
