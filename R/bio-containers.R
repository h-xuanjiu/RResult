# Explicit adapters for saved bioinformatics containers and results.
.rr_bio_containers <- c(
  "Seurat", "Assay", "Assay5", "SummarizedExperiment", "RangedSummarizedExperiment",
  "SingleCellExperiment", "SpatialExperiment", "DESeqDataSet", "DESeqTransform", "DGEList"
)
.rr_bio_results <- c("DGEExact", "DGELRT", "MArrayLM", "enrichResult", "gseaResult", "compareClusterResult")
.rr_bio_families <- c(.rr_bio_containers, .rr_bio_results)

rr_bio_kind <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  if (!isS4(x) || length(cls) != 1L || !as.character(cls) %in% .rr_bio_families) {
    return(NULL)
  }
  kind <- as.character(cls)
  package <- switch(kind,
    Seurat = ,
    Assay = ,
    Assay5 = "SeuratObject",
    SummarizedExperiment = ,
    RangedSummarizedExperiment = "SummarizedExperiment",
    DESeqDataSet = ,
    DESeqTransform = "DESeq2",
    DGEList = ,
    DGEExact = ,
    DGELRT = "edgeR",
    MArrayLM = "limma",
    enrichResult = ,
    gseaResult = ,
    compareClusterResult = c("DOSE", "enrichit"),
    kind
  )
  definition <- methods::getClassDef(kind)
  if (is.null(definition)) {
    for (pkg in package) {
      if (requireNamespace(pkg, quietly = TRUE)) {
        definition <- methods::getClassDef(kind)
        if (!is.null(definition)) break
      }
    }
  }
  required <- switch(kind,
    enrichResult = ,
    gseaResult = "result",
    compareClusterResult = "compareClusterResult",
    character()
  )
  if (!is.null(definition) && definition@package %in% package && all(required %in% names(definition@slots))) kind else NULL
}

rr_bio_pick <- function(value, choices, label) {
  if (!length(choices)) {
    return(NULL)
  }
  if (is.null(value) || identical(value, "")) {
    return(choices[1L])
  }
  if (is.numeric(value)) value <- choices[rr_integer(value, label, upper = length(choices))]
  if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% choices) stop("Unknown ", label, ".", call. = FALSE)
  value
}

rr_bio_frame <- function(data, index, limits, ids = NULL, id_label = "id", complete = FALSE) {
  if (is.null(data)) {
    return(if (is.null(ids)) data.frame() else stats::setNames(data.frame(ids[index]), id_label))
  }
  cols <- if (complete) seq_len(ncol(data)) else seq_len(min(ncol(data), max(0L, limits$max_cols - 1L), max(0L, floor(limits$max_cells / max(1L, length(index))) - 1L)))
  out <- lapply(cols, function(j) {
    raw <- data[[j]]
    if (!rr_simple(raw) && !rr_bio_rle(raw)) {
      return(rr_cell_text(raw, index))
    }
    v <- rr_bio_column_values(raw, index)
    if (is.factor(v)) {
      return(as.character(v))
    }
    if (rr_simple(v) && is.null(dim(v))) {
      return(v)
    }
    rr_cell_text(data[[j]], index)
  })
  names(out) <- make.unique(names(data)[cols])
  out <- as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
  if (is.null(ids)) ids <- as.character(seq_len(nrow(data)))
  if (!length(out)) out <- data.frame(row.names = seq_along(index))
  names_id <- make.unique(c(id_label, names(out)))
  out <- cbind(stats::setNames(data.frame(ids[index], stringsAsFactors = FALSE), names_id[1L]), out)
  names(out) <- names_id
  out
}

rr_bio_source <- function(x, limits, assay = NULL, layer = NULL, lazy = FALSE) {
  kind <- rr_bio_kind(x)
  if (is.null(kind) || !kind %in% .rr_bio_containers) stop("A supported expression container is required.", call. = FALSE)
  reductions <- character()
  reduced <- function(name) NULL
  spatial <- NULL
  feature_data <- NULL
  feature_data_get <- function() NULL
  reduced_dims <- function(name) NULL
  meta <- NULL
  identity <- NULL
  meta_index <- function(index) index
  if (kind %in% c("Seurat", "Assay", "Assay5")) {
    rr_require("SeuratObject", "5.0.0")
    assays <- if (kind == "Seurat") SeuratObject::Assays(x) else "assay"
    if (kind == "Seurat" && is.null(assay)) assay <- SeuratObject::DefaultAssay(x)
    assay <- rr_bio_pick(assay, assays, "assay")
    a <- if (kind == "Seurat") x[[assay]] else x
    layers <- SeuratObject::Layers(a, search = NA)
    if (is.null(layer)) layer <- intersect(c("data", "logcounts", "counts", "scale.data"), layers)[1L]
    if (length(layer) && is.na(layer)) layer <- NULL
    layer <- rr_bio_pick(layer, layers, "layer")
    if (is.null(layer)) stop("This assay has no saved expression layers.", call. = FALSE)
    features <- SeuratObject::Features(a, layer = layer)
    cells <- SeuratObject::Cells(a, layer = layer)
    read <- function(rows, cols) {
      value <- SeuratObject::LayerData(a, layer = layer, features = features[rows], cells = cells[cols])
      # The legacy Assay accessor in SeuratObject 5.0 drops single dimensions.
      if (is.null(dim(value))) value <- matrix(value, nrow = length(rows), ncol = length(cols), dimnames = list(features[rows], cells[cols]))
      value
    }
    feature_data_get <- rr_bio_memoise(function() a[[]])
    if (kind == "Seurat") {
      meta <- x[[]]
      meta_index <- rr_bio_memoise(function(index) match(cells[index], rownames(meta)))
      identity <- SeuratObject::Idents(x)
      reductions <- SeuratObject::Reductions(x)
      reduced <- function(name) SeuratObject::Embeddings(x[[name]])
      reduced_dims <- function(name) dim(x[[name]])
    }
  } else if (kind == "DGEList") {
    rr_require("edgeR")
    assays <- "counts"
    assay <- rr_bio_pick(assay, assays, "assay")
    layers <- "counts"
    layer <- rr_bio_pick(layer, layers, "layer")
    m <- x$counts
    features <- rownames(m)
    cells <- colnames(m)
    read <- function(rows, cols) m[rows, cols, drop = FALSE]
    meta <- x$samples
    feature_data_get <- function() x$genes
  } else {
    rr_require("SummarizedExperiment")
    assays <- SummarizedExperiment::assayNames(x)
    if (is.null(assays)) assays <- rep("", length(SummarizedExperiment::assays(x, withDimnames = FALSE)))
    unnamed <- is.na(assays) | !nzchar(assays)
    if (any(unnamed)) {
      # Reserve saved names before generating display labels for unnamed assays.
      labels <- make.unique(c(assays[!unnamed], paste("Assay", which(unnamed))))
      assays[unnamed] <- utils::tail(labels, sum(unnamed))
    }
    assays <- make.unique(assays)
    assay <- rr_bio_pick(assay, assays, "assay")
    assay_index <- match(assay, assays)
    layers <- if (is.null(assay)) character() else assay
    layer <- rr_bio_pick(layer, layers, "layer (select assay for Bioconductor objects)")
    features <- rownames(x)
    cells <- colnames(x)
    get_assay <- rr_bio_memoise(function() {
      # Display labels need not be native names; retain the original position.
      if (is.null(assay)) NULL else SummarizedExperiment::assay(x, assay_index, withDimnames = FALSE)
    })
    read <- function(rows, cols) {
      m <- get_assay()
      if (is.null(m)) stop("This object has no saved assay.", call. = FALSE)
      m[rows, cols, drop = FALSE]
    }
    meta <- SummarizedExperiment::colData(x)
    feature_data_get <- rr_bio_memoise(function() SummarizedExperiment::rowData(x))
    if (kind %in% c("SingleCellExperiment", "SpatialExperiment")) {
      rr_require("SingleCellExperiment")
      reductions <- SingleCellExperiment::reducedDimNames(x)
      reduced <- function(name) SingleCellExperiment::reducedDim(x, name, withDimnames = FALSE)
      reduced_dims <- function(name) dim(SingleCellExperiment::reducedDim(x, name, withDimnames = FALSE))
    }
    if (kind == "SpatialExperiment") {
      rr_require("SpatialExperiment")
      spatial <- SpatialExperiment::spatialCoords(x)
    }
  }
  nr <- if (kind %in% c("Seurat", "Assay", "Assay5")) length(features) else if (kind == "DGEList") nrow(x$counts) else nrow(x)
  nc <- if (kind %in% c("Seurat", "Assay", "Assay5")) length(cells) else if (kind == "DGEList") ncol(x$counts) else ncol(x)
  named_features <- !is.null(features)
  named_cells <- !is.null(cells)
  if (is.null(features)) features <- as.character(seq_len(nr))
  if (is.null(cells)) cells <- as.character(seq_len(nc))
  all_reductions <- reductions
  reductions <- reductions[vapply(reductions, function(name) {
    d <- reduced_dims(name)
    length(d) == 2L && d[2L] >= 2L
  }, logical(1))]
  # Display windows do not restrict which saved annotations can be selected.
  metadata_names <- if (is.null(meta)) character() else names(meta)
  meta_types <- stats::setNames(vapply(metadata_names, function(nm) rr_bio_column_type(meta[[nm]]), character(1)), metadata_names)
  identity_key <- if (is.null(identity)) NULL else utils::tail(make.unique(c(names(meta), ".identity")), 1L)
  if (!is.null(identity)) meta_types <- c(meta_types, stats::setNames("category", identity_key))
  if (!lazy) feature_data <- feature_data_get()
  # Keep only the last native coordinate reference; never hash or size the
  # complete embedding merely to decide whether a preview can use it.
  reduced <- rr_bio_memoise(reduced)
  coordinate_name <- NULL
  coordinate_ids <- NULL
  coordinate_index <- rr_bio_memoise(function(name, index) {
    if (!identical(name, coordinate_name)) {
      ids <- rownames(reduced(name))
      if (is.null(ids) || anyDuplicated(ids)) stop("Saved Seurat coordinates need unique cell names.", call. = FALSE)
      coordinate_name <<- name
      coordinate_ids <<- ids
    }
    match(cells[index], coordinate_ids)
  })
  feature_ids_checked <- FALSE
  feature_index <- function(ids) {
    if (!feature_ids_checked) {
      if (anyDuplicated(features)) stop("Feature names are duplicated; use numeric row indices.", call. = FALSE)
      feature_ids_checked <<- TRUE
    }
    match(ids, features)
  }
  list(
    kind = kind, assays = assays, assay = assay, layers = layers, layer = layer,
    features = features, cells = cells, named_features = named_features, named_cells = named_cells,
    n_features = nr, n_cells = nc, read = read, window_cache = new.env(parent = emptyenv()), value_cache = new.env(parent = emptyenv()), meta = meta, meta_index = meta_index, identity = identity, identity_key = identity_key, feature_data = feature_data,
    feature_data_get = feature_data_get, feature_index = feature_index, coordinate_index = coordinate_index,
    reductions = reductions, all_reductions = all_reductions, reduced = reduced, spatial = spatial, meta_types = meta_types
  )
}

rr_bio_slice <- function(source, rows, cols, limits) {
  rows <- rr_indices(rows, source$n_features, limits$max_rows, "Feature")
  cols <- rr_indices(cols, source$n_cells, limits$max_plot_points, "Observation")
  if (as.double(length(rows)) * length(cols) > min(limits$max_cells, limits$max_plot_points)) stop("Expression selection exceeds max_cells/max_plot_points.", call. = FALSE)
  cache <- source$window_cache
  if (!is.null(cache) && identical(cache$rows, rows) && identical(cache$cols, cols) &&
      identical(cache$reader, source$read)) return(rr_budget(cache$value, limits))
  value <- rr_bio_window(source, rows, cols, limits)
  if (!is.null(cache)) {
    cache$rows <- rows; cache$cols <- cols; cache$reader <- source$read; cache$value <- value
  }
  value
}

rr_bio_window <- function(source, rows, cols, limits) {
  # Cache the realized, bounded matrix, rather than a DelayedMatrix view
  # whose as.matrix() would otherwise repeat disk I/O on every render.
  value <- as.matrix(source$read(rows, cols))
  if (!is.numeric(value) || is.complex(value) || !identical(dim(value), c(length(rows), length(cols)))) stop("Selected assay must provide a numeric feature-by-observation matrix.", call. = FALSE)
  # Seurat accessors can order selected features/cells by the native layer order.
  if (source$kind %in% c("Seurat", "Assay", "Assay5") && !is.null(rownames(value))) value <- value[match(source$features[rows], rownames(value)), , drop = FALSE]
  if (source$kind %in% c("Seurat", "Assay", "Assay5") && !is.null(colnames(value))) value <- value[, match(source$cells[cols], colnames(value)), drop = FALSE]
  rr_budget(value, limits)
}

rr_bio_features <- function(source, feature, limits, default = 1L) {
  if (is.null(feature) || !length(feature) || identical(feature, "")) {
    return(seq_len(min(default, source$n_features, limits$max_rows)))
  }
  if (is.numeric(feature)) {
    return(rr_indices(feature, source$n_features, limits$max_rows, "Feature"))
  }
  if (!is.character(feature) || anyNA(feature) || anyDuplicated(feature) || length(feature) > limits$max_rows) stop("Use unique feature IDs or numeric row indices within max_rows.", call. = FALSE)
  if (is.function(source$feature_index)) idx <- source$feature_index(feature) else {
    if (anyDuplicated(source$features)) stop("Feature names are duplicated; use numeric row indices.", call. = FALSE)
    idx <- match(feature, source$features)
  }
  if (anyNA(idx)) stop("Unknown feature: ", paste(feature[is.na(idx)], collapse = ", "), call. = FALSE)
  idx
}

rr_bio_group <- function(source, index, key = NULL, categorical = FALSE, observation_ids = NULL) {
  if (is.null(key) || identical(key, "") || identical(key, 0) || identical(key, "0")) {
    return(NULL)
  }
  data <- source$meta
  if (identical(key, source$identity_key) && !is.null(source$identity)) {
    ids <- if (is.null(observation_ids)) source$cells[index] else observation_ids
    v <- as.character(source$identity[ids])
    missing <- "(missing)"
    while (missing %in% v[!is.na(v)]) missing <- paste0(missing, "*")
    v[is.na(v)] <- missing
    return(list(value = factor(v, levels = unique(v)), name = "Saved identities", type = "category"))
  }
  if (is.null(data)) stop("This object has no observation metadata.", call. = FALSE)
  j <- if (is.character(key) && length(key) == 1L && sum(names(data) == key) == 1L) match(key, names(data)) else rr_integer(suppressWarnings(as.numeric(key)), "metadata column", upper = ncol(data))
  column <- data[[j]]
  type <- rr_bio_column_type(column)
  if (!type %in% if (categorical) "category" else c("category", "numeric")) stop("Choose a ", if (categorical) "categorical" else "numeric or categorical", " metadata column.", call. = FALSE)
  rows <- if (is.null(observation_ids)) source$meta_index(index) else match(observation_ids, rownames(data))
  v <- rr_bio_column_values(column, rows)
  if (type == "category") {
    v <- as.character(v)
    missing <- "(missing)"
    while (missing %in% v[!is.na(v)]) missing <- paste0(missing, "*")
    v[is.na(v)] <- missing
    v <- factor(v, levels = unique(v))
  }
  list(value = v, name = names(data)[j], type = type)
}

rr_bio_reduction <- function(source, reduction = NULL) {
  names <- source$reductions
  if (is.null(reduction)) {
    preference <- match(c("umap", "tsne", "pca"), tolower(names))
    preference <- preference[!is.na(preference)]
    if (length(preference)) reduction <- names[preference[1L]]
  }
  rr_bio_pick(reduction, names, "saved reduction")
}

rr_bio_container_choices <- function(source) {
  if (!source$n_cells) {
    return(character())
  }
  expression <- length(source$assays) && source$n_features > 0L
  c(
    if (length(source$reductions)) c("Saved embedding" = "embedding", if (expression) c("Gene on embedding" = "feature_map")),
    if (!is.null(source$spatial) && ncol(source$spatial) >= 2L) c("Spatial coordinates" = "spatial", if (expression) c("Gene on spatial coordinates" = "spatial_feature")),
    if (any(source$meta_types == "category")) c("Metadata composition" = "composition"),
    if (any(source$meta_types == "numeric")) c("Saved metadata distribution" = "metadata_distribution"),
    if (expression) c("Expression heatmap" = "expression_heatmap", "Gene distribution" = "expression_violin", "Grouped gene summary" = "expression_dot")
  )
}


# Decode only the selected Rle values; unknown metadata classes stay opaque.
rr_bio_rle <- function(x) {
  isS4(x) && identical(as.character(attr(x, "class", exact = TRUE)), "Rle") &&
    identical(methods::getClassDef("Rle")@package, "S4Vectors")
}

rr_bio_column_type <- function(x) {
  rr_variable_type(if (rr_bio_rle(x)) methods::slot(x, "values") else x)
}

rr_bio_column_values <- function(x, index) {
  if (rr_bio_rle(x)) as.vector(x[index]) else x[index]
}
