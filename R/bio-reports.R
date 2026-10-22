# Stored test results; any BH adjustment is calculated before display sampling.
rr_bio_coef <- function(x, bio_coef = NULL) {
  m <- x$coefficients
  choices <- colnames(m)
  if (is.null(choices)) choices <- paste("Coefficient", seq_len(ncol(m)))
  if (is.character(bio_coef) && !bio_coef %in% choices && grepl("^[0-9]+$", bio_coef)) bio_coef <- as.numeric(bio_coef)
  chosen <- rr_bio_pick(bio_coef, choices, "coefficient / contrast")
  list(index = match(chosen, choices), name = chosen, choices = choices)
}

rr_bio_result_data <- function(x, kind, limits, bio_coef = NULL, cap = limits$max_plot_points, complete = FALSE, include_table = FALSE) {
  if (kind %in% c("enrichResult", "gseaResult", "compareClusterResult")) {
    slot <- if (kind == "compareClusterResult") "compareClusterResult" else "result"
    d <- methods::slot(x, slot)
    if (!is.data.frame(d)) stop("Saved enrichment table is unavailable.", call. = FALSE)
    idx <- if (complete) seq_len(nrow(d)) else seq_len(min(nrow(d), cap, limits$max_rows))
    return(list(table = d[idx, , drop = FALSE], total = nrow(d), source = d, coefficient = NULL))
  }
  selected <- NULL
  if (kind == "MArrayLM") {
    selected <- rr_bio_coef(x, bio_coef)
    n <- nrow(x$coefficients); j <- selected$index
    idx <- if (complete) seq_len(n) else rr_sample_indices(n, min(cap, limits$max_plot_points, limits$max_cells %/% 5L))
    get <- function(v, rows = idx) if (is.matrix(v)) as.numeric(v[rows, j]) else if (length(v) == n) as.numeric(v[rows]) else rep(NA_real_, length(rows))
    d <- data.frame(logFC = get(x$coefficients), AveExpr = get(x$Amean), PValue = get(x$p.value))
    ids <- rownames(x$coefficients)
    all_p <- if (n <= limits$max_scan_rows && !is.null(x$p.value)) get(x$p.value, seq_len(n)) else NULL
    notes <- c(paste("Saved coefficient / contrast:", selected$name, "; raw P values are available only if saved in the fitted object."),
      "limma effects are stored coefficients, including possible intercepts. Interpret them as log2 fold changes only for appropriate contrasts fitted to log2-scale expression.")
    one_effect <- TRUE
  } else {
    raw <- x$table; n <- nrow(raw); ids <- rownames(raw)
    idx <- if (complete) seq_len(n) else rr_sample_indices(n, min(cap, limits$max_plot_points, limits$max_cells %/% max(2L, min(ncol(raw) + 2L, limits$max_cols))))
    d <- raw[idx, , drop = FALSE]
    all_p <- if (n <= limits$max_scan_rows) raw$PValue else NULL
    notes <- "Saved edgeR test table; no model fitting or new contrast is performed."
    effects <- grep("^logFC($|[.])", names(d), value = TRUE)
    df <- x$df.test
    one_effect <- length(effects) == 1L && (is.null(df) || all(df[is.finite(df)] == 1))
    if (length(effects) == 1L && effects != "logFC") d$logFC <- d[[effects]]
  }
  selected_ids <- if (is.null(ids)) as.character(idx) else ids[idx]
  saved_fdr <- "FDR" %in% names(d)
  adjustment <- "No saved raw P values available."
  if ("PValue" %in% names(d)) {
    valid <- is.finite(d$PValue) & d$PValue >= 0 & d$PValue <= 1
    d$PValue[!valid] <- NA_real_
    if (saved_fdr) adjustment <- "FDR is retained from the saved result table."
    else if (!is.null(all_p) && n) {
      all_p[!is.finite(all_p) | all_p < 0 | all_p > 1] <- NA_real_
      d$FDR <- stats::p.adjust(all_p, method = "BH")[idx]
      adjustment <- paste("BH FDR calculated across all", n, "stored gene rows before sampling; no filtering or refitting.")
    } else if (n > limits$max_scan_rows) adjustment <- "BH FDR is unavailable because the full gene family exceeds max_scan_rows; increase that limit to enable full-family adjustment."
  }
  # Saved gene-test results are ordinary narrow data frames. Full table
  # export does not read an expression matrix or container annotations.
  table <- if (include_table) data.frame(feature = selected_ids, d, check.names = FALSE, row.names = NULL) else
    if (complete) NULL else rr_bio_frame(d, seq_len(nrow(d)), limits, selected_ids, "feature")
  effects <- if ("logFC" %in% names(d)) as.numeric(d$logFC) else rep(NA_real_, length(idx))
  mean_col <- intersect(c("AveExpr", "logCPM"), names(d))
  mean <- if (length(mean_col)) as.numeric(d[[mean_col[1L]]]) else rep(NA_real_, length(idx))
  probability <- if ("FDR" %in% names(d)) d$FDR else if ("PValue" %in% names(d)) d$PValue else rep(NA_real_, length(idx))
  if (!one_effect) effects[] <- NA_real_
  list(table = table, total = n, coefficient = selected, one_effect = one_effect,
    notes = c(notes, adjustment, if (!one_effect) "The saved test is multi-parameter; a single-coefficient volcano/MA view is omitted to avoid associating an omnibus P value with one effect."),
    data = data.frame(feature = selected_ids, index = idx, effect = effects, mean = mean, probability = probability),
    effect_label = if (kind == "MArrayLM") "Saved coefficient / contrast" else "Saved log2 fold change",
    probability_label = if (saved_fdr) "Saved FDR" else if ("FDR" %in% names(d)) "BH FDR" else "Unadjusted P value", mean_label = if (length(mean_col)) mean_col[1L] else "Average expression")
}

rr_bio_result_choices <- function(x, kind, limits, bio_coef = NULL, result = NULL) {
  if (kind %in% c("enrichResult", "gseaResult", "compareClusterResult")) {
    slot <- if (kind == "compareClusterResult") "compareClusterResult" else "result"
    d <- methods::slot(x, slot)
    if (!nrow(d)) return(character())
    c("Enrichment dot plot" = "enrichment_dot", "Leading saved terms" = "enrichment_bar")
  } else {
    r <- if (is.null(result)) rr_bio_result_data(x, kind, limits, bio_coef, complete = TRUE) else result
    d <- r$data
    c(if (any(is.finite(d$effect) & is.finite(d$probability))) c("Volcano plot" = "volcano"),
      if (any(is.finite(d$effect) & is.finite(d$mean))) c("MA plot" = "ma"),
      if (any(is.finite(d$probability))) c("P / FDR distribution" = "probability"))
  }
}

rr_bio_table_choices <- function(s) {
  c("Observations", "Features", "Sources", if (length(s$layers)) "Layers",
    if (length(s$all_reductions)) "Reductions", if (length(s$reductions)) "Saved coordinates",
    if (length(s$assays) && s$n_cells && s$n_features) "Expression window",
    if (!is.null(s$spatial)) "Spatial coordinates")
}

rr_bio_container_tables <- function(s, limits, requested = NULL) {
  choices <- rr_bio_table_choices(s)
  if (is.null(requested)) requested <- choices
  if (!all(requested %in% choices)) stop("Unknown result table for this source.", call. = FALSE)
  tables <- list()
  cols <- rr_sample_indices(s$n_cells, min(limits$max_rows, limits$max_plot_points, limits$max_cells %/% max(2L, min(if (is.null(s$meta)) 2L else ncol(s$meta) + 1L, limits$max_cols))))
  rows <- seq_len(min(s$n_features, limits$max_rows))
  if ("Observations" %in% requested) {
    meta <- rr_bio_frame(s$meta, s$meta_index(cols), limits,
      if (is.null(s$meta)) s$cells else if (s$kind == "Seurat") rownames(s$meta) else s$cells, "observation")
    if (!is.null(s$identity) && nrow(meta) && ncol(meta) < limits$max_cols) meta[[utils::tail(make.unique(c(names(meta), "Saved_identity")), 1L)]] <- as.character(s$identity[s$cells[cols]])
    attr(meta, "rresult_scope") <- paste("Annotation preview:", nrow(meta), "of", s$n_cells,
      "observations. Columns follow the configured preview budget. CSV exports this preview; it does not export the entire container.")
    tables$Observations <- meta
  }
  if ("Features" %in% requested) {
    data <- if (is.function(s$feature_data_get)) s$feature_data_get() else s$feature_data
    seurat <- s$kind %in% c("Seurat", "Assay", "Assay5")
    f_index <- if (!is.null(data) && seurat) match(s$features[rows], rownames(data)) else rows
    tables$Features <- rr_bio_frame(data, f_index, limits,
      if (is.null(data)) s$features else if (seurat) rownames(data) else s$features, "feature")
    attr(tables$Features, "rresult_scope") <- paste("Feature preview:", nrow(tables$Features), "of", s$n_features,
      "features. Columns follow the configured preview budget. CSV exports this preview.")
  }
  if ("Sources" %in% requested) tables$Sources <- data.frame(assay = utils::head(s$assays, limits$max_children), stringsAsFactors = FALSE)
  if ("Layers" %in% requested) tables$Layers <- data.frame(layer = utils::head(s$layers, limits$max_children))
  if ("Reductions" %in% requested) tables$Reductions <- data.frame(name = utils::head(s$all_reductions, limits$max_children))
  if ("Saved coordinates" %in% requested) {
    coordinate_limits <- limits; coordinate_limits$max_plot_points <- min(limits$max_plot_points, limits$max_rows)
    coordinates <- tryCatch(rr_bio_coordinates(s, "embedding", coordinate_limits), error = function(e) NULL)
    if (!is.null(coordinates)) tables$`Saved coordinates` <- coordinates$data
  }
  if ("Expression window" %in% requested) {
    nr <- min(10L, limits$max_rows, s$n_features, limits$max_cells, limits$max_plot_points)
    nc <- min(8L, limits$max_cols, s$n_cells, floor(min(limits$max_cells, limits$max_plot_points) / nr))
    values <- rr_bio_slice(s, seq_len(nr), seq_len(nc), limits)
    d <- as.data.frame(values, optional = TRUE)
    names(d) <- make.unique(rr_text(s$cells[seq_len(nc)], 60L))
    tables$`Expression window` <- data.frame(feature = rr_text(s$features[seq_len(nr)]), d, check.names = FALSE)
  }
  if ("Spatial coordinates" %in% requested) {
    data <- as.data.frame(s$spatial[cols, , drop = FALSE])
    tables$`Spatial coordinates` <- rr_bio_frame(data, seq_along(cols), limits, s$cells[cols], "observation")
  }
  for (name in intersect(c("Sources", "Layers", "Reductions", "Expression window", "Spatial coordinates"), names(tables))) {
    attr(tables[[name]], "rresult_scope") <- paste(name,
      "is a bounded source preview. CSV exports the stated preview, including all its browser pages.")
  }
  tables
}

rr_bio_container_report <- function(x, kind, limits, assay = NULL, layer = NULL, source = NULL, tables = NULL) {
  s <- if (is.null(source)) rr_bio_source(x, limits, assay, layer) else source
  requested <- tables
  tables <- rr_bio_container_tables(s, limits, requested)
  extra <- if (kind %in% c("SingleCellExperiment", "SpatialExperiment")) {
    alternatives <- SingleCellExperiment::altExpNames(x)
    if (length(alternatives)) paste("Alternative experiments saved:", paste(utils::head(alternatives, limits$max_children), collapse = ", "), "; these are not merged into the selected assay.")
  }
  report <- rr_new_report(kind, "Explore saved expression and annotations", paste(kind, "|", s$assay, "|", s$layer),
    list(Features = s$n_features, Observations = s$n_cells, Assays = length(s$assays), "Saved reductions" = length(s$all_reductions)),
    c(paste("Selected expression source:", s$assay, "/", s$layer), "Assay windows are sliced before dense conversion; saved embeddings and observation annotations are read on demand.", extra),
    tables, rr_bio_container_choices(s),
    c("Gene/feature IDs are matched exactly. Bioconductor rows are features and columns are observations; Seurat layer-specific cells are aligned by barcode.",
      "Expression values retain the selected assay/layer scale; counts, normalized values and centered values are not interchanged. No normalization, PCA, UMAP, clustering or marker testing is run.",
      "Saved coordinate maps show every observation with available coordinates. Gene maps and gene distributions use all observations in the selected expression source. Composition and grouped gene summaries use all observations, with expression read in bounded blocks.",
      "Expression heatmaps and result tables remain bounded previews. Blank feature selection uses the first 6 genes for grouped summaries, 20 for heatmaps and 1 for single-gene views.",
      if (kind == "SpatialExperiment") "Spatial plots use saved physical coordinates without image overlays or automatic axis inversion.",
      "CSV exports the selected bounded table; chart styles and PNG/PDF/HTML exports are shared with other reports."), limits)
  if (!is.null(requested)) report$table_choices <- rr_bio_table_choices(s)
  report
}

rr_bio_report <- function(x, kind, limits, assay = NULL, layer = NULL, bio_coef = NULL) {
  if (kind %in% .rr_bio_containers) return(rr_bio_container_report(x, kind, limits, assay, layer))
  r <- rr_bio_result_data(x, kind, limits, bio_coef, complete = TRUE, include_table = TRUE)
  if (kind %in% c("enrichResult", "gseaResult", "compareClusterResult")) {
    d <- r$source
    settings <- intersect(methods::slotNames(x), c("organism", "ontology", "setType", "pvalueCutoff", "pAdjustMethod", "qvalueCutoff", "fun"))
    setting <- data.frame(setting = settings, value = vapply(settings, function(nm) rr_ggplot_text(methods::slot(x, nm)), character(1)))
    return(rr_new_report(kind, "Inspect saved enrichment results", kind,
      list("Stored term rows" = r$total, "Stored groups" = if ("Cluster" %in% names(d)) length(unique(d$Cluster)) else 1L),
      c("Term statistics, gene ratios and enrichment scores are read from the saved result table.", "Charts rank the stored rows and do not rerun enrichment or query annotation services."),
      list(`Saved terms` = r$table, Settings = setting), rr_bio_result_choices(x, kind, limits),
      c("The table shows stored result rows; saved significance cutoffs are reported without silently reapplying an additional filter.",
        "Dot/bar charts display leading terms. Gene ratios are parsed as numeric fractions; GSEA charts use saved NES/set sizes, not newly calculated running-score curves.",
        "For comparison objects, term selection is bounded within each displayed cluster; groups outside max_children are omitted explicitly."), limits))
  }
  rr_new_report(kind, if (kind == "MArrayLM") "Inspect saved gene-wise model coefficients" else "Inspect saved differential-expression results", if (is.null(r$coefficient)) kind else r$coefficient$name,
    list("Stored genes" = r$total, "Reported genes" = nrow(r$table)),
    r$notes, list(`Gene results` = r$table), rr_bio_result_choices(x, kind, limits, bio_coef, result = r),
    c(r$notes, "Display P/FDR and absolute log2-fold-change cutoffs affect colours only; stored estimates, test statistics and model fits remain unchanged.",
      "Volcano, MA and probability charts use all saved gene rows; non-finite values are omitted where required by the plotted axes. Saved gene-result tables and CSV exports retain all gene rows."), limits)
}




