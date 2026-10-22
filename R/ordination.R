# Saved ordinations: methods, coordinates and explicit sample metadata --------

.rr_ordination_families <- c("prcomp", "princomp", "PCA", "metaMDS", "monoMDS", "pcoa", "wcmdscale", "rda", "cca")

rr_ordination_kind <- function(x) {
  cls <- attr(x, "class", exact = TRUE)
  for (kind in setdiff(.rr_ordination_families, c("PCA", "rda"))) if (identical(cls, kind)) return(kind)
  if (identical(cls, c("rda", "cca"))) return("rda")
  if (identical(cls, c("PCA", "list"))) return("PCA")
  if (identical(cls, c("metaMDS", "monoMDS"))) return("metaMDS")
  NULL
}

rr_ordination_matrix <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.matrix(x) || !is.numeric(x) || is.complex(x)) stop("This saved ordination has invalid numeric coordinates.", call. = FALSE)
  x
}

rr_ordination <- function(x, kind = rr_ordination_kind(x)) {
  out <- list(kind = kind, method = "PCA", scores = NULL, variables = NULL,
    eigen = numeric(), percent = numeric(), variable_label = "Variable loadings", correction = "None",
    negative = 0L, stress = NULL, engine = NULL, shepard = FALSE)
  if (kind %in% .rr_constrained_families) {
    constrained <- !is.null(x$CCA) && length(x$CCA$eig) > 0L
    block <- if (constrained) x$CCA else x$CA
    out$method <- toupper(kind)
    out$coordinate_set <- if (constrained) "Constrained weighted-average scores" else "Unconstrained saved scores"
    out$scores <- if (!is.null(block$wa)) block$wa else block$u
    out$variables <- block$v
    out$environment <- if (constrained) block$biplot else NULL
    out$eigen <- block$eig
    out$percent <- if (x$tot.chi > 0) 100 * out$eigen / x$tot.chi else rep(NA_real_, length(out$eigen))
    out$axis_names <- names(out$eigen)
    out$variable_label <- "Saved species coordinates (scaling 0)"
  } else if (kind %in% c("prcomp", "princomp", "PCA")) {
    if (kind == "PCA") {
      out$scores <- x$ind$coord; out$variables <- x$var$coord
      out$eigen <- x$eig[, 1L]; out$percent <- x$eig[, 2L]
      out$variable_label <- "Active variable coordinates"
      out$scaled <- isTRUE(x$call$scale.unit)
    } else {
      out$scores <- if (kind == "prcomp") x$x else x$scores
      out$variables <- if (kind == "prcomp") x$rotation else unclass(x$loadings)
      out$eigen <- x$sdev^2
      out$percent <- if (sum(out$eigen) > 0) 100 * out$eigen / sum(out$eigen) else rep(NA_real_, length(out$eigen))
      out$scaled <- if (kind == "prcomp") is.numeric(x$scale) else any(x$scale != 1)
    }
  } else if (kind %in% c("metaMDS", "monoMDS")) {
    out$method <- if (identical(x$model, "linear")) "Linear MDS" else if (identical(x$model, "hybrid")) "Hybrid MDS" else "NMDS"
    out$scores <- x$points
    # metaMDS stores NA when weighted-average species scores were not computed.
    out$variables <- if (kind == "metaMDS" && identical(x$species, NA)) NULL else x$species
    out$variable_label <- "Saved species coordinates"
    out$stress <- x$stress
    out$engine <- if (kind == "monoMDS" || inherits(x, "monoMDS")) "monoMDS" else x$engine
    out$shepard <- length(x$diss) > 0L && length(x$diss) == length(x$dist) && length(x$diss) == length(x$dhat)
  } else if (kind %in% c("pcoa", "wcmdscale")) {
    out$method <- "PCoA"
    if (kind == "pcoa") {
      corrected <- !is.null(x$vectors.cor)
      out$scores <- if (corrected) x$vectors.cor else x$vectors
      out$eigen <- if (corrected) x$values$Corr_eig else x$values$Eigenvalues
      out$correction <- if (corrected) as.character(x$correction[1L]) else "None"
      original <- x$values$Eigenvalues
    } else {
      out$scores <- x$points; out$eigen <- x$eig
      out$correction <- if (is.character(x$add)) x$add else "None"
      original <- x$eig
    }
    tolerance <- if (length(original)) max(abs(original)) * 1e-8 else 0
    out$original_negative <- sum(original < -tolerance)
    out$negative <- if (length(out$eigen)) sum(out$eigen < -max(abs(out$eigen)) * 1e-8) else 0L
    positive <- out$eigen[out$eigen > if (length(out$eigen)) max(abs(out$eigen)) * 1e-8 else 0]
    out$percent <- if (length(positive)) 100 * positive / sum(positive) else numeric()
  }
  out$scores <- rr_ordination_matrix(out$scores)
  out$variables <- rr_ordination_matrix(out$variables)
  out$axes <- max(c(0L, ncol(out$scores), ncol(out$variables)))
  out$observations <- if (!is.null(out$scores)) nrow(out$scores) else if (kind == "princomp") x$n.obs else NA_integer_
  out
}

rr_ordination_choices <- function(o) {
  scores <- !is.null(o$scores) && ncol(o$scores) >= 2L && nrow(o$scores) > 0L
  variables <- !is.null(o$variables) && ncol(o$variables) >= 2L && nrow(o$variables) > 0L
  if (o$kind %in% .rr_constrained_families) return(c(if (scores) c("Sample map" = "scores"),
    if (scores && variables) c("Samples and species arrows" = "biplot"),
    if (scores && !is.null(o$environment) && ncol(o$environment) >= 2L) c("Samples and explanatory-variable arrows" = "environment"),
    if (variables) c("Saved species map" = "species"),
    if (length(o$eigen)) c("Saved axis eigenvalues" = "eigenvalues")))
  if (o$method == "PCA") return(c(if (scores) c("Sample map" = "scores"),
    if (scores && variables) c("Samples and variable arrows" = "biplot"),
    "Explained variance" = "scree", if (variables) stats::setNames("loadings", o$variable_label),
    if (!is.null(o$variables) && length(o$variables)) c("Variable coordinate heatmap" = "loadings_heatmap")))
  c(if (scores) c("Sample map" = "scores"),
    if (variables && o$method != "PCoA") c("Saved species map" = "species"),
    if (o$shepard) c("Shepard fit diagnostic" = "shepard"),
    if (length(o$eigen)) c("Eigenvalues, including negative axes" = "eigenvalues"))
}

rr_ordination_table <- function(m, limits, label = "sample") {
  if (is.null(m)) return(data.frame())
  idx <- seq_len(nrow(m))
  axes <- seq_len(ncol(m))
  nm <- rownames(m)
  d <- data.frame(id = if (is.null(nm)) as.character(idx) else nm[idx], stringsAsFactors = FALSE)
  names(d) <- label
  if (length(axes)) d <- cbind(d, m[idx, axes, drop = FALSE])
  d
}

# Matching uses original identifiers; labels are truncated only after matching.
rr_ordination_metadata <- function(metadata, sample_id, scores, index, limits, preview = TRUE) {
  if (is.null(metadata)) {
    if (!is.null(sample_id)) stop("sample_id requires metadata.", call. = FALSE)
    return(list(data = NULL, note = "No sample metadata supplied."))
  }
  if (!identical(rr_report_kind(metadata), "data_frame")) stop("metadata must be a standard data frame or tibble.", call. = FALSE)
  if (is.null(scores)) stop("Sample metadata requires saved sample scores.", call. = FALSE)
  if (rr_df_nrow(metadata) > limits$max_name_scan || nrow(scores) > limits$max_name_scan)
    stop("Sample matching exceeds max_name_scan; increase the explicit limit.", call. = FALSE)
  plain <- rr_plain(metadata)
  ids <- rownames(scores)
  if (identical(sample_id, ".position")) {
    if (rr_df_nrow(metadata) != nrow(scores)) stop("Positional metadata must have one row per saved sample.", call. = FALSE)
    rows <- index; note <- "Metadata matched by row position, explicitly requested with sample_id = '.position'."
  } else {
    if (is.null(ids) || anyNA(ids) || anyDuplicated(ids) || any(!nzchar(ids)))
      stop("Saved sample names must be unique for metadata matching; use sample_id = '.position' for explicit positional matching.", call. = FALSE)
    if (is.null(sample_id)) source <- rownames(metadata) else {
      if (!is.character(sample_id) || length(sample_id) != 1L || is.na(sample_id) ||
          sum(names(plain) == sample_id) != 1L) stop("sample_id must name one unique metadata column, or be '.position'.", call. = FALSE)
      source <- plain[[sample_id]]
      if (!rr_simple(source) || !is.null(dim(source))) stop("The sample ID column must be a simple vector.", call. = FALSE)
      source <- as.character(source)
    }
    if (anyNA(source) || anyDuplicated(source) || any(!nzchar(source))) stop("Metadata sample IDs must be nonmissing and unique.", call. = FALSE)
    rows <- match(ids, source)
    if (anyNA(rows)) stop("Metadata is missing IDs for saved samples; supply matching sample IDs.", call. = FALSE)
    rows <- rows[index]
    note <- paste("Metadata matched by sample name using", if (is.null(sample_id)) "metadata row names." else paste0("column '", sample_id, "'."))
  }
  cols <- if (preview) seq_len(min(length(plain), limits$max_cols,
    floor(limits$max_cells / max(1L, length(index))))) else seq_along(plain)
  simple <- vapply(plain[cols], function(v) rr_simple(v) && is.null(dim(v)), logical(1))
  d <- lapply(plain[cols][simple], function(v) v[rows])
  d <- as.data.frame(d, check.names = FALSE, stringsAsFactors = FALSE)
  if (!ncol(d)) d <- data.frame(row.names = seq_along(index))
  result <- list(data = d, note = note)
  if (preview) rr_budget(result, limits) else result
}

rr_ordination_report <- function(x, kind, limits, metadata = NULL, sample_id = NULL) {
  if (kind %in% .rr_constrained_families) return(rr_constrained_report(x, kind, limits, metadata, sample_id))
  o <- rr_ordination(x, kind)
  plots <- rr_ordination_choices(o)
  samples <- rr_ordination_table(o$scores, limits)
  metrics <- list(Method = o$method, Samples = o$observations, "Stored axes" = o$axes)
  notes <- c("The saved analysis is displayed without refitting or re-running transformations.",
    "Sample maps preserve equal X/Y units. Nearby samples have similar coordinates in the displayed projection; visual separation alone is not a significance test.",
    "Sample maps retain all finite saved coordinates. Result tables retain saved rows and columns; labels and variable overlays are limited for readability. Axis signs and rotation can differ between valid fits.")
  tables <- list(Samples = samples)
  if (o$method == "PCA") {
    metrics$Variables <- nrow(o$variables)
    metrics$`PC1 + PC2 (%)` <- sum(utils::head(o$percent, 2L))
    tables$Variance <- data.frame(component = paste0("PC", seq_along(o$eigen)), sd = sqrt(o$eigen),
      eigenvalue = o$eigen, variance_percent = o$percent, cumulative_percent = cumsum(o$percent))
    tables$Loadings <- rr_ordination_table(o$variables, limits, "variable")
    title <- "Which samples and variables vary together?"
    highlights <- c(paste("The first two components retain", rr_fmt(sum(utils::head(o$percent, 2L))), "percent of the full fitted variance."),
      paste("Variables were", if (o$scaled) "scaled before fitting." else "not scaled to unit variance."))
    notes <- c(notes, "Arrow overlays multiply all displayed variable coordinates by one stated visual scale factor; their lengths are not on the sample-score scale.",
      if (kind == "PCA") "FactoMineR views use active individuals and active variable coordinates, not eigenvector loadings. Supplementary individuals/variables are not mixed into the active maps.",
      if (is.null(o$scores)) "Scores were not saved; sample maps and sample metadata are unavailable.")
  } else if (o$method == "PCoA") {
    metrics$Correction <- o$correction
    metrics$`Negative eigenvalues` <- if (kind == "wcmdscale" && !length(o$eigen)) NA_integer_ else o$negative
    if (kind == "pcoa") metrics$`Original negative eigenvalues` <- o$original_negative
    tables$Eigenvalues <- data.frame(axis = seq_along(o$eigen), eigenvalue = o$eigen)
    if (kind == "pcoa" && !is.null(x$vectors.cor)) tables$`Original eigenvalues` <- data.frame(axis = seq_along(x$values$Eigenvalues), eigenvalue = x$values$Eigenvalues)
    title <- "Explore sample differences from the saved distances"
    highlights <- c(paste("Coordinate source:", if (kind == "pcoa" && !is.null(x$vectors.cor)) "saved corrected coordinates." else "saved principal coordinates."),
      paste("Axis percentages use the sum of positive eigenvalues; correction:", o$correction))
    notes <- c(notes, "PCoA percentages describe positive eigenvalue inertia, not PCA explained variance. Negative axes are shown separately in the eigenvalue chart; a 2D map cannot preserve their geometry.",
      "A stored distance correction is retained; no new correction is applied.",
      if (!length(o$eigen)) "Eigenvalues were not saved; axis percentages are unavailable.")
  } else {
    metrics$Stress <- o$stress
    metrics$Engine <- if (is.null(o$engine)) "Not recorded" else o$engine
    title <- "How well does the sample map represent the dissimilarities?"
    highlights <- c(paste("Saved", o$method, "stress:", rr_fmt(o$stress),
      if (identical(o$engine, "isoMDS")) "(percent scale)." else "(proportion scale)."),
      if (kind == "metaMDS" && !is.null(x$converged)) paste("Repeated best-solution matches:", x$converged, "across", x$tries, "random starts."))
    if (!is.null(o$variables)) tables$Species <- rr_ordination_table(o$variables, limits, "species")
    notes <- c(notes, "NMDS axes have no PCA-style explained-variance percentages. Stress describes the whole saved configuration, including axes not shown in a selected 2D projection.",
      "Lower stress indicates closer fit only when comparing the same data, engine and stress definition. It is not a P value or evidence of significant group differences.",
      "The Shepard diagnostic uses stored engine dissimilarities, distances and fitted disparities; it does not reconstruct or refit the original distances.",
      if (kind == "metaMDS" && isTRUE(x$converged == 0)) "The best solution was not repeated. Inspect the original fit and consider more random starts before interpretation.",
      if (isTRUE(x$icause == 1)) "The engine reached its iteration limit; inspect the fit before interpreting the map.",
      if (!is.null(o$variables)) "Species locations are saved weighted-average coordinates; they are not PCA loadings or significance tests." else
        "Species coordinates were not saved; the species map is unavailable.")
  }
  if (!is.null(metadata)) {
    if (is.null(o$scores)) stop("Sample metadata requires saved sample scores.", call. = FALSE)
    idx <- seq_len(nrow(o$scores))
    matched <- rr_ordination_metadata(metadata, sample_id, o$scores, idx, limits, preview = FALSE)
    tables$Metadata <- cbind(samples["sample"], matched$data)
    notes <- c(notes, matched$note)
  }
  if (is.null(metadata) && !is.null(sample_id)) stop("sample_id requires metadata.", call. = FALSE)
  rr_new_report(kind, title, paste(o$method, "|", kind, "| saved analysis"), metrics, highlights, tables, plots, notes, limits)
}
