#' List the standard object families with dedicated reports
#' @return A data frame of supported families, standard class signatures and
#'   principal report views. Other known R objects retain generic exploration.
#' @examples
#' rresult_supported()
#' @export
rresult_supported <- function() {
  core <- data.frame(family = c("lm", "glm", "aov", "htest", "prcomp", "kmeans", "ts", "data_frame", "table", "dist", "hclust", "density", "nls", "ggplot"),
    classes = c("lm", "glm/lm", "aov/lm", "htest", "prcomp", "kmeans", "ts; mts", "data.frame; tbl_df/tbl/data.frame",
      "table; xtabs/table", "dist", "hclust", "density", "nls", "ggplot2 standard S3/S7 ggplot"),
    views = c("Fit, coefficients, diagnostics", "Family/link, coefficients, response diagnostics", "Sequential ANOVA, contributions, diagnostics",
      "Estimates, uncertainty, test quantities", "Variance, scores, loadings", "Cluster sizes and centers", "Trends and ACF",
      "Type-aware charts, quality, correlations", "Stored counts/weights, heatmap, proportions", "Bounded pairwise windows and distributions",
      "Dendrogram, merge heights, cut-tree sizes", "Saved density grid", "Parameters, uncertainty, fit and residuals",
      "Original chart, layers, mappings, source-data previews and optional appearance edits"), stringsAsFactors = FALSE)
  core$classes[core$family == "data_frame"] <- "data.frame; tibble; data.table; grouped_df; rowwise_df"
  core <- rbind(core, data.frame(family = .rr_bio_families,
    classes = paste(.rr_bio_families, "(standard package-defined S4 class)"),
    views = c(rep("Saved assay/layer windows, metadata, gene distributions and available saved/spatial maps", length(.rr_bio_containers)),
      rep("Saved differential-expression tables, full-family BH FDR, volcano/MA and available probabilities", 3L),
      rep("Stored enrichment tables, ranked term dot/bar charts and comparison groups", 3L)), stringsAsFactors = FALSE))
  rbind(core, data.frame(family = .rr_first_families,
    classes = c("summary.lm", "summary.glm", "anova/data.frame", "TukeyHSD/multicomp", "pairwise.htest", "power.htest",
      "survfit; survfitcox/survfit", "coxph", "survreg", "lmerMod; glmerMod; lmerModLmerTest", "lme", "emmGrid", "summary_emm/data.frame"),
    views = c("Saved coefficients and residuals", "Saved link-scale coefficients and deviance residuals", "Original analysis columns and tests",
      "Simultaneous contrasts and adjusted P values", "Adjusted P-value heatmap", "Saved power calculation",
      "Survival, confidence bands, censoring and risk snapshots", "Hazard ratios and fitted model statistics", "Distribution, parameters and uncertainty",
      "Fixed effects, variance components and conditional diagnostics", "Fixed effects, variance components and conditional diagnostics",
      "Marginal means, contrasts and adjusted inference", "Saved marginal estimates, scale and adjustment"), stringsAsFactors = FALSE),
    data.frame(family = setdiff(.rr_ordination_families, "prcomp"),
      classes = c("princomp", "PCA/list (FactoMineR)", "metaMDS; metaMDS/monoMDS (vegan)", "monoMDS (vegan)", "pcoa (ape)", "wcmdscale (vegan; eig=TRUE or x.ret=TRUE)", "rda/cca (vegan)", "cca (vegan)"),
      views = c("PCA sample maps, variable arrows, variance and coordinate heatmaps",
        "Active PCA samples, variable coordinates, variance and arrow overlays",
        "Sample/species maps, stress and saved Shepard diagnostic", "Sample maps, stress and saved Shepard diagnostic",
        "Saved corrected/original principal coordinates and eigenvalues", "Weighted principal coordinates and saved eigenvalues",
        "Constrained RDA sites, species, environment and inertia", "Constrained correspondence sites, species, environment and inertia"), stringsAsFactors = FALSE),
    data.frame(family = .rr_extended_families,
      classes = c("anova.cca/anova/data.frame (vegan)", "negbin/glm/lm (MASS)", "gam/glm/lm; bam/gam/glm/lm (mgcv)",
        "betadisper (vegan)", "envfit (vegan)", "roc (pROC)", "DESeqResults (DESeq2)"),
      views = c("Saved permutation tests, R-squared, F statistics and inertia", "Theta, coefficients, count ratios and diagnostics",
        "Parametric and smooth terms, one-dimensional smooth curves and diagnostics", "Saved group means and sampled within-group distances",
        "Environmental fit strengths, P values and vector directions", "Saved ROC curve, AUC, AUC intervals and thresholds",
        "Original differential-expression tables, volcano and MA plots"), stringsAsFactors = FALSE))
}

