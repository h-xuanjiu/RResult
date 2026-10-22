# RResult 0.1.0 (2026-10-23)

## Development updates (version retained pending release)

* Standardized the GPL-3.0-or-later declaration with usethis. The repository
  keeps the full license in LICENSE.md, excluded from the source package.

* Fixed mean/SE error bars shifting away from their group means when another
  colour group has only one observation and no standard error.
* Bioconductor containers now support unnamed or blank-named assays by index
  and generated display labels. Source selection remains lazy and preserves
  existing assay names and original positions.
* Mixed-model reports now use the fitted model's observation count, excluding
  missing rows restored by na.exclude in fitted/residual diagnostics.

* Removed bundled reference-only example scripts and their documentation links.
  Usage examples remain in the README and public help pages.
* Removed two unused internal presentation helpers.

* Explorer charts now use the selected data-frame columns, replacing the
  independent single-column index control. Numeric box/violin plots compare
  columns or use separate panels; other distributions and mixed-type automatic
  charts display each variable separately. All source rows are retained per
  column. Chart pages make every selected variable accessible, six at a time.
* Moved explorer plots before collapsed data tables and statistics. Table
  paging preserves existing charts. Changing the column selection invalidates
  stale plots, actual variable names and row scope are displayed, and lazy
  column statistics use the same complete data as the charts.

* Added batch row/column pickers to the object explorer. Data-frame columns
  and dense/sparse matrix axes support searchable, paged checkboxes, selection
  across pages, select-page/select-matches actions and cancel/apply. Exact
  positions distinguish duplicate or unnamed columns. Selection limits are
  explicit and rejected batches keep the previous selection; only one page of
  labels is sent to the browser. Query previews and CSV use the chosen positions.

* Fixed an empty-output startup deadlock shared by report families: chart
  controls and auto-height plots could have zero initial height and be
  suspended as hidden indefinitely. Reports now initialize those outputs,
  reserve a visible plot area, and wait for their chart inputs before drawing.
  Result, interpretation and explorer pages no longer trigger hidden charts.
* Reused native lm summaries, data selections and full-family gene statistics;
  default explicit discrete palettes no longer precompute the plot's statistics.
  Plot-choice queries for bio/ordination objects avoid building result tables
  or reading expression windows. Complete observation/result scope is retained.
  The unused broom runtime dependency is removed; it remains a development
  reference for native-result compatibility tests.

* Removed implicit sampling and leading-category/panel cutoffs from ordinary
  vector/data-frame charts. All supported columns are selectable. Counts and
  proportions aggregate observed combinations without a dense Cartesian grid.
* Saved statistical result tables retain every row and column in R. Browser
  pages and column windows limit rendering only; Table CSV exports the entire
  selected result table. Expression/container/source-data previews explicitly
  state their narrower export scope. CSV preserves 17-digit numeric values.
* Added arbitrary vector/array index windows, previous/next query navigation,
  searchable full column pickers, query-window CSV and expandable long cells.
  Literal data-frame search now checks complete cell text.
* Removed core saved-chart cutoffs, including time-series positions/series,
  cluster centers, model terms and ordination eigenvalues. Ordination metadata
  fields and axes are fully selectable, including Axis 1 on Y. Long survival
  stratum names no longer merge after display-label shortening.

* Saved UMAP/PCA/spatial and ordination sample maps now show all available
  finite coordinates. Seurat embedding maps no longer inherit the cell subset
  of a selected split expression layer. Metadata remain matched by cell ID.
  Gene maps/distributions and grouped summaries read all selected-source cells
  in bounded expression blocks; composition/metadata histograms use all cells.
  Volcano/MA/probability plots, ROC and survival curves retain all saved rows.
  Heatmap/table previews and label/variable overlays keep their display bounds.
  Blank gene-selection defaults are now explained beside the input.

* Bioinformatics workspaces now share one selected source between reports,
  chart controls and plots, defer feature/expression tables until requested,
  and reuse the last bounded expression window and coordinate/name lookups.
  Initial controls no longer trigger an expression preview or a premature
  default chart. Dimension checks avoid realizing unselected reductions.
  Public full reports, chart data, CSV and complete HTML exports are retained.
  No new dependencies were added.

* Reduced Suggests to optional label repulsion (ggrepel) and Enhances to 17
  packages used by external-object views. Removed declarations needed only
  to generate test/example objects. The complete development tests remain
  in the checkout and are excluded from source releases. Core help examples
  remain runnable.
  All 58 object families and runtime implementations are unchanged.
* Removed unused knitr and rmarkdown and the development-only roxygen2
  declaration. Generated public help files remain included.
* Seurat views now explicitly check the minimum SeuratObject version at use
  time, since version constraints in Enhances are not enforced by R.

* Added 16 bioinformatics report families (58 total): Seurat/Assay/Assay5,
  SummarizedExperiment/RangedSummarizedExperiment, SingleCellExperiment,
  SpatialExperiment, DESeqDataSet/DESeqTransform, edgeR DGEList/DGEExact/DGELRT,
  limma MArrayLM, enrichResult/gseaResult/compareClusterResult.
* Added explicit assay/layer, saved reduction, feature, metadata and contrast
  selectors; expression maps/windows/distributions/dot summaries, physical
  coordinate maps, differential-expression views and ranked enrichment views.
* Split Seurat layers align metadata/embeddings by barcode. Sparse/delayed/HDF5
  assays are sliced before dense conversion. Any new BH FDR calculation uses
  the full stored gene family before sampling; omnibus tests keep their scope.
  All biology dependencies are optional and version remains 0.1.0.

* Added a persistent Style settings dialog with nine themes, nine palettes,
  custom group and gradient colours, 26 point shapes, larger size ranges,
  separate font sizes, axis/tick/grid/border/background and layout controls.
* Added coordinate-aware automatic canvas height, optional Cartesian canvas
  filling, size-aware heading wrapping, and larger canvas/export dimensions.
  Equal coordinate ratios are retained by default. PNG/PDF/HTML share styling.
* Imported ggplots still remain unchanged by default; explicit palette/colour
  overrides retain scale transformations, limits, breaks and labels.

* Added nine report families: vegan anova.cca, rda, cca, betadisper and envfit;
  MASS negbin; mgcv gam (including bam); pROC roc; and DESeq2 DESeqResults.
  The inventory now contains 42 dedicated families, with version held at 0.1.0.
* Added centered GAM smooth curves with bounded native-basis predictions,
  saved scaling-0 constrained ordination maps, negative-binomial inference,
  ROC threshold views, and differential-expression volcano/MA views.
  New classes share the existing chart studio and CSV/PNG/PDF/HTML exports.

* Fixed GLM residual plots with na.exclude so fitted values and deviance
  residuals remain paired, including when diagnostic points are sampled.
* Fixed metadata and workspace startup for row-compressed pattern sparse
  matrices, including ngRMatrix.
* Category summaries now count full sampled labels before shortening display
  labels, retaining distinct categories and missing values.

* Ordination colour grouping is now visible directly below the chart selector
  when metadata is supplied, with a metadata-ready message. Shape, panel and
  outline settings are separate from sample labels.

* Added dedicated princomp, FactoMineR PCA, vegan metaMDS/monoMDS and
  ape/vegan PCoA reports, extending the inventory from 27 to 33 families.
* PCA now opens a sample map, with variable-arrow overlays and coordinate
  heatmaps. Ordination maps accept explicitly matched sample metadata for
  colours, shapes, panels, bounded sample labels and descriptive group hulls.
* NMDS retains saved stress and engine-scale Shepard pairs. PCoA uses saved
  corrected coordinates and reports negative eigenvalues and its percentage
  denominator. Unclassed coordinate matrices/lists are not guessed as models.

* Added 13 standard report families: saved lm/glm summaries, anova tables,
  Tukey/pairwise/power results, survival curves and models, lme4/nlme mixed
  models and emmeans grids/summaries. The inventory now contains 27 families.
* Standard data.table, grouped_df and rowwise_df objects receive data reports,
  including saved group sizes. Optional modelling dependencies are declared separately.
* Added categorical colour groups, bounded faceting, explicit axis category
  roles, frequency polygons, fitted linear trends, 2D bins, mean/SE and count bubbles.
* Chart studio now has separate scrollable controls, an expandable canvas,
  a hide-settings action, responsive layout and independent canvas/export sizes.
* Added vector PDF exports, seven palettes, fixed colours, label editing,
  orientation, tick angles and grid controls. Mapped point sizes remain mapped.
* Preserved ANOVA term names and original comparison adjustments; survival plots
  retain stored confidence bands, censoring and exact-time risk snapshots.

* Added standard ggplot objects as the 14th report family: original rendering,
  layer/mapping/source-data inspection, opt-in appearance edits and exports.
  Existing colours, scales, facets and mapped aesthetics are preserved by default.

* Added type-aware scatter, ordered line, box, violin + box, individual-point,
  ECDF, count, proportion, category heatmap and explicit summary-bar charts.
* Chart appearance controls and the style argument now cover themes, palettes,
  fonts, points, lines, titles and legends, including exported plots.
* Added standard table/xtabs, dist, hclust, density and nls reports. The inventory
  includes these families, available through rresult_supported().
* Added rresult_plot_choices() for compatible chart suggestions; categorical
  codes are not inferred from arbitrary numeric columns.

* Visual reports now open with result cards, highlights and an immediately visible
  chart; report tables and interpretation have their own views.
* Added GLM, ANOVA, hypothesis-test, PCA, k-means and time-series reports.
* Data-frame/tibble reports include sampled quality profiles, correlations and
  selectable numeric relationships. Atomic summaries use formatted cards/tables.
* Reports can download the displayed table as CSV, chart as PNG and a standalone
  offline HTML report with an embedded chart. Added rresult_report() for reuse.
* Declared tibble as a suggested dependency used by the tests.

## Initial implementation

* Local, live Shiny workspace with light and dark themes.
* Conservative, lazy object tree and bounded data providers.
* Dense and sparse matrix windows, exact name selection, and local heatmaps.
* Server-side data-frame range search, sorting, and pagination.
* Sampled vector distributions and a linear-model adapter.
* Public adapter registration for trusted third-party extensions.


