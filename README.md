# RResult

**Explore data. Choose compatible charts. Read saved statistical results.**

RResult 0.1.0 opens a local Shiny workspace for objects already in R. Use it
to inspect data, compare variable distributions, query matrix windows, and
view statistical results with charts, tables and interpretation notes.
Reports and figures can be exported as CSV, PNG, vector PDF and offline HTML.

Supported statistical objects open a dedicated report. Other objects open the
object explorer, where lists, matrices, S3/S4 objects, functions and environments
can be inspected on demand. `rresult_supported()` lists the 58 dedicated report
families.

## Installation

Requires **R >= 4.1.0**, **Shiny >= 1.8.0** and **ggplot2 >= 3.4.0**.
To install from a local source folder named `RResult`, run the following from
the directory containing that folder:

```r
install.packages(c("shiny", "htmltools", "ggplot2", "jsonlite"))
install.packages("RResult", repos = NULL, type = "source")
```

If your working directory is already the package source directory, use
`install.packages(".", repos = NULL, type = "source")` instead. Installing from
a local source directory does not install missing dependencies automatically.
Restart R after updating an installed version.

Packages used for particular model or bioinformatics objects are optional and
are accessed only when needed. You do not need to install every package listed
in `Enhances`. The optional `ggrepel` package improves label placement.

## Quick start

Each workspace occupies the R prompt until it closes. Close one workspace
before running the next `rresult()` call.

```r
library(RResult)

# A data report with charts, tables and variable-quality information.
rresult(iris)

# Close the workspace, then open a fitted model.
fit <- lm(mpg ~ wt + hp, data = mtcars)
rresult(fit)

# Use the object explorer directly for distributions and table queries.
rresult_explore(mtcars)
```

Use **Close workspace**, close the last connected browser tab, or interrupt R
to return to the prompt. Reports open on **Overview**; **Chart studio** changes
the chart, **Result tables** provides paged values, and **Read the result**
shows interpretation notes. The **Object explorer** remains available.

For a plot or result table without opening the workspace:

```r
rresult_plot(fit, "coefficients")
rresult_report(fit)$tables$Coefficients
rresult_info(mtcars)
```

## Explore data frames

The data-frame report lets you choose X and optional Y variables, categorical
colour groups, panels and compatible chart types. Choose
**None: distribution** for Y to inspect one variable. Use
**Grouping and variable roles** to treat numeric codes as categories explicitly.

The object explorer provides a separate workflow for comparing several columns:

1. Select columns with **Columns: search any name or index**, or open
   **Choose columns in batches**. Columns are identified by position, so
   duplicate names remain distinguishable. The initial selection is at most
   the first 10 columns.
2. The batch picker shows 50 choices per page. **Select page**,
   **Select all matches** and **Apply selection** support selections across
   pages, subject to the configured column limit.
3. Click **Draw selected columns**. Multiple numeric columns default to
   side-by-side box plots; separate plots, histograms, density curves,
   frequency polygons, violin plots and cumulative distributions are also
   available when compatible. Categorical columns use counts or proportions;
   **Automatic** handles mixed supported types.
4. Chart pages contain up to six selected variables. Expand
   **Column statistics and inspected values** to see full-column statistics
   for the variables on the current chart page.
5. Expand **Data table, pages and CSV** to load the first table preview.
   **Load / refresh table** reruns the table query. Under
   **Table search, rows and paging**, set the search range, text, sort column,
   page and page size before using **Query page**.

Explorer distributions and their column statistics use **all source rows**,
independently of table search, sorting and paging. Missing/non-finite values
are excluded where required for the chart; numerical statistics report missing
and non-finite counts separately. Changing columns clears stale charts, while
changing table pages preserves the chart.

Table search and sorting operate within the specified source-row range and
selected columns. The default search range is at most 100,000 rows; advance
**First search row** to browse later rows. Searches are literal and inspect
complete cell values. Long text cells can be expanded. Factor, Date and
POSIXct columns retain meaningful display; POSIXct is shown in UTC.
List, matrix and unfamiliar custom-class columns are described rather than
flattened or evaluated.

Previews show source row positions alongside custom row names. Sorting and
paging preserve this correspondence; CSV retains the original row names.

```r
dat <- transform(mtcars, cyl = factor(cyl), am = factor(am))

page <- rresult_extract(dat, cols = c(1, 6), sort_col = 1, page_size = 5)
page$row_indices  # Source positions.
page$row_labels   # Original row names.
page$data         # Display strings; original column labels are separate.

rresult_plot(dat, "boxplot", axis_x = 2, axis_y = 1)
rresult_plot(dat, "scatter", axis_x = 6, axis_y = 1, colour_by = 2)
rresult_plot(dat, "histogram", axis_x = 1, axis_y = 0)
```

Data scope depends on the view:

| View | Scope |
|---|---|
| Explorer distributions and column statistics | All source rows for each selected column; up to six variables per chart page |
| Data-frame relationship/distribution charts | All rows; incomplete selected values or grouping fields are excluded |
| Explorer table and Query window CSV | The queried page within the selected row range and columns |
| Column types and sampled missingness | An evenly spaced sample of at most `max_plot_points` rows |
| Report Variables table and missingness chart | Sampled rows across the columns |
| Report correlation chart | Sampled rows and at most the first 12 ordinary numeric columns, also subject to cell limits; pairwise-complete Pearson correlations |

## Query dense and sparse matrices

Choose **Starting row**, **Starting column** and their counts, then click
**Query window**. Matrix values are loaded only after a query. Batch row and
column pickers are also available; **Use row range** and **Use column range**
switch back to range selection.

Exact names use one name per line. A nonempty exact-name input replaces the
selection for that axis, selecting every occurrence of duplicated names in
source order. Missing names are reported. The batch picker can distinguish
individual occurrences by position. Literal name search returns a bounded
candidate list and reports truncated scanning. Use the API or numeric positions
for names containing newlines; the interface trims surrounding whitespace.

```r
set.seed(42)
mat <- matrix(rnorm(1000000), nrow = 1000,
  dimnames = list(paste0("row", 1:1000), paste0("col", 1:1000)))

window <- rresult_extract(mat, row_start = 900, n_rows = 20,
                          col_start = 800, n_cols = 20)
window$data
rresult_extract(mat, row_names = c("row900", "not-present"),
                col_names = c("col800", "col801"))
rresult_plot(mat, "heatmap", row_start = 900, n_rows = 20,
             col_start = 800, n_cols = 20, color_range = c(-3, 3))

if (requireNamespace("Matrix", quietly = TRUE)) {
  sparse <- Matrix::rsparsematrix(10000, 10000, density = 0.0001)
  rresult_extract(sparse, row_start = 9990, n_rows = 10,
                  col_start = 9990, n_cols = 10)
}
```

Matrix table search, sorting and paging affect **only the queried window**;
Query window CSV saves that window. Heatmaps also use that window, with a
maximum of 200 rows, 200 columns and 40,000 cells, additionally constrained by
the configured preview limits. Missing/non-finite and out-of-colour-range
values are grey. Sparse matrices are sliced and validated before the small
window is converted to a dense matrix.

## Vectors, arrays and object trees

Vectors and arrays provide **Starting value**, **Number of values** and
**Query values** controls for exact indexed windows. Arrays use R's
column-major order. **Inspect distribution** draws a compatible distribution
using all values; the accompanying atomic summary uses a bounded, evenly
spaced sample and states its scope.

```r
rresult_extract(seq_len(10000), start = 9000, count = 5)
rresult_extract(list(data = mtcars), path = "data", page_size = 3)
rresult_plot(mtcars$mpg, "density")
```

Object-tree paths use numeric indices or unique names; use indices when names
are duplicated. Lists and S4 slots expand on demand within depth/child limits.
Unknown objects do not automatically invoke their custom `summary()`, `print()`,
`plot()`, `as.list()` or subsetting methods. Functions are not executed.
Environments list binding names without reading values, forcing promises or
triggering active bindings.

## Statistical reports

Dedicated reports match standard class signatures. Unfamiliar subclasses use
the object explorer. `rresult_supported()` provides the full inventory,
including standard tibble, data.table, grouped/rowwise data-frame and other
supported variants.

| Objects | Main report views |
|---|---|
| `lm`, `summary.lm` | Fit quantities, coefficients, intervals and available diagnostics |
| `glm`, `summary.glm` | Family/link, coefficients, Wald intervals, deviance and available response diagnostics |
| `aov`, `anova` | Sequential contributions/diagnostics for `aov`; original test and comparison columns for `anova` |
| `htest`, `power.htest` | Saved estimates, test quantities, intervals or power-calculation parameters |
| `TukeyHSD`, `pairwise.htest` | Simultaneous contrasts or adjusted P-value comparisons |
| `nls` | Parameters, local-linear intervals and fit/residual views |
| MASS `negbin` | Theta/SE, saved fitting warnings, coefficients, count ratios and diagnostics |
| mgcv `gam` / `bam` | Parametric/smooth tables, supported one-dimensional smooth curves and diagnostics |
| `survfit` | Survival, cumulative-event and cumulative-hazard curves, saved intervals, censor marks and risk snapshots |
| `coxph`, `survreg` | Model-specific coefficients and uncertainty; Cox hazard ratios |
| `lmerMod`, `glmerMod`, `lmerModLmerTest`, `lme` | Fixed effects, variance/covariance components, correlations and conditional diagnostics |
| `emmGrid`, `summary_emm` | Marginal estimates and contrasts with their saved scale and adjustment |
| `prcomp`, `princomp`, FactoMineR `PCA` | Sample maps, variance, variable coordinates, arrow overlays and coordinate heatmaps |
| vegan `metaMDS`, `monoMDS` | Saved sample maps, stress and available species/Shepard views |
| ape `pcoa`, vegan `wcmdscale` | Saved principal coordinates, eigenvalues and correction information |
| vegan `rda`, `cca`, `anova.cca` / `adonis2` | Saved constrained ordination or permutation-test quantities |
| vegan `betadisper`, `envfit` | Saved dispersion distances, fit strengths, directions and centroids |
| pROC `roc` | Saved ROC curve, AUC, available AUC interval and thresholds |
| `kmeans`, `hclust`, `dist` | Cluster sizes/centres, hierarchy summaries or bounded distance windows |
| `ts` / `mts`, `density`, `table` / `xtabs` | Saved time trends, windowed ACF, density grids or count/weight views |
| `ggplot` | Original chart, layers/mappings, bounded source-data previews and optional style edits |

```r
rresult_report(t.test(mpg ~ am, data = mtcars))$tables
rresult_plot(glm(am ~ wt + hp, data = mtcars, family = binomial()),
             "coefficients", effect_scale = "ratio")
rresult_plot(fit, "qq")
```

Interpretation notes follow the supplied results. Linear-model notes cover
factors/interactions, omitted observations, rank deficiency and insufficient
residual degrees of freedom. Aliased coefficients remain visible with
unavailable statistics.

GLM coefficients use the link scale by default. Binomial/logit and Poisson/log
models offer odds/rate-ratio views; intervals are 95% Wald intervals using the
`summary.glm()` reference distribution. Observed/fitted binomial charts use
success proportions or 0/1 responses, including when the response is recovered
from the stored model frame. Negative-binomial reports retain saved theta
estimation warnings as well as convergence information.

The `aov` contribution views use sequential Type I sums of squares; generic
`anova` tables retain their original test columns. Mixed-model tables retain
random-effect covariance/correlation entries; their variance charts show diagonal
variances only. Survival and comparison views retain stored confidence levels
and multiplicity adjustments. A saved `htest` does not supply raw observations,
and a saved `kmeans` result does not supply observation coordinates.

GAM curves are available for saved one-dimensional numeric smooths without a
`by` modifier. They show centred link-scale effects with approximate 95%
pointwise intervals conditional on smoothing parameters. Other smooth terms
remain in the tables. ROC AUC intervals are displayed only when already saved;
no new bootstrap or threshold optimisation is run. Time-series ACF uses a
bounded contiguous leading window.

## PCA, NMDS and PCoA

Supply sample metadata to add colour, shape and panel controls. By default,
metadata row names are matched to saved sample names. Use an ID column via
`sample_id = "sample"`, or explicitly select `sample_id = ".position"` for
positional matching. IDs must be unique and cover every saved sample; extra
metadata rows are allowed. Positional matching requires the same row count.

```r
pca <- prcomp(iris[, 1:4], scale. = TRUE)
metadata <- iris["Species"]
rresult_plot(pca, "scores", metadata = metadata, sample_id = ".position",
             colour_by = "Species")
rresult_plot(pca, "biplot", metadata = metadata, sample_id = ".position",
             colour_by = "Species", group_hulls = TRUE, top_variables = 4)

if (requireNamespace("vegan", quietly = TRUE)) {
  data("dune", package = "vegan")
  data("dune.env", package = "vegan")
  set.seed(42)
  nmds <- vegan::metaMDS(vegan::vegdist(dune), trace = 0)
  rresult_plot(nmds, "scores", metadata = dune.env, colour_by = "Management")
  pcoa <- vegan::wcmdscale(vegan::vegdist(dune), eig = TRUE, add = "lingoes")
  rresult_plot(pcoa, "scores", metadata = dune.env, colour_by = "Management")
}
```

Maps retain all finite saved sample coordinates. PCA needs saved scores;
FactoMineR views use active individuals/variables. Variable overlays use one
reported visual multiplier. Group hulls are outlines, not confidence regions
or group tests. NMDS axes have no explained-variance percentages; stress
describes the whole saved configuration. Missing species coordinates leave
the sample map available, while species views are omitted. Shepard plots
require saved engine pairs.

PCoA uses saved corrected coordinates when present; percentages use the
positive eigenvalue sum, with negative eigenvalues reported separately.
`wcmdscale()` needs `eig = TRUE` or `x.ret = TRUE` to return its report class.
RDA/CCA use saved scaling-0 coordinates and percentages based on total inertia.
A standalone `envfit` result does not contain the original sample map.

## Single-cell and bioinformatics objects

| Objects | Main report views |
|---|---|
| Seurat, legacy Assay, Assay5 | Assay/layer selection, saved embeddings, feature maps, metadata and expression windows |
| SummarizedExperiment, RangedSummarizedExperiment | Assay windows, observation/feature annotations and expression summaries |
| SingleCellExperiment | Experiment views plus saved reduced dimensions and alternative-experiment inventory |
| SpatialExperiment | Experiment views plus saved physical-coordinate and feature-coordinate maps |
| DESeqDataSet, DESeqTransform, edgeR DGEList | Stored assays/counts and annotations |
| DESeqResults | Standard saved DESeq2 result columns, descriptions, volcano/MA views and display cutoffs |
| edgeR DGEExact/DGELRT, limma MArrayLM | Saved tests, coefficient selection and available volcano/MA/probability views |
| enrichResult, gseaResult, compareClusterResult | Saved term tables, ranked dot/bar charts and comparison groups |

Seurat access requires **SeuratObject >= 5.0.0**. Split layers are selected
individually and are not joined. Ordinary saved embedding maps cover the saved
reduction independently of the expression layer; feature maps use the selected
layer's cells with available coordinates. Gene identifiers match exactly.

A small expression example:

```r
if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
  set.seed(42)
  counts <- matrix(rpois(400, lambda = 2), nrow = 20,
    dimnames = list(paste0("Gene", 1:20), paste0("Cell", 1:20)))
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(group = rep(c("A", "B"), each = 10)))
  rresult_plot(sce, "expression_dot", assay = "counts",
               feature = c("Gene1", "Gene2"), colour_by = "group")
}

# For an existing Seurat object with a saved UMAP and RNA data layer:
# rresult_plot(seurat_object, "feature_map", assay = "RNA", layer = "data",
#              reduction = "umap", feature = "MS4A1")
# For an existing limma result:
# rresult_report(limma_fit, bio_coef = "Treatment")
```

Opening a saved embedding does not read an expression preview. Container
tables load when selected. Feature maps/distributions and grouped expression
summaries use all selected-source observations, with expression read in bounded
blocks. Expression heatmaps, annotation tables and expression windows remain
bounded previews. Blank feature selection takes the first 6 genes for grouped
summaries, 20 for heatmaps and 1 for single-gene views.

Above-zero expression percentages retain the selected layer's scale; positive
centred values are not biological detection rates. Spatial views use saved
physical coordinates without histology overlays. No normalisation, dimensional
reduction, clustering, differential testing or enrichment pipeline is launched.

Saved gene-result plots use all rows. Existing adjusted P values are retained.
If edgeR/limma has no saved FDR, BH adjustment uses all stored P values for the
selected test when the full family fits `max_scan_rows`; otherwise the report
keeps raw P values and explains why FDR is unavailable. Display cutoffs do not
change the saved tests. limma coefficients require an appropriate log2-scale
contrast to represent log2 fold changes. Multi-parameter edgeR tests omit
single-effect volcano/MA views. An LRT P value can test more than the fold
change displayed on the horizontal axis.

## Chart choices and appearance

| Selected variables | Compatible chart examples |
|---|---|
| One numeric variable | Histogram, frequency polygon, density, box plot, violin, ECDF |
| One categorical variable | Counts or proportions |
| Two numeric variables | Scatter, linear trend, 2D bin counts; ordered lines when X is unique within groups |
| Time X and numeric Y | Time trend or scatter |
| Category and numeric | Box/violin, individual observations, mean +/- SE, explicit summary bars |
| Two categorical variables | Grouped counts, within-X proportions, heatmap or count bubbles |
| Date/time alone | Temporal distribution or ECDF |

```r
rresult_plot_choices(iris, axis_x = 5, axis_y = 1)
rresult_plot(iris, "violin", axis_x = 5, axis_y = 1,
  style = list(theme = "classic", palette = "colorblind", base_size = 14))
rresult_plot(iris, "summary_bar", axis_x = 5, axis_y = 1,
             aggregation = "median")
rresult_plot(iris, "smooth", axis_x = 1, axis_y = 2, colour_by = 5,
             facet_by = 5, style = list(palette = "viridis", legend = "bottom"))
```

For data frames, `rresult_plot_choices()` lists compatible charts for the
selected variables and roles. The report also offers missingness and
correlation views; these can be requested with `rresult_plot(x, "missingness")`
and `rresult_plot(x, "correlation")`. Density/violin views need enough distinct
finite values; mean/SE views show one standard error and assume independent
observations. Smooth charts fit linear trends to the displayed complete
observations and can show 95% pointwise confidence bands.

**Style settings** provides nine themes, nine palettes, custom fixed/mapped
colours, point shapes, text sizes, axes, grids, borders, legends and spacing.
Settings remain after the editor closes. **Canvas and export size** separates
screen height from export dimensions. **Plot proportions: Fill available
canvas** releases a Cartesian fixed ratio; retain equal axis units for
ordination-distance comparisons. See `?rresult_plot` for the full style API.

Standard existing ggplots keep their original data, layers, mappings, scales
and theme by default:

```r
p <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width, colour = Species)) +
  ggplot2::geom_point() + ggplot2::theme_classic()
rresult_report(p)$tables$Layers
rresult_plot(p)
rresult_plot(p, style = list(title = "Updated title", base_size = 16))
```

The workspace can inspect global/layer source-data previews and mappings.
Plotting executes the supplied ggplot normally; metadata inspection does not
evaluate its aesthetic expressions or data functions. Imported plots default
to **Keep original**, with optional style edits. Unknown plot wrappers are
handled by the explorer.

## Export results

| Control | Saved content |
|---|---|
| Query window CSV | The queried data-frame page, matrix window or atomic window; source values and original row names where applicable |
| Table CSV | The complete selected report table, independently of its displayed page/column window |
| Chart PNG / Vector PDF | The selected report chart with its current style and export dimensions |
| Save report HTML | All report tables and the selected chart in a standalone offline file with an embedded image |

Plain double values are written with 17 significant digits. Query exports use
the original supported column values rather than rounded preview strings;
CSV does not preserve R class metadata. Unsupported columns use the same
descriptive text as the preview. Container annotations, expression/distance windows and
ggplot source-data tables retain their stated preview scope in exports.
DESeq2 tables contain the standard available result columns, not arbitrary
user-added columns. Exports do not save the complete original R object.
Large complete tables and HTML files can take time to assemble.

## Public API and resource limits

| Function | Purpose |
|---|---|
| `rresult()` | Open a local report or object explorer |
| `rresult_explore()` | Open the object explorer directly |
| `rresult_info()` | Read conservative object metadata |
| `rresult_extract()` | Query a bounded preview or indexed window |
| `rresult_report()` | Return a structured report with tables and interpretation notes |
| `rresult_plot()` | Return a ggplot from a supported object |
| `rresult_plot_choices()` | List available chart choices for the object/selected variables |
| `rresult_supported()` | List dedicated report families |
| `rresult_register_adapter()` | Register trusted modules for another class in the current R process |

```r
rresult_extract(mat, n_rows = 200, n_cols = 100,
  limits = list(max_rows = 200, max_cols = 100, max_cells = 20000))
```

| Limit argument | Default | Hard ceiling |
|---|---:|---:|
| `max_rows` | 100 | 10,000 |
| `max_cols` | 50 | 1,000 |
| `max_cells` | 10,000 | 1,000,000 |
| `max_preview_bytes` | 5 MiB | 50 MiB |
| `max_depth` | 4 | 20 |
| `max_children` | 50 | 1,000 |
| `max_plot_points` | 10,000 | 100,000 |
| `max_scan_rows` | 100,000 | 1,000,000 |
| `max_name_scan` | 1,000,000 | 10,000,000 |

Pass limits directly to `rresult()`/`rresult_info()`, or in `limits = list(...)`
for extraction, reports, plots and plot choices. Limits cannot be disabled
with `Inf`. They bound previews, tree expansion, name scanning, sampled
profiles, selected diagnostics and expression-read blocks; they do not
downsample ordinary data charts, saved coordinate maps, core saved-result
charts or the rows of saved statistical tables. Individual views retain their
stated limits. Large full-data charts and native model summaries can still
take time and memory.

## Local workspace behaviour

`rresult()` binds to `127.0.0.1` with an available port by default. Objects
remain in the local R process. The workspace does not upload them or
automatically save them. Each app holds its supplied object reference;
browser sessions keep separate selection, paging and plot state.

Reassigning or changing the caller's variable does not synchronise the app;
open a new workspace to use the new value. By default, the server stops after
the last browser session disconnects. `stop_on_close = FALSE` permits
reconnection and requires an R interrupt to stop the listener.

To construct an app without starting it:

```r
app <- rresult(mtcars, run = FALSE)
# Start it explicitly when ready:
# shiny::runApp(app, host = "127.0.0.1")
```

A manually run app needs an R interrupt after its browser sessions close.
Create a fresh app to run again after stopping it. Sparse access is limited
to recognised Matrix-package classes. Composite/character object sizes are
not recursively calculated at startup; displayed numeric/sparse sizes are
storage estimates. Listing an exceptionally large environment can still be
costly even though binding values are not read.

## Development checks

From the directory containing the source folder `RResult`:

```r
install.packages(c("testthat", "broom"))
testthat::test_local("RResult")
```

From inside the source directory, use `testthat::test_local(".")`.
This loads the current source and runs the existing cases in
`tests/testthat/`; it does not generate new tests. `broom` is used by current
development comparisons, not by the runtime package. Optional object packages
and fixture packages enable their corresponding tests; missing optional
packages can cause those tests to be skipped. The suite includes numerical
comparisons, boundary cases and Shiny server interactions.

The current `.Rbuildignore` excludes `tests`, and `.gitignore` also ignores
that directory. Keep a separate copy or explicitly include the tests in
version control if they are to be shared with a development checkout.
**The following package check does not run those excluded tests.** Run the
source tests separately.

```text
R CMD build RResult
R CMD check --no-manual --no-vignettes RResult_0.1.0.tar.gz
```

These commands build and check the current source; an older archive is not
updated automatically by editing files. Use the newly built archive when
validating an installation. `--no-manual` skips the PDF manual.
The repository includes a Windows/Linux/macOS R-release check configuration;
that configuration alone is not evidence of completed cross-platform testing.

`roxygen2` is needed only when regenerating help files after API documentation
changes. See `?rresult`, `?rresult_extract`, `?rresult_plot` and
`?rresult_report` for detailed arguments.

## License

GPL-3.0-or-later. The full license text is in `LICENSE.md` in the source
repository and in R's `share/licenses/GPL-3`.




