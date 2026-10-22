# Public API ------------------------------------------------------------------

#' Open a live R object workspace
#'
#' @description
#' Inspect an R object through a local, interactive Shiny workspace. Queries
#' access the object on demand; the complete object is never serialized to the
#' browser. Use \code{run = FALSE} to construct an app without starting it.
#' Supported statistical results and data frames open a visual report with
#' metrics, charts, result tables and CSV/PNG/offline HTML downloads.
#' Standard bioinformatics containers and saved differential-expression and
#' enrichment results have dedicated read-only reports. Their metadata are
#' obtained from the supplied object; optional packages are needed only for
#' their associated object families.
#'
#' @param x Any R object. Unknown classes receive conservative inspection.
#' @param view One of \code{"auto"}, \code{"report"}, or \code{"explore"}.
#' @param max_rows Maximum rows per returned preview/page.
#' @param max_cols Maximum columns per returned preview/page.
#' @param max_depth Maximum object-tree path depth.
#' @param max_preview_bytes Maximum size in bytes of a provider response.
#' @param launch.browser Logical or function passed to \code{shiny::runApp}.
#' @param max_cells Maximum cells per matrix window or data-frame page.
#' @param max_children Maximum children or name-search results per request.
#' @param max_plot_points Budget for sampled profiles, selected diagnostic
#'   views and expression-read blocks. Ordinary vector/data-frame charts,
#'   saved coordinate maps and core saved-result charts retain all relevant
#'   observations. Each limited view states its scope.
#' @param max_scan_rows Maximum data-frame rows searched/sorted per query.
#' @param max_name_scan Maximum matrix names scanned per name request.
#' @param port Optional local port. \code{NULL} lets Shiny select a free port.
#' @param run Logical; start the local app immediately.
#' @param stop_on_close Logical; stop a running app when its last browser
#'   session disconnects. Set FALSE to allow reconnecting after tab closure.
#' @param metadata Optional data frame of sample annotations for PCA, NMDS,
#'   PCoA, RDA or CCA. Rows are matched to saved sample names, using metadata row names by
#'   default. Extra metadata rows are allowed. Missing or duplicate IDs are rejected.
#' @param sample_id Optional metadata column name containing sample IDs.
#'   Use \code{".position"} to explicitly match by row position instead; this
#'   requires exactly one metadata row per saved sample.
#' @param ... Reserved for future extensions; currently rejected when supplied.
#'
#' @details
#' The listener is always \code{127.0.0.1}, even if \code{shiny.host} is changed.
#' Each app owns its object reference and each session owns its interaction
#' state. Ordinary R copy-on-modify semantics apply: reassigning the caller's
#' variable does not update the app. Environments are listed conservatively
#' without forcing values. Closing the last tab (or clicking Close workspace)
#' releases session caches and stops the default running app; the caller's
#' object remains alive. Interrupt R to stop before a browser connects.
#'
#' Limits have finite hard ceilings and are validated in both public APIs and
#' providers. List/character object sizes are not recursively calculated at
#' startup. Composite metadata is intentionally bounded. Custom adapter code
#' is explicitly trusted and may have different computational costs.
#' Bioinformatics workspaces share their selected source and load result
#' tables on demand. Opening saved embeddings does not read an expression
#' preview. \code{rresult_report()} and saved HTML reports retain complete tables.
#' Saved UMAP/PCA/spatial and ordination maps show all finite saved coordinates.
#' Gene maps and gene distributions read all selected-source observations in
#' bounded blocks; grouped gene summaries and composition use all observations.
#' Saved result tables retain their rows and columns in R and are paged in the
#' browser. CSV saves the complete selected result table. Expression windows,
#' container annotations and ggplot source-data previews retain their stated
#' preview scope, including in downloads.
#' @return When \code{run = FALSE}, an invisible \code{shiny.appobj}; otherwise the
#'   invisible result returned by \code{shiny::runApp} after the app stops.
#' @examples
#' app <- rresult(mtcars, run = FALSE)
#' rresult(matrix(1:100, 10), run = FALSE)
#' rresult(lm(mpg ~ wt + hp, mtcars), run = FALSE)
#' if (interactive()) rresult(mtcars)
#' @seealso \code{rresult_extract}, \code{rresult_info}, \code{rresult_plot}, \code{rresult_report}
#' @export
rresult <- function(x, view = "auto", max_rows = 100, max_cols = 50,
                    max_depth = 4, max_preview_bytes = 5 * 1024^2,
                    launch.browser = TRUE, max_cells = 10000,
                    max_children = 50, max_plot_points = 10000,
                    max_scan_rows = 100000, max_name_scan = 1000000,
                    port = NULL, run = TRUE, stop_on_close = TRUE,
                    metadata = NULL, sample_id = NULL, ...) {
  if (length(list(...))) stop("Unused arguments in ...", call. = FALSE)
  view <- match.arg(view, c("auto", "report", "explore"))
  if (!is.logical(run) || length(run) != 1L || is.na(run)) stop("run must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(stop_on_close) || length(stop_on_close) != 1L || is.na(stop_on_close)) stop("stop_on_close must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(port)) port <- rr_integer(port, "port", 1024, 65535)
  limits <- rr_limits(max_rows, max_cols, max_depth, max_preview_bytes, max_cells,
                      max_children, max_plot_points, max_scan_rows, max_name_scan)
  if (!is.null(metadata) || !is.null(sample_id)) {
    if (is.null(rr_ordination_kind(x))) stop("Sample metadata is currently supported for ordinations only.", call. = FALSE)
    scores <- rr_ordination(x)$scores
    rr_ordination_metadata(metadata, sample_id, scores,
      if (is.null(scores)) integer() else rr_sample_indices(nrow(scores), limits$max_rows), limits)
  }
  app <- rr_app(x, view, limits, stop_on_close = stop_on_close && run, metadata = metadata, sample_id = sample_id)
  if (!run) return(invisible(app))
  invisible(shiny::runApp(app, host = "127.0.0.1", port = port, launch.browser = launch.browser))
}

#' Force the generic live object explorer
#' @param x Any R object.
#' @param ... Arguments passed to \code{rresult}, except \code{view}.
#' @return The value returned by \code{rresult}.
#' @examples
#' rresult_explore(lm(mpg ~ wt, mtcars), run = FALSE)
#' @export
rresult_explore <- function(x, ...) {
  rresult(x, view = "explore", ...)
}

#' Inspect bounded object metadata
#' @param x Any R object.
#' @param ... Named limits accepted by \code{rresult}: \code{max_rows},
#'   \code{max_cols}, \code{max_depth}, \code{max_preview_bytes},
#'   \code{max_cells}, \code{max_children}, \code{max_plot_points},
#'   \code{max_scan_rows}, and \code{max_name_scan}.
#' @return A list of safe metadata. Unknown/composite sizes may be NA.
#' @examples
#' rresult_info(mtcars)
#' rresult_info(function(x, y = 1) x + y)
#' @export
rresult_info <- function(x, ...) {
  rr_info(x, rr_limits(...))
}

#' Extract a bounded, live object preview
#'
#' @description
#' Request a matrix window, a data-frame page, or conservative node information.
#' Numeric matrix indices may reach any valid position in the source object.
#' Name selection returns every occurrence of duplicate names, subject to limits.
#'
#' @param x Any R object.
#' @param path List/vector of numeric child indices or unique child/slot names.
#' @param ... Provider arguments. Matrices accept \code{row_start},
#'   \code{n_rows}, \code{col_start}, \code{n_cols}, \code{rows}, \code{cols},
#'   \code{row_names}, \code{col_names}. Data frames accept \code{row_start},
#'   \code{n_rows} (bounded search range), \code{cols} (numeric indices),
#'   \code{page}, \code{page_size}, \code{search} (literal text),
#'   \code{sort_col} (numeric index), \code{decreasing}. Vectors and arrays
#'   accept \code{start} and \code{count} for an exact contiguous index window;
#'   arrays use column-major order. Without these arguments the original
#'   deterministic sample preview is returned.
#' @param limits Named list overriding the limits documented in \code{rresult}.
#' @return A bounded list containing \code{data} and indices for tabular objects,
#'   sampled or explicitly windowed values for atomic objects, or node metadata
#'   and immediate children.
#'   Data-frame \code{data} contains display strings with stable internal column
#'   IDs; original column names are in \code{column_labels}. Original row names
#'   are retained in \code{row_labels} and the preview's row names; numeric source
#'   positions remain in \code{row_indices}. This is a preview API.
#' @examples
#' rresult_extract(matrix(1:100, 10), row_start = 3, n_rows = 2,
#'                 col_start = 2, n_cols = 3)
#' rresult_extract(mtcars, page = 2, page_size = 5, sort_col = 1)
#' rresult_extract(list(data = mtcars), path = "data", page_size = 3)
#' rresult_extract(seq_len(10000), start = 9000, count = 5)
#' @export
rresult_extract <- function(x, path = NULL, ..., limits = list()) {
  rr_extract(x, do.call(rr_limits, limits), path = path, ...)
}

#' Create a statistical plot from an R object
#' @param x A known atomic vector, matrix, data frame, or supported statistical
#'   result; see \code{rresult_supported} for the complete standard-class list.
#' @param type Plot type: \code{auto}, \code{histogram}, \code{density},
#'   \code{bar}, \code{heatmap}, \code{coefficients}, \code{residuals},
#'   \code{qq}, \code{scale_location}, \code{cooks}, \code{fitted}, \code{test},
#'   \code{scree}, \code{scores}, \code{loadings}, \code{clusters}, \code{centers},
#'   \code{anova}, \code{series}, \code{acf}, \code{missingness},
#'   \code{correlation}, or \code{relationship}. Data charts additionally include
#'   \code{scatter}, \code{line}, \code{boxplot}, \code{violin}, \code{jitter},
#'   \code{ecdf}, \code{proportion}, \code{summary_bar}, and \code{count_heatmap}.
#'   Additional data charts are \code{freqpoly}, \code{smooth} (a fitted linear
#'   trend), \code{bin2d}, \code{mean_se}, and \code{count_bubble}.
#'   Use \code{rresult_plot_choices(x)} to list valid identifiers, including
#'   the additional standard-object charts. Incompatible charts are rejected.
#' @param ... Matrix-window arguments accepted by \code{rresult_extract},
#'   and optional \code{color_range = c(low, high)} for heatmaps.
#'   PCA and relationship plots accept \code{axis_x} and \code{axis_y} (component
#'   or numeric column indices). Report coefficient plots accept
#'   \code{include_intercept} and \code{effect_scale = "ratio"} for binomial/logit
#'   odds ratios or Poisson/log rate ratios. GLM intervals are 95\% Wald intervals.
#'   Data frames accept \code{axis_y = 0} for a single-variable chart,
#'   \code{bins} (2 to 100), \code{show_points}, and
#'   \code{aggregation = "mean"}, \code{"median"} or \code{"sum"} for summary bars.
#'   \code{colour_by} and \code{facet_by} select categorical column indices;
#'   zero or NULL means no grouping. \code{x_as_category} and
#'   \code{y_as_category} explicitly treat the selected axes as categories without
#'   modifying the source. \code{facet_scales} is fixed, free_x, free_y or free;
#'   all observed panels are retained. \code{mean_se} shows mean +/- one standard
#'   error of displayed observations, assuming independent observations.
#'   \code{show_ci} controls available uncertainty intervals and linear-trend
#'   bands; \code{show_censor} controls censor marks for survival curves.
#'   Survival and comparison reports retain their stored confidence level and
#'   multiplicity adjustment. Cox charts additionally provide hazard_ratios.
#'   The hclust cut-tree chart accepts \code{clusters}.
#'   Ordination maps additionally accept \code{metadata}, \code{sample_id}
#'   (as in \code{rresult}), \code{colour_by}, \code{shape_by} and
#'   \code{facet_by} (metadata column names or indices), \code{sample_labels},
#'   \code{max_labels} (0--200), \code{top_variables} (1--100), and
#'   \code{group_hulls}. Shape and panel columns must be categorical. Hulls
#'   outline displayed samples in categorical colour groups, not confidence
#'   regions. Sample maps use equal axis units and saved coordinates only.
#'   PCA arrow overlays rescale all shown variable coordinates by one reported
#'   visual factor. NMDS axes have no explained-variance percentages; PCoA
#'   percentages use positive eigenvalues and retain stored corrections.
#'   RDA/CCA use saved scaling-0 coordinates and total-inertia percentages;
#'   \code{type = "environment"} overlays explanatory-variable arrows.
#'   GAM \code{type = "smooths"} accepts \code{smooth_term}, the original
#'   numeric smooth index. Curves are available for saved one-dimensional
#'   numeric smooths without a by modifier and show centered link-scale effects.
#'   DESeqResults volcano/MA charts accept \code{padj_cutoff} (default: saved
#'   alpha, or 0.05) and nonnegative \code{lfc_cutoff} (default: 0).
#'   These are display cutoffs; saved tests and P-value adjustment are retained.
#'   Standard pROC objects provide \code{roc} and \code{thresholds} charts,
#'   preserving direction and percent/proportion scale. AUC intervals are
#'   displayed in the report only when already saved.
#'   Bioinformatics containers additionally accept \code{assay}, Seurat
#'   \code{layer}, \code{reduction}, and \code{feature} (exact IDs or numeric
#'   feature indices). Views include \code{embedding}, \code{feature_map},
#'   \code{spatial}, \code{spatial_feature}, \code{composition},
#'   \code{metadata_distribution}, \code{expression_heatmap},
#'   \code{expression_violin}, and \code{expression_dot}.
#'   \code{colour_by}, \code{shape_by}, and \code{facet_by} use observation
#'   metadata column names/indices; shape/facet maps require categories.
#'   Seurat's saved identities are available as \code{".identity"} unless
#'   that metadata name is already in use. Maps retain saved coordinate units;
#'   no dimensional reduction, normalization, clustering or gene testing is run.
#'   Saved maps display all finite coordinates. Gene maps, distributions and
#'   grouped summaries use all observations in the selected expression source,
#'   reading expression in bounded blocks. Blank feature selection takes the
#'   first 6 genes for grouped summaries, 20 for heatmaps and 1 for single-gene
#'   views. Expression heatmaps remain bounded previews. Summaries retain layer scale;
#'   above-zero percentages in a centered layer are not detection rates.
#'   limma MArrayLM objects accept \code{bio_coef} (saved coefficient name or
#'   index). limma values are saved coefficients, including possible intercepts;
#'   log2-fold-change interpretation requires an appropriate log2-scale contrast.
#'   edgeR/limma provide available \code{volcano}, \code{ma}, and
#'   \code{probability} views. BH FDR is computed over the full stored gene
#'   family before sampling when it fits \code{max_scan_rows}; otherwise saved
#'   raw P values remain available without subset-based FDR. Omnibus edgeR tests
#'   omit single-effect volcano/MA views. Existing FDR columns are retained.
#'   Saved enrichment objects provide \code{enrichment_dot} and
#'   \code{enrichment_bar}; \code{top_terms} (1--100, default 20) selects leading
#'   terms within each displayed group, without rerunning enrichment.
#' @param limits Named list overriding finite provider limits.
#' @param style Named appearance list; all fields are optional.
#'   Themes: \code{theme} = minimal, classic, bw, light, dark, grey,
#'   linedraw, void or test. Palettes: \code{palette} = teal, ocean,
#'   colorblind, purple, viridis, sunset, grey, inferno or blue_red.
#'   \code{colour}/\code{fill} set fixed mark colours; \code{colours}/\code{fills}
#'   are colour vectors for mapped groups or continuous gradients. Named vectors
#'   match exact group names (unlisted groups use the palette); unnamed group
#'   vectors follow scale-limit order and must cover all displayed groups.
#'   Continuous gradients require unnamed colours. Names, hex values and
#'   transparency accepted by \code{grDevices::col2rgb} are supported.
#'   Points/lines: \code{point_size} (0.1--24 mm), \code{alpha} (0--1),
#'   \code{point_shape} (0--25), \code{shapes} (mapped shape codes, at most 26),
#'   \code{stroke} (0--8), \code{line_width} (0.05--12 mm), \code{line_type}
#'   (solid, dashed, dotted, dotdash, longdash, twodash, blank),
#'   \code{ribbon_alpha} (0--1). Mapped sizes, opacity and line types remain mapped.
#'   Text: \code{base_size} (6--72 pt), \code{label_size} (0.5--24 mm for
#'   plot labels), \code{font_family}, \code{font_face} (plain, bold, italic,
#'   bold.italic), \code{text_colour}. Independent sizes (4--120 pt):
#'   \code{title_size}, \code{subtitle_size}, \code{caption_size},
#'   \code{axis_title_size}, \code{axis_text_size}, \code{legend_title_size},
#'   \code{legend_text_size}, \code{strip_text_size}.
#'   Labels: \code{title}, \code{subtitle}, \code{caption}, \code{x_label},
#'   \code{y_label}, \code{title_hjust} (0--1), and logical \code{hide_title},
#'   \code{hide_subtitle}, \code{hide_caption}.
#'   Axes: \code{x_angle}/\code{y_angle} (-180--180 degrees),
#'   logical \code{show_x_text}/\code{show_y_text}, \code{axis_lines}/\code{axis_ticks}
#'   (theme, both, x, y, none), \code{axis_colour}, \code{axis_line_width}/\code{tick_width}
#'   (0.05--6 mm), \code{tick_length} (0--15 mm), logical \code{flip}.
#'   Grid: \code{grid} (theme, major, all, none), \code{grid_colour},
#'   \code{grid_width} (0.05--4 mm), \code{grid_type} (as line_type).
#'   Layout: \code{legend} (bottom, right, top, left, none), \code{legend_rows}
#'   (1--20 for discrete colour/fill/shape scales), \code{legend_key_size}
#'   (1--40 mm), \code{panel_spacing} (0--40 mm), \code{plot_margin}
#'   (one or four values in top/right/bottom/left order, 0--120 pt),
#'   \code{panel_border} (theme, show, hide), \code{border_colour},
#'   \code{border_width} (0.05--6 mm), and \code{panel_fill}, \code{plot_fill},
#'   \code{legend_fill}, \code{strip_fill}.
#'   \code{aspect = "preserve"} retains coordinate ratios by default;
#'   \code{"fill"} releases a Cartesian plot's fixed ratio to fill its canvas.
#'   This changes physical X/Y unit proportions; equal units matter for distances
#'   and ordination geometry. Map/polar plots reject fill/axis-swap requests.
#'   For an existing ggplot, the default returns the supplied plot unchanged.
#'   Only explicit fields apply. Optional colour edits retain scale transformations,
#'   limits and breaks. Type is auto or original. Returned ggplots can also be
#'   edited with ordinary ggplot2 layers/themes.
#' @return A ggplot object. Ordinary vector/data-frame charts retain all rows,
#'   categories and panels. Matrix windows and explicitly limited diagnostics
#'   retain their stated scope.
#'   Saved coordinate maps, gene-result plots and ROC/survival curves retain all
#'   available observations; invalid coordinates are omitted explicitly.
#'   Supplied ggplots retain their original full data and transformations.
#' @examples
#' rresult_plot(mtcars$mpg, "histogram")
#' rresult_plot(matrix(1:100, 10), "heatmap")
#' rresult_plot(lm(mpg ~ wt + hp, mtcars), "coefficients")
#' rresult_plot(prcomp(USArrests, scale. = TRUE), "scores")
#' rresult_plot(iris, "violin", axis_x = 5, axis_y = 1,
#'              style = list(theme = "classic", palette = "colorblind"))
#' rresult_plot(iris, "smooth", axis_x = 1, axis_y = 2, colour_by = 5)
#' @export
rresult_plot <- function(x, type = "auto", ..., limits = list(), style = list()) {
  if (rr_is_ggplot(x)) {
    if (!type %in% c("auto", "original")) stop("Existing ggplots support type auto or original.", call. = FALSE)
    if (length(list(...))) stop("Existing ggplot data and layers are defined in the supplied plot; unused plot arguments.", call. = FALSE)
    do.call(rr_limits, limits)
    return(rr_edit_ggplot(x, style))
  }
  rr_apply_style(rr_plot(x, type, do.call(rr_limits, limits), ...), style)
}




