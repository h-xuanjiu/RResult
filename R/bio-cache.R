# Small, session-owned caches. Keys contain selections, never whole objects.
rr_bio_memoise <- function(fun, max_bytes = Inf) {
  force(fun)
  key <- NULL
  value <- NULL
  ready <- FALSE
  function(...) {
    args <- list(...)
    if (ready && identical(args, key)) return(value)
    result <- fun(...)
    if (is.finite(max_bytes) && as.numeric(utils::object.size(result)) > max_bytes) {
      key <<- NULL; value <<- NULL; ready <<- FALSE
    } else {
      key <<- args; value <<- result; ready <<- TRUE
    }
    result
  }
}

rr_bio_source_cache <- function(x, limits) {
  force(x); force(limits)
  source <- NULL
  list(get = function(assay = NULL, layer = NULL) {
    # The browser initially sends NULL, then the selected default names.
    # These two states refer to the same source and must not prepare it twice.
    same <- function(value, current) is.null(value) || identical(value, "") || identical(value, current)
    if (!is.null(source) && same(assay, source$assay) && same(layer, source$layer)) return(source)
    source <<- rr_bio_source(x, limits, assay, layer, lazy = TRUE)
    source
  }, clear = function() {
    source <<- NULL
    x <<- NULL
  })
}
