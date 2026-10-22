# Staged, bounded selection of exact axis positions ---------------------------

rr_bulk_axis <- function(x, axis) {
  axis <- match.arg(axis, c("rows", "columns"))
  kind <- rr_kind(x)
  if (kind == "data_frame") {
    if (axis == "columns") return(list(total = length(rr_plain(x)), labels = names(rr_plain(x))))
    labels <- .row_names_info(x, type = 0L)
    if (length(labels) == 2L && is.na(labels[1L]) && labels[2L] <= 0L) labels <- NULL
    return(list(total = rr_df_nrow(x), labels = labels))
  }
  if (!kind %in% c("matrix", "sparse_matrix")) stop("Choose a matrix or data frame first.", call. = FALSE)
  index <- if (axis == "rows") 1L else 2L
  list(total = rr_dims(x)[index], labels = rr_dimnames(x)[[index]])
}

rr_bulk_catalogue <- function(x, axis, query, limits) {
  if (!is.character(query) || length(query) != 1L || is.na(query) ||
      nchar(query, type = "bytes") > 256L) stop("Search must be one short literal string.", call. = FALSE)
  info <- rr_bulk_axis(x, axis)
  if (!nzchar(query)) return(list(total = info$total, matches = NULL,
    matched = info$total, scanned = 0L, limited = FALSE))
  n <- min(info$total, limits$max_name_scan)
  chunks <- list()
  if (n) for (start in seq.int(1L, n, by = 4096L)) {
    index <- seq.int(start, min(n, start + 4095L))
    hit <- grepl(query, as.character(index), fixed = TRUE)
    if (!is.null(info$labels)) {
      labels <- info$labels[index]
      hit <- hit | (!is.na(labels) & grepl(query, labels, fixed = TRUE))
    }
    chunks[[length(chunks) + 1L]] <- index[hit]
  }
  matches <- as.integer(unlist(chunks, use.names = FALSE))
  list(total = info$total, matches = matches, matched = length(matches),
    scanned = n, limited = n < info$total)
}

rr_bulk_page_indices <- function(catalogue, page, page_size = 50L) {
  if (!catalogue$matched) return(integer())
  first <- (as.double(page) - 1L) * page_size + 1L
  if (first > catalogue$matched) return(integer())
  index <- seq.int(first, min(catalogue$matched, first + page_size - 1L))
  if (is.null(catalogue$matches)) as.integer(index) else catalogue$matches[index]
}

rr_bulk_labels <- function(x, axis, index) {
  labels <- rr_bulk_axis(x, axis)$labels
  text <- if (is.null(labels)) rep("(unnamed)", length(index)) else labels[index]
  text[is.na(text)] <- "(NA name)"
  text[!nzchar(text)] <- "(unnamed)"
  stats::setNames(as.character(index), paste0(index, ": ", rr_text(text, 160L)))
}

rr_bulk_picker <- function(input, output, session, node, limits, on_apply, selected) {
  active <- shiny::reactiveVal(FALSE)
  axis <- shiny::reactiveVal(NULL)
  pending <- shiny::reactiveVal(integer())
  catalogue <- shiny::reactiveVal(NULL)
  page <- shiny::reactiveVal(1L)
  query <- shiny::reactiveVal("")
  generation <- shiny::reactiveVal(0L)
  error <- shiny::reactiveVal(NULL)
  selection_observer <- NULL
  page_size <- 50L
  cap <- function() limits[[if (identical(axis(), "rows")) "max_rows" else "max_cols"]]
  pages <- function() max(1L, ceiling(catalogue()$matched / page_size))
  visible <- function() rr_bulk_page_indices(catalogue(), page(), page_size)
  checkbox_id <- function() paste0("bulk_items_", generation())

  # Each rendered page gets a new input ID. Late events from a removed page can
  # therefore never clear staged selections on another page or after a bulk edit.
  repaint <- function() {
    if (!is.null(selection_observer)) selection_observer$destroy()
    generation(generation() + 1L)
    id <- checkbox_id()
    token <- generation()
    current <- visible()
    selection_observer <<- shiny::observeEvent(input[[id]], {
      if (!active() || generation() != token) return()
      raw <- input[[id]]
      picked <- suppressWarnings(as.numeric(raw))
      if (length(picked) != length(raw) || anyNA(picked) || any(!picked %in% current)) {
        error("Invalid selection. Choose items from the visible page.")
        repaint()
        return()
      }
      next_value <- sort(unique(c(setdiff(pending(), current), as.integer(picked))))
      if (length(next_value) > cap()) {
        error(paste("Selection exceeds the", cap(), axis(), "limit. The previous selection was kept."))
        repaint()
        return()
      }
      pending(next_value)
      error(NULL)
    }, ignoreInit = TRUE, ignoreNULL = FALSE, priority = 100)
  }

  close <- function() {
    was_active <- shiny::isolate(active())
    active(FALSE)
    pending(integer())
    axis(NULL)
    catalogue(NULL)
    error(NULL)
    if (!is.null(selection_observer)) selection_observer$destroy()
    selection_observer <<- NULL
    if (was_active) shiny::removeModal(session = session)
    invisible(NULL)
  }

  open <- function(value) {
    value <- match.arg(value, c("rows", "columns"))
    info <- rr_bulk_axis(node(), value)
    initial <- selected(value)
    if (is.null(initial)) initial <- integer()
    initial <- rr_indices(initial, info$total,
      limits[[if (value == "rows") "max_rows" else "max_cols"]], "Selection")
    axis(value)
    pending(sort(unique(initial)))
    page(1L)
    query("")
    catalogue(rr_bulk_catalogue(node(), value, "", limits))
    error(NULL)
    active(TRUE)
    repaint()
    dialog <- shiny::modalDialog(
      title = paste("Select", value), size = "l", easyClose = FALSE,
      htmltools::tags$div(class = "rr-bulk-picker",
        shiny::textInput("bulk_search", "Search names or numbers", ""),
        shiny::uiOutput("bulk_status"),
        htmltools::tags$div(class = "rr-bulk-actions",
          shiny::actionButton("bulk_select_page", "Select page", class = "rr-secondary"),
          shiny::actionButton("bulk_select_all", "Select all matches", class = "rr-secondary"),
          shiny::actionButton("bulk_clear", "Clear selection", class = "rr-secondary")),
        shiny::uiOutput("bulk_items"),
        htmltools::tags$div(class = "rr-bulk-pagination",
          shiny::actionButton("bulk_prev", "Previous", class = "rr-secondary"),
          shiny::numericInput("bulk_page", "Page", 1L, min = 1L, max = pages(), step = 1L),
          shiny::actionButton("bulk_next", "Next", class = "rr-secondary"))),
      footer = htmltools::tagList(
        shiny::actionButton("bulk_cancel", "Cancel", class = "rr-secondary"),
        shiny::actionButton("bulk_apply", "Apply selection", class = "rr-primary")))
    shiny::showModal(htmltools::tagAppendAttributes(dialog, class = "rr-bulk-modal"), session = session)
    invisible(NULL)
  }

  output$bulk_items <- shiny::renderUI({
    shiny::req(active())
    generation()
    index <- visible()
    if (!length(index)) return(rr_notice("No matching items. Adjust the search."))
    shiny::checkboxGroupInput(checkbox_id(), NULL,
      choices = rr_bulk_labels(node(), axis(), index),
      selected = as.character(intersect(shiny::isolate(pending()), index)))
  })
  output$bulk_status <- shiny::renderUI({
    shiny::req(active())
    info <- catalogue()
    htmltools::tagList(
      rr_notice(paste(length(pending()), "selected; limit", cap(), axis(), "per query.",
        info$matched, "matching items; page", page(), "of", pages(), ".")),
      if (nzchar(query())) rr_notice(paste("Search checked the first", info$scanned, "of", info$total, "positions.",
        if (info$limited) "Clear the search to browse later indices, or raise max_name_scan.")),
      if (!is.null(error())) rr_notice(error(), TRUE))
  })
  shiny::outputOptions(output, "bulk_items", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "bulk_status", suspendWhenHidden = FALSE)

  change_page <- function(value) {
    if (!active()) return()
    tryCatch({
      value <- rr_integer(value, "Page", upper = pages())
      if (identical(value, page())) return()
      page(value)
      error(NULL)
      repaint()
      shiny::updateNumericInput(session, "bulk_page", value = value, max = pages())
    }, error = function(e) error(conditionMessage(e)))
  }
  shiny::observeEvent(input$bulk_page, change_page(input$bulk_page))
  shiny::observeEvent(input$bulk_prev, change_page(max(1L, page() - 1L)))
  shiny::observeEvent(input$bulk_next, { if (active()) change_page(min(pages(), page() + 1L)) })
  shiny::observeEvent(input$bulk_search, {
    if (!active() || identical(input$bulk_search, query())) return()
    tryCatch({
      result <- rr_bulk_catalogue(node(), axis(), input$bulk_search, limits)
      query(input$bulk_search)
      catalogue(result)
      page(1L)
      error(NULL)
      repaint()
      shiny::updateNumericInput(session, "bulk_page", value = 1L, max = pages())
    }, error = function(e) error(conditionMessage(e)))
  }, ignoreNULL = TRUE)

  add <- function(index) {
    if (!active()) return()
    next_value <- sort(unique(c(pending(), index)))
    if (length(next_value) > cap()) {
      error(paste("Selection exceeds the", cap(), axis(), "limit. The previous selection was kept."))
      return()
    }
    pending(as.integer(next_value))
    error(NULL)
    repaint()
  }
  shiny::observeEvent(input$bulk_select_page, { if (active()) add(visible()) })
  shiny::observeEvent(input$bulk_select_all, {
    if (!active()) return()
    info <- catalogue()
    # Do not allocate an unbounded sequence merely to reject a large selection.
    if (info$matched > cap()) {
      error(paste("There are", info$matched, "matches, exceeding the", cap(), axis(), "limit. The previous selection was kept."))
      return()
    }
    add(if (is.null(info$matches)) seq_len(info$matched) else info$matches)
  })
  shiny::observeEvent(input$bulk_clear, {
    if (!active()) return()
    pending(integer())
    error(NULL)
    repaint()
  })
  shiny::observeEvent(input$bulk_cancel, close())
  shiny::observeEvent(input$bulk_apply, {
    if (!active()) return()
    tryCatch({
      on_apply(axis(), pending())
      close()
    }, error = function(e) error(conditionMessage(e)))
  })

  state <- shiny::reactive({
    if (!active()) return(list(active = FALSE, axis = NULL, pending = integer(),
      page = 1L, query = "", visible = integer(), pages = 1L,
      generation = generation(), checkbox_id = NULL, error = NULL))
    list(active = TRUE, axis = axis(), pending = pending(), page = page(), query = query(),
      visible = visible(), pages = pages(), generation = generation(), checkbox_id = checkbox_id(),
      error = error(), matched = catalogue()$matched, scanned = catalogue()$scanned,
      limited = catalogue()$limited, total = catalogue()$total)
  })
  list(open = open, reset = close, state = state)
}
