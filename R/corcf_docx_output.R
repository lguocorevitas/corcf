`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Write corcf results to a Word document
#'
#' Create a Word report from a `corcf()` result object, including summary
#' counts, per-variable comparison results, and value-label conflicts.
#'
#' @param res A result object returned by `corcf()`.
#' @param master_path Character scalar. Path to the master data source.
#' @param using_path Character scalar. Path to the using data source.
#' @param id_cols Optional character vector of ID columns used for comparison.
#' @param path Character scalar. Output path for the `.docx` report.
#' @param report_date Date for the report header. Defaults to `Sys.Date()`.
#'
#' @returns Invisibly returns the output file path.
#' @export
#'
#' @examples
#' res <- list(
#'   ecode = 9L,
#'   n_master = 3L,
#'   n_using = 3L,
#'   type_mismatch = 0L,
#'   value_mismatch = 1L,
#'   merge_counts = list(
#'     not_matched = 0L,
#'     master_only = 0L,
#'     using_only = 0L,
#'     matched = 3L
#'   ),
#'   per_variable = list(
#'     x = list(status = "mismatch", n = 1L),
#'     y = list(status = "match", n = 0L)
#'   ),
#'   label_conflicts = list(),
#'   vars_missing_in_using = character(),
#'   vars_missing_in_master = character()
#' )
#'
#' tmp <- tempfile(fileext = ".docx")
#' \dontrun{
#'   write_corcf_word(
#'     res = res,
#'     master_path = "master.csv",
#'     using_path = "using.csv",
#'     id_cols = c("id"),
#'     path = tmp
#'   )
#' }
write_corcf_word <- function(res,
                             master_path,
                             using_path,
                             id_cols = NULL,
                             path = "corcf_results.docx",
                             report_date = Sys.Date()) {

  required_fields <- c(
    "ecode", "n_master", "n_using", "type_mismatch", "value_mismatch",
    "merge_counts", "per_variable", "label_conflicts",
    "vars_missing_in_using", "vars_missing_in_master"
  )

  if (!is.list(res) || !all(required_fields %in% names(res))) {
    stop(
      "`res` must be a corcf result-like list containing: ",
      paste(required_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(id_cols) && !is.character(id_cols)) {
    stop("`id_cols` must be NULL or a character vector.", call. = FALSE)
  }

  if (!is.character(path) || length(path) != 1L) {
    stop("`path` must be a single character string.", call. = FALSE)
  }

  doc <- officer::read_docx()

  doc <- officer::body_add_par(
    doc,
    "corcf comparison results",
    style = "heading 1"
  )

  doc <- officer::body_add_par(
    doc,
    sprintf("Report date: %s", format(report_date, "%Y-%m-%d"))
  )
  doc <- officer::body_add_par(
    doc,
    sprintf("Master file: %s", master_path)
  )
  doc <- officer::body_add_par(
    doc,
    sprintf("Using file: %s", using_path)
  )

  if (!is.null(id_cols) && length(id_cols) > 0L) {
    doc <- officer::body_add_par(
      doc,
      sprintf("Unique key (id_cols): %s", paste(id_cols, collapse = ", "))
    )
  }

  doc <- officer::body_add_par(doc, sprintf("ecode: %s", res$ecode))
  doc <- officer::body_add_par(doc, sprintf("master N: %s", res$n_master))
  doc <- officer::body_add_par(doc, sprintf("using N: %s", res$n_using))
  doc <- officer::body_add_par(doc, sprintf("type mismatches: %s", res$type_mismatch))
  doc <- officer::body_add_par(doc, sprintf("value mismatches: %s", res$value_mismatch))

  doc <- officer::body_add_par(doc, "Variables not found in using:")
  doc <- officer::body_add_par(
    doc,
    if (length(res$vars_missing_in_using) > 0L) {
      paste(res$vars_missing_in_using, collapse = ", ")
    } else {
      "None"
    }
  )

  doc <- officer::body_add_par(doc, "Variables not found in master:")
  doc <- officer::body_add_par(
    doc,
    if (length(res$vars_missing_in_master) > 0L) {
      paste(res$vars_missing_in_master, collapse = ", ")
    } else {
      "None"
    }
  )

  if (!is.null(res$merge_counts)) {
    mc <- res$merge_counts

    doc <- officer::body_add_par(doc, "Merge counts by ID", style = "heading 2")

    merge_df <- data.frame(
      Result = c(
        "Not matched",
        "from master (_merge==1)",
        "from using (_merge==2)",
        "Matched (_merge==3)"
      ),
      `Number of obs` = c(
        mc$not_matched %||% NA_integer_,
        mc$master_only %||% NA_integer_,
        mc$using_only %||% NA_integer_,
        mc$matched %||% NA_integer_
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    ft_mc <- flextable::flextable(merge_df)
    ft_mc <- flextable::autofit(ft_mc)
    ft_mc <- flextable::width(ft_mc, j = "Result", width = 3.0)
    ft_mc <- flextable::set_table_properties(ft_mc, layout = "autofit", width = 1)

    doc <- flextable::body_add_flextable(doc, ft_mc)
  }

  pv_names <- names(res$per_variable)

  if (!is.null(pv_names) && length(pv_names) > 0L) {
    pv <- do.call(rbind, lapply(pv_names, function(v) {
      x <- res$per_variable[[v]]
      data.frame(
        variable = v,
        status = x$status %||% NA_character_,
        n = x$n %||% NA_integer_,
        stringsAsFactors = FALSE
      )
    }))

    doc <- officer::body_add_par(doc, "Per-variable summary", style = "heading 2")

    ft <- flextable::flextable(pv)
    ft <- flextable::fontsize(ft, part = "all", size = 9)
    ft <- flextable::autofit(ft)
    ft <- flextable::width(ft, j = "variable", width = 2.5)
    ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)

    doc <- flextable::body_add_flextable(doc, ft)
  }

  if (!is.null(res$label_conflicts) && length(res$label_conflicts) > 0L) {
    lc <- do.call(rbind, lapply(names(res$label_conflicts), function(v) {
      out <- res$label_conflicts[[v]]
      cbind(variable = v, out, stringsAsFactors = FALSE)
    }))

    doc <- officer::body_add_par(doc, "Value label conflicts", style = "heading 2")

    ft_lc <- flextable::flextable(lc)
    ft_lc <- flextable::fontsize(ft_lc, part = "all", size = 9)
    ft_lc <- flextable::autofit(ft_lc)
    ft_lc <- flextable::width(ft_lc, j = "variable", width = 2.0)
    ft_lc <- flextable::width(ft_lc, j = "master_label", width = 2.2)
    ft_lc <- flextable::width(ft_lc, j = "using_label", width = 2.2)
    ft_lc <- flextable::set_table_properties(ft_lc, layout = "autofit", width = 1)

    doc <- flextable::body_add_flextable(doc, ft_lc)
  }

  print(doc, target = path)
  invisible(path)
}
