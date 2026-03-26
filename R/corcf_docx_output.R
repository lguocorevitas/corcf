library(officer)
library(flextable)
conflicted::conflicts_prefer(flextable::width)

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Title
#'
#' @param res
#' @param master_path
#' @param using_path
#' @param id_cols
#' @param path
#' @param report_date
#'
#' @returns
#' @export
#'
#' @examples
#' tdy_date <- Sys.Date()
#'   write_corcf_word(
#' res,
#' master_path = test_data,
#' using_path  = current_data,
#' id_cols     = id_cols,
#' path = glue("{analytic_data}/rewrite/QC/corcf_results_{data_name}_{tdy_date}.docx"),
#' report_date = Sys.Date()
#' )

write_corcf_word <- function(res,
                             master_path,
                             using_path,
                             id_cols = NULL,
                             path = "corcf_results.docx",
                             report_date = Sys.Date()) {

  doc <- read_docx()

  doc <- body_add_par(doc, "corcf comparison results", style = "heading 1")

  # Report metadata
  doc <- body_add_par(doc, sprintf("Report date: %s", format(report_date, "%Y-%m-%d")))
  doc <- body_add_par(doc, sprintf("Master file: %s", master_path))
  doc <- body_add_par(doc, sprintf("Using file: %s", using_path))
  if (!is.null(id_cols)) {
    stopifnot(is.character(id_cols))
    doc <- body_add_par(doc, sprintf("Unique key (id_cols): %s", paste(id_cols, collapse = ", ")))
  }
  # Existing summary lines
  doc <- body_add_par(doc, sprintf("ecode: %s", res$ecode))
  doc <- body_add_par(doc, sprintf("master N: %s", res$n_master))
  doc <- body_add_par(doc, sprintf("using N: %s", res$n_using))
  doc <- body_add_par(doc, sprintf("type mismatches: %s", res$type_mismatch))
  doc <- body_add_par(doc, sprintf("value mismatches: %s", res$value_mismatch))
  # doc <- officer::body_add_par(
  #   doc,
  #   sprintf("Variables not found in using: %s", paste(res$vars_missing_in_using, collapse = ", "))
  # )
  doc <- officer::body_add_par(
    doc,
    "Variables not found in using:"
  )

  doc <- officer::body_add_par(
    doc,
    paste(res$vars_missing_in_using, collapse = ", ")
  )
#   add vars not found in master
  if (length(res$vars_missing_in_master) > 0) {
    doc <- officer::body_add_par(doc, "Variables not found in master:")
    doc <- officer::body_add_par(
      doc,
      paste(res$vars_missing_in_master, collapse = ", ")
    )
  }
  # Merge counts table (if present)
  if (!is.null(res$merge_counts)) {
    mc <- res$merge_counts
    doc <- body_add_par(doc, "Merge counts by ID", style = "heading 2")

    merge_df <- data.frame(
      Result = c("Not matched", "  from master (_merge==1)", "  from using (_merge==2)", "Matched (_merge==3)"),
      `Number of obs` = c(mc$not_matched, mc$master_only, mc$using_only, mc$matched),
      check.names = FALSE
    )

    ft_mc <- flextable(merge_df)
    ft_mc <- autofit(ft_mc)
    ft_mc <- width(ft_mc, j = "Result", width = 3.0)
    ft_mc <- set_table_properties(ft_mc, layout = "autofit", width = 1)
    doc <- body_add_flextable(doc, ft_mc)
  }

  # Per-variable summary table
  pv <- do.call(rbind, lapply(names(res$per_variable), function(v) {
    x <- res$per_variable[[v]]
    data.frame(
      variable = v,
      status   = x$status %||% NA_character_,
      n        = if (!is.null(x$n)) x$n else NA_integer_,
      stringsAsFactors = FALSE
    )
  }))

  doc <- body_add_par(doc, "Per-variable summary", style = "heading 2")
  ft <- flextable(pv)
  ft <- fontsize(ft, part = "all", size = 9)
  ft <- autofit(ft)
  ft <- width(ft, j = "variable", width = 2.5)
  ft <- set_table_properties(ft, layout = "autofit", width = 1)
  doc <- body_add_flextable(doc, ft)

  # Label conflicts table (if present)
  if (!is.null(res$label_conflicts) && length(res$label_conflicts) > 0) {
    lc <- do.call(rbind, lapply(names(res$label_conflicts), function(v) {
      cbind(variable = v, res$label_conflicts[[v]], stringsAsFactors = FALSE)
    }))

    doc <- body_add_par(doc, "Value label conflicts", style = "heading 2")
    ft_lc <- flextable(lc)
    ft_lc <- fontsize(ft_lc, part = "all", size = 9)
    ft_lc <- autofit(ft_lc)
    ft_lc <- width(ft_lc, j = "variable",     width = 2.0)
    ft_lc <- width(ft_lc, j = "master_label", width = 2.2)
    ft_lc <- width(ft_lc, j = "using_label",  width = 2.2)
    ft_lc <- set_table_properties(ft_lc, layout = "autofit", width = 1)
    doc <- body_add_flextable(doc, ft_lc)
  }

  print(doc, target = path)
  invisible(path)
}


