
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Compare two datasets by key and variable values
#'
#' Compare a master dataset and a using dataset across selected variables,
#' optionally keyed by one or more ID columns. The function reports merge-style
#' counts, type mismatches, value mismatches, and value-label conflicts.
#'
#' @param master A data frame containing the master data.
#' @param using A data frame containing the using data, or a path to a CSV file.
#' @param vars Character vector of variables to compare. Use `"_all"` or `NULL`
#'   to compare all variables except the ID columns.
#' @param id Optional character vector of unique ID columns.
#' @param verbose Logical; if `TRUE`, print mismatched records.
#' @param verbose1 Optional positive integer limiting the number of mismatched
#'   rows printed per variable. Also turns on `verbose`.
#' @param all Logical; if `TRUE`, also print variables that match.
#' @param masterlist Logical; if `TRUE`, print IDs found only in `master`.
#' @param usinglist Logical; if `TRUE`, print IDs found only in `using`.
#' @param separator Optional positive integer controlling grouped printing of
#'   mismatch output.
#' @param sepby Optional character vector of columns used to sort mismatch output.
#' @param reldif Optional numeric tolerance for relative difference comparisons
#'   on numeric variables.
#' @param nodecrease Logical; only report relative-difference mismatches where
#'   the master value is greater than or equal to the using value.
#' @param label_conflicts Logical; if `TRUE`, detect value-label conflicts.
#' @param print_label_conflicts Logical; if `TRUE`, print detected label conflicts.
#'
#' @returns A list of class `"corcf_result"` containing merge counts, error code,
#'   mismatch summaries, per-variable results, label conflicts, and variable
#'   availability summaries.
#' @keywords internal
#' @import data.table
"_PACKAGE"
#' @export
#'
#' @examples
#' master_df <- data.frame(
#'   id = c(1, 2, 3),
#'   visitdate = c("2024-01-01", "2024-01-02", "2024-01-03"),
#'   x = c(10, 20, 30),
#'   stringsAsFactors = FALSE
#' )
#'
#' using_df <- data.frame(
#'   id = c(1, 2, 3),
#'   visitdate = c("2024-01-01", "2024-01-02", "2024-01-03"),
#'   x = c(10, 21, 30),
#'   stringsAsFactors = FALSE
#' )
#'
#' res <- corcf(
#'   master = master_df,
#'   using = using_df,
#'   vars = "x",
#'   id = c("id", "visitdate")
#' )
#'
#' res$ecode
#' res$value_mismatch
corcf <- function(master,
                  using,
                  vars = NULL,
                  id = NULL,
                  verbose = FALSE,
                  verbose1 = NULL,
                  all = FALSE,
                  masterlist = FALSE,
                  usinglist = FALSE,
                  separator = NULL,
                  sepby = NULL,
                  reldif = NULL,
                  nodecrease = FALSE,
                  label_conflicts = TRUE,
                  print_label_conflicts = TRUE) {

  stop198 <- function(msg) stop(msg, call. = FALSE)

  is_labelled_any <- function(x) {
    inherits(x, c("haven_labelled", "labelled", "haven_labelled_spss", "labelled_spss"))
  }

  get_labels <- function(x) {
    labs <- attr(x, "labels")
    if (is.null(labs)) {
      return(stats::setNames(character(), character()))
    }
    stats::setNames(names(labs), as.character(unname(labs)))
  }

  label_conflicts_for_pair <- function(x, y) {
    mx <- get_labels(x)
    my <- get_labels(y)

    if (length(mx) == 0L || length(my) == 0L) {
      return(NULL)
    }

    common_codes <- intersect(names(mx), names(my))
    if (!length(common_codes)) {
      return(NULL)
    }

    bad <- common_codes[mx[common_codes] != my[common_codes]]
    if (!length(bad)) {
      return(NULL)
    }

    data.frame(
      code = bad,
      master_label = unname(mx[bad]),
      using_label = unname(my[bad]),
      stringsAsFactors = FALSE
    )
  }

  zap_labels_one <- function(x) {
    if (!is_labelled_any(x)) {
      return(x)
    }
    attr(x, "labels") <- NULL
    attr(x, "label") <- NULL
    class(x) <- setdiff(
      class(x),
      c("haven_labelled", "labelled", "haven_labelled_spss", "labelled_spss")
    )
    x
  }

  to_base <- function(x) {
    if (is.factor(x)) {
      return(as.character(x))
    }
    if (requireNamespace("vctrs", quietly = TRUE) && inherits(x, "vctrs_vctr")) {
      x <- vctrs::vec_data(x)
    }
    keep_names <- names(x)
    attributes(x) <- NULL
    if (!is.null(keep_names)) {
      names(x) <- keep_names
    }
    x
  }

  is_stringish <- function(x) {
    is.character(x) || is.factor(x)
  }

  reldif_vec <- function(a, b) {
    out <- rep(NA_real_, length(a))
    ok2 <- !(is.na(a) | is.na(b))
    idx <- which(ok2)

    if (length(idx)) {
      aa <- a[idx]
      bb <- b[idx]
      denom <- pmax(abs(aa), abs(bb))
      rd <- ifelse(denom == 0, 0, abs(aa - bb) / denom)
      out[idx] <- rd
    }

    out
  }

  eq_stata <- function(a, b) {
    a0 <- to_base(a)
    b0 <- to_base(b)

    same_na <- is.na(a0) & is.na(b0)
    both_non_na <- !(is.na(a0) | is.na(b0))

    out <- rep(FALSE, length(a0))
    out[same_na] <- TRUE
    out[both_non_na] <- a0[both_non_na] == b0[both_non_na]
    out
  }

  print_mismatches <- function(df, id, sepby, master_col, using_col, max_rows, separator) {
    cols <- c(id, sepby, master_col, using_col)
    cols <- cols[!is.na(cols) & nzchar(cols)]
    cols <- unique(cols)
    cols <- cols[cols %in% names(df)]
    out <- df[, cols, drop = FALSE]

    if (!is.null(sepby) && length(sepby) > 0L && all(sepby %in% names(out))) {
      ord <- do.call(order, out[, sepby, drop = FALSE])
      out <- out[ord, , drop = FALSE]
    }

    if (!is.null(max_rows) && max_rows > 0L && nrow(out) > max_rows) {
      out <- out[seq_len(max_rows), , drop = FALSE]
    }

    if (!is.null(separator) && is.numeric(separator) && separator > 0L && nrow(out) > 0L) {
      n <- nrow(out)
      starts <- seq(1L, n, by = separator)
      for (s in starts) {
        e <- min(s + separator - 1L, n)
        print(out[s:e, , drop = FALSE], row.names = FALSE)
        if (e < n) {
          cat(strrep("-", 60), "\n")
        }
      }
    } else {
      print(out, row.names = FALSE)
    }
  }

  if (!is.null(separator) || !is.null(sepby)) {
    if (!is.null(separator) && !is.null(sepby)) {
      stop198("options separator() and sepby() may not be combined")
    }
    if (!is.null(separator)) {
      if (!(is.numeric(separator) && length(separator) == 1L && separator == as.integer(separator))) {
        stop198("option separator() must be a single integer")
      }
      if (separator < 1L) {
        stop198("option separator() must be >= 1")
      }
    }
  }

  if (!is.null(verbose1)) {
    if (!(is.numeric(verbose1) &&
          length(verbose1) == 1L &&
          verbose1 == as.integer(verbose1) &&
          verbose1 > 0L)) {
      stop198("option verbose1() requires an integer > 0")
    }
    verbose <- TRUE
  }

  if (nodecrease && is.null(reldif)) {
    stop198("option nodecrease may only be specified with option reldif()")
  }

  max_print <- if (!is.null(verbose1)) as.integer(verbose1) else 0L

  if (!is.data.frame(master)) {
    stop198("master must be a data.frame")
  }

  using_df <- using
  if (is.character(using) && length(using) == 1L) {
    using_df <- tryCatch(
      utils::read.csv(using, stringsAsFactors = FALSE),
      error = function(e) stop198(paste0("Failed to read using file: ", e$message))
    )
  }

  if (!is.data.frame(using_df)) {
    stop198("using must be a data.frame or a file path to a CSV")
  }

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop198("data.table package is required")
  }

  master_dt <- data.table::as.data.table(master)
  using_dt <- data.table::as.data.table(using_df)

  id <- if (is.null(id) || length(id) == 0L) NULL else as.character(id)
  sepby <- if (is.null(sepby) || length(sepby) == 0L) NULL else as.character(sepby)

  if (is.null(vars) || (is.character(vars) && length(vars) == 1L && vars == "_all")) {
    vars <- setdiff(names(master_dt), id %||% character())
  } else {
    vars <- as.character(vars)
    if (length(vars) < 1L) {
      stop198("vars must be a non-empty character vector")
    }
  }

  obs_master <- nrow(master_dt)
  obs_using <- nrow(using_dt)

  if (obs_master != obs_using) {
    message(sprintf(
      "note: master has %d observations; using has %d observations",
      obs_master, obs_using
    ))
  }

  keep_master <- unique(c(id, vars))
  keep_master <- keep_master[keep_master %in% names(master_dt)]
  master_k <- master_dt[, keep_master, with = FALSE]

  vars_missing_in_master <- setdiff(names(using_dt), names(master_dt))

  notexist <- vars[!(vars %in% names(using_dt))]
  vars_missing_in_using <- notexist

  if (length(notexist) > 0L) {
    for (v in notexist) {
      message(sprintf("%s: does not exist in using", v))
    }
    vars <- setdiff(vars, notexist)
    if (length(vars) == 0L) {
      stop198("No common variables")
    }
  }

  if (length(vars_missing_in_master) > 0L) {
    for (v in vars_missing_in_master) {
      message(sprintf("%s: exists in using but not in master", v))
    }
  }

  label_conflict_list <- list()
  if (isTRUE(label_conflicts)) {
    common_vars <- intersect(vars, intersect(names(master_dt), names(using_dt)))
    for (v in common_vars) {
      conf <- label_conflicts_for_pair(master_dt[[v]], using_dt[[v]])
      if (!is.null(conf)) {
        label_conflict_list[[v]] <- conf
        if (isTRUE(print_label_conflicts)) {
          codes <- paste(unique(conf$code), collapse = ", ")
          message(sprintf("value label conflict in %s: codes {%s}", v, codes))
        }
      }
    }
  }

  if (is.null(id)) {
    master_k[, .rowkey := .I]
    using_k <- using_dt[, unique(vars), with = FALSE]
    using_k[, .rowkey := .I]
    key <- ".rowkey"
    comp_label <- "records"
  } else {
    missing_id_master <- setdiff(id, names(master_dt))
    missing_id_using <- setdiff(id, names(using_dt))

    if (length(missing_id_master) > 0L) {
      stop198(paste0(
        "ID columns missing in master: ",
        paste(missing_id_master, collapse = ", ")
      ))
    }

    if (length(missing_id_using) > 0L) {
      stop198(paste0(
        "ID columns missing in using: ",
        paste(missing_id_using, collapse = ", ")
      ))
    }

    keep_using <- unique(c(id, vars))
    keep_using <- keep_using[keep_using %in% names(using_dt)]
    using_k <- using_dt[, keep_using, with = FALSE]

    key <- id
    comp_label <- "IDs"
  }

  master_k[, .present_m := TRUE]
  using_k[, .present_u := TRUE]

  vars_cmp <- setdiff(vars, key)

  master_ren <- data.table::copy(master_k)
  using_ren <- data.table::copy(using_k)

  if (length(vars_cmp) > 0L) {
    m_rename <- intersect(vars_cmp, names(master_ren))
    u_rename <- intersect(vars_cmp, names(using_ren))

    data.table::setnames(master_ren, m_rename, paste0(m_rename, ".m"))
    data.table::setnames(using_ren, u_rename, paste0(u_rename, ".u"))
  }

  cols_m <- setdiff(names(master_ren), key)
  cols_u <- setdiff(names(using_ren), key)

  master_ren[, (cols_m) := lapply(.SD, zap_labels_one), .SDcols = cols_m]
  using_ren[, (cols_u) := lapply(.SD, zap_labels_one), .SDcols = cols_u]

  if (!is.null(key)) {
    master_ren[, (key) := lapply(.SD, as.character), .SDcols = key]
    using_ren[, (key) := lapply(.SD, as.character), .SDcols = key]
  }

  merged <- merge(master_ren, using_ren, by = key, all = TRUE)

  master_present <- !is.na(merged[[".present_m"]])
  using_present <- !is.na(merged[[".present_u"]])

  source <- ifelse(
    master_present & !using_present, 1L,
    ifelse(!master_present & using_present, 2L, 3L)
  )

  if (!is.null(id)) {
    if (masterlist) {
      n_monly <- sum(source == 1L, na.rm = TRUE)
      if (n_monly > 0L) {
        message("Master-only IDs")
        print(merged[source == 1L, id, with = FALSE], row.names = FALSE)
      } else {
        message("No master-only IDs")
      }
    }

    if (usinglist) {
      n_uonly <- sum(source == 2L, na.rm = TRUE)
      if (n_uonly > 0L) {
        message("Using-only IDs")
        print(merged[source == 2L, id, with = FALSE], row.names = FALSE)
      } else {
        message("No using-only IDs")
      }
    }
  }

  N_master_only <- sum(source == 1L, na.rm = TRUE)
  N_using_only <- sum(source == 2L, na.rm = TRUE)
  N_matched <- sum(source == 3L, na.rm = TRUE)
  N_notmatched <- N_master_only + N_using_only

  message(sprintf("    Result%26sNumber of obs", ""))
  message("    -----------------------------------------")
  message(sprintf("    Not matched%22s%15s", "", format(N_notmatched, big.mark = ",")))
  message(sprintf("        from master%18s%15s  (_merge==1)", "", format(N_master_only, big.mark = ",")))
  message(sprintf("        from using%19s%15s  (_merge==2)", "", format(N_using_only, big.mark = ",")))
  message("")
  message(sprintf("    Matched%26s%15s  (_merge==3)", "", format(N_matched, big.mark = ",")))
  message("    -----------------------------------------")

  common <- merged[source == 3L]
  message(sprintf("Comparison of common %s follows", comp_label))

  type_mismatch <- 0L
  value_mismatch <- 0L
  nomatch <- 0L
  per_var <- list()
  ecode <- 0L

  for (v in vars_cmp) {
    vm <- paste0(v, ".m")
    vu <- paste0(v, ".u")

    if (!(vm %in% names(common)) || !(vu %in% names(common))) {
      nomatch <- nomatch + 1L
      per_var[[v]] <- list(status = "not_comparable", reason = "missing merged columns")
      next
    }

    tm_str <- is_stringish(to_base(common[[vm]]))
    tu_str <- is_stringish(to_base(common[[vu]]))

    if (tm_str != tu_str) {
      nomatch <- nomatch + 1L
      type_mismatch <- type_mismatch + 1L

      message(sprintf(
        "variable %s is %s in master but %s in using data",
        v,
        if (tm_str) "string" else "numeric",
        if (tu_str) "string" else "numeric"
      ))

      per_var[[v]] <- list(
        status = "type_mismatch",
        master_type = class(common[[vm]]),
        using_type = class(common[[vu]])
      )
      ecode <- 106L
      next
    }

    if (!is.null(reldif) && !tm_str) {
      a <- suppressWarnings(as.numeric(common[[vm]]))
      b <- suppressWarnings(as.numeric(common[[vu]]))
      rd <- reldif_vec(a, b)

      both_na <- is.na(common[[vm]]) & is.na(common[[vu]])
      one_na <- xor(is.na(common[[vm]]), is.na(common[[vu]]))

      cond <- (!both_na) & (one_na | (!is.na(rd) & rd > reldif))
      if (nodecrease) {
        cond <- cond & (a >= b)
      }

      mism_idx <- which(cond)
    } else {
      eq <- eq_stata(common[[vm]], common[[vu]])
      mism_idx <- which(!eq)
    }

    n_mism <- length(mism_idx)

    if (n_mism > 0L) {
      nomatch <- nomatch + 1L
      value_mismatch <- value_mismatch + 1L
      message(sprintf("%s: %d mismatches", v, n_mism))
      per_var[[v]] <- list(status = "mismatch", n = n_mism)

      if (verbose) {
        listing <- common[mism_idx]
        listing[, master_data := get(vm)]
        listing[, using_data := get(vu)]

        print_mismatches(
          df = as.data.frame(listing),
          id = if (is.null(id)) ".rowkey" else id,
          sepby = sepby,
          master_col = "master_data",
          using_col = "using_data",
          max_rows = if (max_print > 0L) max_print else NULL,
          separator = separator
        )
        cat("\n\n")
      }

      if (ecode == 0L) {
        ecode <- 9L
      }
    } else {
      if (all) {
        message(sprintf("%s: match", v))
      }
      per_var[[v]] <- list(status = "match", n = 0L)
    }
  }

  structure(
    list(
      merge_counts = list(
        not_matched = N_notmatched,
        master_only = N_master_only,
        using_only = N_using_only,
        matched = N_matched
      ),
      ecode = ecode,
      n_master = obs_master,
      n_using = obs_using,
      vars_compared = vars_cmp,
      id = id,
      type_mismatch = type_mismatch,
      value_mismatch = value_mismatch,
      nomatch = nomatch,
      per_variable = per_var,
      label_conflicts = label_conflict_list,
      vars_missing_in_using = vars_missing_in_using,
      vars_missing_in_master = vars_missing_in_master
    ),
    class = "corcf_result"
  )
}
