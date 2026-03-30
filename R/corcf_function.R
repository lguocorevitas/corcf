#'#'#' Title
#'
#' @param master
#' @param using
#' @param vars
#' @param id
#' @param verbose
#' @param verbose1
#' @param all
#' @param masterlist
#' @param usinglist
#' @param separator
#' @param sepby
#' @param reldif
#' @param nodecrease
#' @param label_conflicts
#' @param print_label_conflicts
#'
#' @returns
#' @export
#'
#' @examples
#' library(stringr)
#' library(haven)
#' library(lubridate)
#' library(glue)
#' library(tidyverse)
#' for word output
#' library(officer)
#' library(flextable)
#' tdy_date <- Sys.Date()
#' tdy_year  <- sprintf("%04d", year(tdy_date))
#' tdy_month <- sprintf("%02d", month(tdy_date))
#' cut_date    <- as.Date(glue("{tdy_year}-{tdy_month}-01"))
#' sharepoint <- "~/../../Thermo Fisher Scientific/"
#' dir_ra_monthly  <- glue("{sharepoint}/Biostat Data Files - RA/monthly/")
#' analytic_data  <- glue("{dir_ra_monthly}/{tdy_year}/{cut_date}/")

#' data_name <-"visits_calc"
#' test_data <- glue("{analytic_data}/rewrite/RA_{data_name}_{cut_date}.dta")
#' current_data  <- glue("{analytic_data}/2_3_keyvisitvars_{cut_date}.dta")
#' id_cols <- c("id", "visitdate")
#'
#' master_df <- read_dta(test_data)
#' using_df  <- read_dta(current_data)
#' if (!"id" %in% names(using_df)) using_df <- using_df %>% mutate(id = subject_number)

#' res <- corcf(
#'  master = master_df,
#'  using  = using_df,
#'  vars   = "_all",
#'  id     = id_cols
#'  # ,verbose1 = 200
#' )
#'
#' names(res$label_conflicts)
#' res$ecode # if code =106 means you hit at least one type mismatch
#' names(res$label_conflicts)

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

  `%||%` <- function(a, b) if (!is.null(a)) a else b
  stop198 <- function(msg) stop(msg, call. = FALSE)

  # ---------- label helpers ----------
  is_labelled_any <- function(x) inherits(x, c("haven_labelled", "labelled", "haven_labelled_spss", "labelled_spss"))

  get_labels <- function(x) {
    labs <- attr(x, "labels")
    if (is.null(labs)) return(setNames(character(), character()))
    # map: names = code (chr), values = label text
    setNames(names(labs), as.character(unname(labs)))
  }

  label_conflicts_for_pair <- function(x, y) {
    mx <- get_labels(x)
    my <- get_labels(y)
    if (length(mx) == 0L || length(my) == 0L) return(NULL)

    common_codes <- intersect(names(mx), names(my))
    if (!length(common_codes)) return(NULL)

    bad <- common_codes[mx[common_codes] != my[common_codes]]
    if (!length(bad)) return(NULL)

    data.frame(
      code = bad,
      master_label = unname(mx[bad]),
      using_label  = unname(my[bad]),
      stringsAsFactors = FALSE
    )
  }

  zap_labels_one <- function(x) {
    if (!is_labelled_any(x)) return(x)
    attr(x, "labels") <- NULL
    attr(x, "label")  <- NULL
    class(x) <- setdiff(class(x), c("haven_labelled", "labelled", "haven_labelled_spss", "labelled_spss"))
    x
  }

  # ---------- comparison helpers ----------
  to_base <- function(x) {
    if (is.factor(x)) return(as.character(x))
    if (requireNamespace("vctrs", quietly = TRUE) && inherits(x, "vctrs_vctr")) {
      x <- vctrs::vec_data(x)
    }
    keep_names <- names(x)
    attributes(x) <- NULL
    if (!is.null(keep_names)) names(x) <- keep_names
    x
  }

  is_stringish <- function(x) is.character(x) || is.factor(x)

  reldif_vec <- function(a, b) {
    out <- rep(NA_real_, length(a))
    ok2 <- !(is.na(a) | is.na(b))
    idx <- which(ok2)
    if (length(idx)) {
      aa <- a[idx]; bb <- b[idx]
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
    cols <- cols[!is.null(cols) & nzchar(cols)]
    cols <- unique(cols)
    cols <- cols[cols %in% names(df)]
    out <- df[, cols, drop = FALSE]

    if (!is.null(sepby) && length(sepby) > 0 && all(sepby %in% names(out))) {
      ord <- do.call(order, out[, sepby, drop = FALSE])
      out <- out[ord, , drop = FALSE]
    }

    if (!is.null(max_rows) && max_rows > 0 && nrow(out) > max_rows) {
      out <- out[seq_len(max_rows), , drop = FALSE]
    }

    if (!is.null(separator) && is.numeric(separator) && separator > 0 && nrow(out) > 0) {
      n <- nrow(out)
      starts <- seq(1, n, by = separator)
      for (s in starts) {
        e <- min(s + separator - 1, n)
        print(out[s:e, , drop = FALSE], row.names = FALSE)
        if (e < n) cat(strrep("-", 60), "\n")
      }
    } else {
      print(out, row.names = FALSE)
    }
  }

  # ---------- option checks ----------
  if (!is.null(separator) || !is.null(sepby)) {
    if (!is.null(separator) && !is.null(sepby)) stop198("options separator() and sepby() may not be combined")
    if (!is.null(separator)) {
      if (!(is.numeric(separator) && length(separator) == 1 && separator == as.integer(separator))) {
        stop198("option separator() must be a single integer")
      }
      if (separator < 1) stop198("option separator() must be >= 1")
    }
  }

  if (!is.null(verbose1)) {
    if (!(is.numeric(verbose1) && length(verbose1) == 1 && verbose1 == as.integer(verbose1) && verbose1 > 0)) {
      stop198("option verbose1() requires an integer >0")
    }
    verbose <- TRUE
  }
  max_print <- if (!is.null(verbose1)) as.integer(verbose1) else 0

  if (nodecrease && is.null(reldif)) stop198("option nodecrease may only be specified with option reldif()")

  # ---------- accept inputs; coerce to data.table safely ----------
  if (!is.data.frame(master)) stop198("master must be a data.frame")
  using_df <- using
  if (is.character(using) && length(using) == 1) {
    using_df <- tryCatch(utils::read.csv(using, stringsAsFactors = FALSE),
                         error = function(e) stop198(paste0("Failed to read using file: ", e$message)))
  }
  if (!is.data.frame(using_df)) stop198("using must be a data.frame or a file path to a CSV")

  if (!requireNamespace("data.table", quietly = TRUE)) stop198("data.table package is required")
  master_dt <- data.table::as.data.table(master)
  using_dt  <- data.table::as.data.table(using_df)

  id <- if (is.null(id) || length(id) == 0) NULL else as.character(id)
  sepby <- if (is.null(sepby) || length(sepby) == 0) NULL else as.character(sepby)

  # vars: NULL / "_all" means all except id
  if (is.null(vars) || (is.character(vars) && length(vars) == 1 && vars == "_all")) {
    vars <- setdiff(names(master_dt), id %||% character())
  } else {
    vars <- as.character(vars)
    if (length(vars) < 1) stop198("vars must be a non-empty character vector")
  }

  # ---------- initial checks ----------
  obs_master <- nrow(master_dt)
  obs_using  <- nrow(using_dt)
  if (obs_master != obs_using) {
    message(sprintf("note: master has %d observations; using has %d observations", obs_master, obs_using))
  }

  # ---------- keep columns (data.table-safe) ----------
  keep_master <- unique(c(id, vars))
  keep_master <- keep_master[keep_master %in% names(master_dt)]
  master_k <- master_dt[, ..keep_master]

#   adding a list of vars that NOT in master
  # Variables present in using but absent in master
  vars_missing_in_master <- setdiff(names(using_dt), names(master_dt))

  # Drop vars not in using
  notexist <- vars[!(vars %in% names(using_dt))]
  vars_missing_in_using <- notexist
  if (length(notexist) > 0) {
    for (v in notexist) message(sprintf("%s: does not exist in using", v))
    vars <- setdiff(vars, notexist)
    if (length(vars) == 0) stop198("No common variables")
  }
  if (length(vars_missing_in_master) > 0) {
    for (v in vars_missing_in_master) {
      message(sprintf("%s: exists in using but not in master", v))
    }
  }
  # ---------- label conflicts (single pass) ----------
  label_conflict_list <- list()
  if (isTRUE(label_conflicts)) {
    for (v in intersect(vars, intersect(names(master_dt), names(using_dt)))) {
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

  # ---------- keying ----------
  if (is.null(id)) {
    master_k[, .rowkey := .I]
    using_k <- using_dt[, unique(vars), with = FALSE]
    using_k[, .rowkey := .I]
    key <- ".rowkey"
    comp_label <- "records"
  } else {
    missing_id_master <- setdiff(id, names(master_dt))
    missing_id_using  <- setdiff(id, names(using_dt))
    if (length(missing_id_master) > 0) stop198(paste0("ID columns missing in master: ", paste(missing_id_master, collapse = ", ")))
    if (length(missing_id_using)  > 0) stop198(paste0("ID columns missing in using: ",  paste(missing_id_using,  collapse = ", ")))

    keep_using <- unique(c(id, vars))
    keep_using <- keep_using[keep_using %in% names(using_dt)]
    using_k <- using_dt[, ..keep_using]

    key <- id
    comp_label <- "IDs"
  }

  # presence flags
  master_k[, .present_m := TRUE]
  using_k[,  .present_u := TRUE]

  # ---------- suffix compare vars but NEVER suffix keys ----------
  vars_cmp <- setdiff(vars, key)

  master_ren <- data.table::copy(master_k)
  using_ren  <- data.table::copy(using_k)

  if (length(vars_cmp)) {
    data.table::setnames(master_ren, intersect(vars_cmp, names(master_ren)),
                         paste0(intersect(vars_cmp, names(master_ren)), ".m"))
    data.table::setnames(using_ren,  intersect(vars_cmp, names(using_ren)),
                         paste0(intersect(vars_cmp, names(using_ren)),  ".u"))
  }

  # ---------- remove labels for merge safely (no [[<- loops) ----------
  cols_m <- setdiff(names(master_ren), key)
  cols_u <- setdiff(names(using_ren),  key)

  master_ren[, (cols_m) := lapply(.SD, zap_labels_one), .SDcols = cols_m]
  using_ren[,  (cols_u) := lapply(.SD, zap_labels_one), .SDcols = cols_u]

  # ensure key columns are base to avoid vctrs/labelled merge issues
  if (!is.null(key)) {
    master_ren[, (key) := lapply(.SD, as.character), .SDcols = key]
    using_ren[,  (key) := lapply(.SD, as.character), .SDcols = key]
  }

  # ---------- merge ----------
  merged <- merge(master_ren, using_ren, by = key, all = TRUE)

  master_present <- !is.na(merged[[".present_m"]])
  using_present  <- !is.na(merged[[".present_u"]])

  source <- ifelse(master_present & !using_present, 1L,
                   ifelse(!master_present & using_present, 2L, 3L))

  # optional lists
  if (!is.null(id)) {
    if (masterlist) {
      n_monly <- sum(source == 1L, na.rm = TRUE)
      if (n_monly > 0) {
        message("Master-only IDs")
        print(merged[source == 1L, ..id], row.names = FALSE)
      } else message("No master-only IDs")
    }
    if (usinglist) {
      n_uonly <- sum(source == 2L, na.rm = TRUE)
      if (n_uonly > 0) {
        message("Using-only IDs")
        print(merged[source == 2L, ..id], row.names = FALSE)
      } else message("No using-only IDs")
    }
  }

  # merge-style counts
  N_master_only <- sum(source == 1L, na.rm = TRUE)
  N_using_only  <- sum(source == 2L, na.rm = TRUE)
  N_matched     <- sum(source == 3L, na.rm = TRUE)
  N_notmatched  <- N_master_only + N_using_only

  message(sprintf("    Result%26sNumber of obs", ""))
  message("    -----------------------------------------")
  message(sprintf("    Not matched%22s%15s", "", format(N_notmatched, big.mark=",")))
  message(sprintf("        from master%18s%15s  (_merge==1)", "", format(N_master_only, big.mark=",")))
  message(sprintf("        from using%19s%15s  (_merge==2)", "", format(N_using_only, big.mark=",")))
  message("")
  message(sprintf("    Matched%26s%15s  (_merge==3)", "", format(N_matched, big.mark=",")))
  message("    -----------------------------------------")

  common <- merged[source == 3L]
  message(sprintf("Comparison of common %s follows", comp_label))

  # ---------- var-by-var checks ----------
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
      per_var[[v]] <- list(status = "type_mismatch",
                           master_type = class(common[[vm]]),
                           using_type  = class(common[[vu]]))
      ecode <- 106L
      next
    }

    # value comparison
    if (!is.null(reldif) && !tm_str) {
      a <- suppressWarnings(as.numeric(common[[vm]]))
      b <- suppressWarnings(as.numeric(common[[vu]]))
      rd <- reldif_vec(a, b)

      both_na <- is.na(common[[vm]]) & is.na(common[[vu]])
      one_na  <- xor(is.na(common[[vm]]), is.na(common[[vu]]))

      cond <- (!both_na) & (one_na | (!is.na(rd) & rd > reldif))
      if (nodecrease) cond <- cond & (a >= b)

      mism_idx <- which(cond)
    } else {
      eq <- eq_stata(common[[vm]], common[[vu]])
      mism_idx <- which(!eq)
    }

    n_mism <- length(mism_idx)

    if (n_mism > 0) {
      nomatch <- nomatch + 1L
      value_mismatch <- value_mismatch + 1L
      message(sprintf("%s: %d mismatches", v, n_mism))
      per_var[[v]] <- list(status = "mismatch", n = n_mism)

      if (verbose) {
        listing <- common[mism_idx]
        listing[, master_data := get(vm)]
        listing[, using_data  := get(vu)]

        print_mismatches(
          df = as.data.frame(listing),
          id = if (is.null(id)) ".rowkey" else id,
          sepby = sepby,
          master_col = "master_data",
          using_col  = "using_data",
          max_rows = if (max_print > 0) max_print else NULL,
          separator = separator
        )
        cat("\n\n")
      }

      if (ecode == 0L) ecode <- 9L
    } else {
      if (all) message(sprintf("%s: match", v))
      per_var[[v]] <- list(status = "match", n = 0L)
    }
  }

  structure(
    list(
      merge_counts = list(
        not_matched = N_notmatched,
        master_only = N_master_only,
        using_only  = N_using_only,
        matched     = N_matched
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
