#' Check measure columns
#' @param dt A \code{data.table}.
#' @param show.progress Be fun and show a progress bar?
#' @export

check_measures <- function(dt, show.progress = TRUE) {
  dt_noms <- copy(names(dt))
  measure_names <- dt_noms[-c(1, length(dt_noms))]

  if (show.progress) {
    msg <- base::cat
  } else {
    msg <- function(...) invisible(NULL)
  }

  for (mnom in measure_names) {
    if (!mnom %chin% names(MEASURE_VARS)) {
      stop("Not a valid name: ", mnom)
    }
    MEASURE_VAR <- MEASURE_VARS[[which(names(MEASURE_VARS) == mnom)]]
    v <- dt[[mnom]]

    if (is.function(MEASURE_VAR)) {
      MEASURE_VAR <- match.fun(MEASURE_VAR)
      if (!MEASURE_VAR(v)) {
        stop(mnom, " did not satisfy ", (MEASURE_VARS[names(MEASURE_VARS) == mnom]))
      }
    } else {
      if (is.list(MEASURE_VAR)) {
        if ("f" %chin% names(MEASURE_VAR)) {
          f_MEASURE_VAR <- match.fun(MEASURE_VAR[["f"]])
          if (!f_MEASURE_VAR(v)) {
            stop(mnom, " did not satisfy ", (MEASURE_VARS[names(MEASURE_VARS) == mnom]))
          }
        } else {
          if (!all(c("class", "permitted_values") %in% names(MEASURE_VAR))) {
            stop("MEASURE VARS did not have class, permitted_values. ", names(MEASURE_VAR))
          }
          if (identical(MEASURE_VAR$class, "double")) {
            if (!is.double(v)) {
              stop(mnom, " was not type double, as specified.")
            }
          } else {
            if (!identical(class(v), MEASURE_VAR$class)) {
              stop(mnom, " did not have class ", MEASURE_VAR$class, " as specified.")
            }
          }
          msg(".")

          if (identical(class(v), c("ordered", "factor"))) {
            if (!startsWith(mnom, "Age")) {
              if (!identical_levels(v, MEASURE_VAR$permitted_values)) {
                stop(mnom, " was an ordered factor but the levels are not as specified.\n",
                     "Levels:\n\t", levels(v), "\n",
                     "Spec:\n\t", MEASURE_VAR$permitted_values)
              }
            }
          } else {
            if (any(v[!is.na(v)] %notin% MEASURE_VAR$permitted_values)) {
              stop(mnom, " contained values not in specification.\n\t",
                   paste0(unique(v[v %notin% MEASURE_VAR$permitted_values]),
                          collapse = "\n\t"),
                   "\n",
                   "Permitted values were:\n\t",
                   paste0(MEASURE_VAR$permitted_values, collapse = "\n\t"))
            }
          }
          msg(".")
        }
      }
    }
  }



}

identical_levels <- function(vx, y) {
  x <- as.character(levels(vx))
  y <- as.character(y)

  if (length(x) != length(y)) {
    if (anyNA(x) && !anyNA(y)) {
      x <- x[!is.na(x)]
    }
    if (anyNA(y) && !anyNA(x)) {
      y <- y[!is.na(y)]
    }
  }

  length(x) == length(y) &&
    all(x == y)
}


