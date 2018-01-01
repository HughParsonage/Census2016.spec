#' Check measure columns
#' @param dt A \code{data.table}.
#' @importFrom hutils %notin%
#' @export

check_measures <- function(dt) {
  dt_noms <- copy(names(dt))
  measure_names <- dt_noms[-c(1, length(dt_noms))]

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
        if (!all(c("class", "permitted_values") %in% names(MEASURE_VAR))) {
          stop("MEASURE VARS did not have class, permitted_values. ", names(MEASURE_VAR))
        }
        if (identical(MEASURE_VAR$class, "double")) {
          if (!is.double(v)) {
            stop(mnom, " was not type double, as specified.")
          }
        }
        cat(".")

        if (!identical(class(v), MEASURE_VAR$class)) {
          stop(mnom, " did not have class ", MEASURE_VAR$class, " as specified.")
        }
        cat(".")

        if (any(v %notin% MEASURE_VAR$permitted_values)) {
          stop(mnom, " contained values not in specification.\n\t",
               paste0(unique(v[v %notin% MEASURE_VAR$permitted_values]),
                      collapse = "\n\t"),
               "\n",
               "Permitted values were:\n\t",
               paste0(MEASURE_VAR$permitted_values, collapse = "\n\t"))
        }
        cat(".")
      }
    }
  }



}

