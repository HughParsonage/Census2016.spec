#' Extract arbitrary variable from data pack
#' @param DT A molten version of the raw data pack files.
#' @param var A string, one of the names \code{MEASURE_VARS}, the variable to be extracted.
#' @export extract_var

extract_var <- function(DT, var, ...) {
  stopifnot(var %chin% names(MEASURE_VARS),
            "permitted_values" %chin% names(MEASURE_VARS[[var]]),
            var %chin% names(DT) || "variable" %chin% names(DT))

  if (var %chin% names(DT)) {
    set(DT,
        j = var,
        value = zmatch(.subset2(DT, var),
                       y = MEASURE_VARS[[var]][["permitted_values"]],
                       ...))
  } else {
    DT[, (var) := zmatch(.subset2(DT, "variable"),
                         y = MEASURE_VARS[[var]][["permitted_values"]],
                         ...)]
  }

  DT[]
}

zmatch <- function(x, y,
                   extract1 = "^(.*)$",
                   delete.penalty = 0.01,
                   sub.penalty = 0.2,
                   y.complete = TRUE) {
  x <- sub(extract1, "\\1", x, perl = TRUE)
  Y <- gsub("[^A-Za-z]+", "_", y)
  distance_matrix <-
    adist(x, Y,
          costs = list(deletions = delete.penalty,
                       substitutions = sub.penalty))

  indexes <- apply(distance_matrix, 1, which.min)
  out <- y[indexes]
  if (y.complete && any(y[!is.na(y)] %notin% out)) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]

    unmatched_x <- x[x %notin% y]
    stop("Not all y were matched.\n\t",
         paste0(unique(y[y %notin% out]), sep = "\n\t"))
  }

  out
}


