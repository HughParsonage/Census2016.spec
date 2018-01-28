#' Test check
#' @param package The package as a character vector to check.
#' @param data_list Names list of data.tables to check.
#' @param show.progress Be fun and show a progress bar?
#' @export

test_check <- function(package, data_list = NULL, show.progress = TRUE) {
  if (is.null(data_list)) {
    AllData <- list_data(package)
  } else {
    AllData <- data_list
  }

  check_colnames(AllData, show.progress = show.progress)
  if (show.progress) {
    cat("\n")
    cat(crayon::green("Colnames OK"))
  }
  for (i in seq_along(AllData)) {
    if (show.progress) {
      cat("\n")
      cat(names(AllData)[i], ": ")
    }
    dt <- AllData[[i]]
    check_measures(dt, show.progress = show.progress)
  }
  if (show.progress) {
    cat(crayon::green("Measure vars values OK"))
  }
}


