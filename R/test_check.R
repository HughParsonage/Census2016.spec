#' Test check
#' @param package The package as a character vector to check.
#' @export

test_check <- function(package) {
  AllData <- list_data(package)

  check_colnames(AllData)
  cat("\n")
  cat(crayon::green("Colnames OK"))

  for (i in seq_along(AllData)) {
    cat("\n")
    cat(names(AllData)[i], ": ")
    dt <- AllData[[i]]
    check_measures(dt)
  }
  cat(crayon::green("Measure vars values OK"))
}


