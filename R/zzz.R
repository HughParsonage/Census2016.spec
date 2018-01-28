.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.0") {
    utils::globalVariables(c("varIused", ".", "Region_key", "Region"), package = pkgname)
  }

  op <- options()
  op.Census2016spec <- list(
    "Census2016.value.name" = "persons"
  )
  toset <- !(names(op.Census2016spec) %in% names(op))
  if (any(toset)) options(op.Census2016spec[toset])
  invisible(NULL)
}


