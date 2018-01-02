
list_data <- function(package, and_load = TRUE) {
  if (!isNamespaceLoaded(package)) {
    library(package, character.only = TRUE)
  }
  all_data <- try(utils::data(package = package))
  class(all_data) <- NULL #packageIQR is dumb
  all_datasets <- all_data$results[, "Item"]

  all_datasets <-
    setdiff(all_datasets,
            c("ISO3166", "Metadata", "NonABS_decoder", "SA16_decoder"))

  if (and_load) {
    full_list <-
      mget(all_datasets,
           envir = as.environment(paste0("package:", package)))

    return(full_list[vapply(full_list, is.data.table, FALSE)])
  } else {
    return(all_datasets)
  }
}

`%notin%` <- Negate("%in%")

