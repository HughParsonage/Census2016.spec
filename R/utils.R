
list_data <- function(package, and_load = TRUE) {
  all_data <- try(utils::data(package = package))
  class(all_data) <- NULL #packageIQR is dumb
  all_datasets <- all_data$results[, "Item"]
  if (and_load) {
    full_list <-
      mget(all_datasets,
           envir = as.environment(paste0("package:", package)))

    return(full_list[vapply(full_list, is.data.table, FALSE)])
  } else {
    return(all_datasets)
  }
}

