#' Check the column names of tables
#' @param data_list A list of \code{data.tables} to check, or if a string, the name of a package
#' whose data will be checked.
#' @param show.progress Be fun and show a progress bar?
#' @export

check_colnames <- function(data_list, show.progress = TRUE) {
  if (length(data_list) == 1L && is.character(data_list)) {
    data_list <- list_data(data_list, and_load = TRUE)
  }

  if (show.progress) {
    msg <- base::cat
  } else {
    msg <- function(...) invisible(NULL)
  }

  DT_names <- names(data_list)
  i <- 0L
  for (dt in data_list) {
    i <- i + 1L
    msg("\n")
    msg(DT_names[i], ": ", sep = "")

    dt_noms <- names(dt)
    if (!is.null(attributes(dt_noms))) {
      print(dt)
      print(dt_noms)
      stop("Table's names has attributes.")
    }
    msg(".")

    permitted_forenames <-
      c(c("SA1_7DIGITCODE_2016",
          paste0(c("CED", "SED", "LGA", "STE", "SSC", paste0("SA", 2:4)), "_NAME16"),
          paste0(c("POA"), "_CODE_2016")),
        c("SA1_7DIGITCODE_2021",
          "GCCSA_NAME21",
          "SA1_CODE_2021",
          "SA1_internal_2021",
          paste0(c("CED", "SED", "LGA", "STE", "SSC", paste0("SA", 2:4)), "_NAME21"),
          paste0(c("POA"), "_CODE_2021")))

    if (!length(dt_noms)) {
      print(dt)
      stop("No names.")
    }
    msg(".")

    if (!dt_noms[1] %chin% permitted_forenames) {
      print(dt)
      stop("The first name, ", dt_noms[1],
           " is not one of the permitted names:\n\t",
           paste0(permitted_forenames, collapse = "\n\t"))
    }
    msg(".")

    if ("Year" %chin% dt_noms) {
      if (dt_noms[2] != "Year") {
        print(dt)
        stop("Column `Year` occurs in position ", which(dt_noms == "Year"), ". ",
             "It must occur in position 2.")
      }
    }
    msg(".")

    # Measure names
    m_noms <- dt_noms[-c(1, length(dt_noms))]
    m_noms <- m_noms[m_noms != "Year"]

    if (!all(grepl("^([A-Z]|median|mean|average)", m_noms, perl = TRUE))) {
      print(dt)
      stop("`",
           paste0(m_noms[!grepl("^([A-Z]|median|mean)", m_noms, perl = TRUE)], collapse = " "), "`",
           " not in PascalCase.")
    }
    msg(".")

    if (!all(m_noms %chin% names(MEASURE_VARS))) {
      stop(paste0(m_noms[!m_noms %in% names(MEASURE_VARS)], collapse = " "), " not a permitted name.")
    }
    msg(".")
  }
}

