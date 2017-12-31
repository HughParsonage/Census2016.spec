#' Check the column names of tables
#' @param package Quoted name of the package to test.
#'
#'

check_colnames <- function(data_list) {
  for (dt in data_list) {
    dt_noms <- names(dt)
    if (!is.null(attributes(dt_noms))) {
      print(dt)
      print(dt_noms)
      stop("Table's names has attributes.")
    }

    permitted_forenames <-
      c("SA1_7DIGITCODE_2016",
        paste0(c("CED", "LGA", "STE", "SSC", paste0("SA", 2:4)), "_NAME16"),
        paste0(c("POA", "SED"), "_CODE_2016"))

    if (!dt_noms[1] %chin% permitted_forenames) {
      print(dt)
      stop("The first name, ", dt_noms[1],
           " is not one of the permitted names:\n\t",
           paste0(permitted_forenames, collapse = "\n\t"))
    }

    if ("Year" %chin% dt_noms) {
      if (dt_noms[2] != "Year") {
        print(dt)
        stop("Column `Year` occurs in position ", which(dt_noms == "Year"), ". ",
             "It must occur in position 2.")
      }
    }





  }
}

