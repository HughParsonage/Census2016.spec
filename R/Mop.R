#' Mop up a data pack file for inclusion a package
#' @param DT A \code{data.table} that separated into the right variables.
#' @param value.name The value column in \code{DT}.
#' @param suborder If column names are not meant to be ordered alphabetically, this specifies a suborder for columns.
#' @param dry.run (logical, default: \code{FALSE}) If \code{TRUE}, the \code{data.table}s are not assigned; the function is run to see if the code will run.
#' @param totals A \code{data.table} which specifies the total for each region key.
#' @param use.varI (logical) If \code{TRUE}, the default, \code{DT} must contain a column
#' \code{varI}, the metadata entries, and these \code{varI} will be excluded from subsequent
#' calls to \code{fread[GTW]}.
#' @param verbose Be chatty and report progress (for debugging)?
#' @param assign_into The environment into which the result is to be assigned. Typically set to the global environment.
#' @return Called for its side-effect: assignment.
#' @export

Mop <- function(DT, value.name = getOption("Census2016.value.name", "persons"), suborder = NULL, dry.run = FALSE, totals = NULL, use.varI = TRUE, verbose = FALSE, assign_into = getOption("Census.Mop.assign.env")) {
  stopifnot(is.data.table(DT),
            nrow(DT) > 0L,
            value.name %in% names(DT),
            hutils::implies(use.varI, "varI" %chin% names(DT)),
            # Should be Completed
            'MaxSchooling' %notin% names(DT))

  if ("Sex" %in% names(DT) &&
      OR(all(c("Persons", "Males", "Females") %chin% DT[["Sex"]]),
         all(c("Persons", "Male", "Female") %chin% DT[["Sex"]]))) {
    Mop(DT[Sex != "Persons"][, Sex := substr(Sex, 0, 1)],
        value.name = value.name, suborder = suborder,
        dry.run = dry.run, totals = totals,
        use.varI = use.varI,
        verbose = verbose)
    Mop(DT[Sex == "Persons"][, Sex := NULL],
        value.name = value.name, suborder = suborder,
        dry.run = dry.run, totals = totals,
        use.varI = use.varI,
        verbose = verbose)
    return(invisible(NULL))
  }

  if (use.varI) {
    .varIused <- sort(unique(c(.subset2(DT, "varI"))))
  }

  # Check for no duplicates, except for value.name
  if (anyDuplicated(DT, by = setdiff(names(DT), c(value.name, "variable", "varI")))) {
    print(duplicated_rows(DT, by = setdiff(names(DT), c(value.name, "variable", "varI"))))
    stop("Duplicated rows.")
  }
  if (verbose) cat("1\n")

  # Are any columns constant?
  for (uj in seq_along(DT)) {
    if (AND(names(DT)[uj] %notin% c("variable", "varI"),
            uniqueN(.subset2(DT, uj)) == 1L)) {
      uv <- vapply(drop_cols(DT, vars = c("variable", "varI")), uniqueN, integer(1L))
      print(names(uv)[uv == 1L])
      stop("Constant columns in DT.")
    }
  }

  out <-
    DT %>%
    drop_col("variable") %>%
    setcolorder(sort(names(.))) %>%
    set_cols_first(c(Region_key)) %>%
    setorderv(setdiff(names(.), value.name)) %>%
    set_cols_last(value.name) %>%
    .[]

  rm(DT)

  if ("CountryOfBirth" %chin% names(out)) {
    out <- decode_country(out)
  }

  if ("LabourForceStatus" %chin% names(out)) {
    out[LabourForceStatus == "Full-time",
        LabourForceStatus := "Employed (full-time)"]
    out[LabourForceStatus == "Part-time",
        LabourForceStatus := "Employed (part-time)"]
    out[LabourForceStatus == "Away from work",
        LabourForceStatus := "Employed (away from work)"]
  }

  if ("EnglishProficiency" %chin% names(out) &&
      !identical(class(out[["EnglishProficiency"]]),
                 c("ordered", "factor"))) {
    out[,
        EnglishProficiency := factor(EnglishProficiency,
                                     levels = c("Speaks English not well or not at all",
                                                "Speaks English well or very well"),
                                     ordered = TRUE)]
  }

  if ("YearOfArrival.max" %chin% names(out)) {
    vYearOfarrival.max <- out[["YearOfArrival.max"]]
    tryCatch(as.integer(vYearOfarrival.max),
             warning = function(e) stop("YearOfArrival.max not coercible to integer."))
    out[, YearOfArrival.max := as.integer(YearOfArrival.max)]
  }


  if (substr(names(out)[1], 0, 3) != "SA1") {
    for (j in names(out)) {
      out[which(.subset2(out, j) == "Other"), (j) := "(Other)"]
    }
  }

  if (is.data.table(totals)) {
    if (!Region_key %chin% names(totals)) {
      print(totals)
      stop("`totals` does not contain name ", Region_key, ".")
    }
    if (verbose) cat("2\n")

    if (value.name %notin% names(totals)) {
      print(totals)
      stop("`totals` does not contain ", value.name, ".")
    }

    cf_population <-
      out[, list(ApparentPopulation = sum(eval(parse(text = value.name)))), keyby = Region_key] %>%
      .[totals, on = Region_key] %>%
      .[, err_r := 0]

    if ("persons" %chin% names(totals)) {
      cf_population[ApparentPopulation <= 5L, err_r := 1 - persons / ApparentPopulation]
      cf_population[persons <= 5L, err_r := 0]
    } else {
      cf_population[ApparentPopulation <= 5L,
                    err_r := 1 - .subset2(cf_population, value.name) / ApparentPopulation]
    }

    wprop_wrong <-
      weighted.mean(abs(.subset2(cf_population, "err_r")) > 0.005 * ncol(out),
                    .subset2(cf_population, value.name))

    cond <- wprop_wrong > 0.05

    if (is.na(cond)) {
      print(cf_population)
      stop("cond NA", call. = FALSE)
    }

    if (cond) {
      print(wprop_wrong)
      tryCatch(
        print(cf_population[,
                            err_r := paste0(if_else(.subset2(cf_population, value.name) < ApparentPopulation,
                                                    "+",
                                                    "-"),
                                            percent(abs(err_r)))]),
        error = function(e) NULL)
      print(out)
      stop("Error too high. Working population too high / low.")
    }
    if (verbose) cat("3\n")
  }


  if (anyDuplicated(out, by = setdiff(names(out), value.name))) {
    print(duplicated_rows(out, by = setdiff(names(out), value.name)))
    stop("Duplicated rows.")
  }
  if (verbose) cat("4\n")

  if (!is.null(suborder)) {
    set_colsuborder(out, suborder)
    setorderv(out, setdiff(names(out), value.name))
  }

  out_noms <- names(out)

  object_name <-
    paste0(Region,
           "__",
           paste0(setdiff(out_noms, c(Region_key,
                                      "persons",
                                      "varI")),
                  collapse = "_"))

  if (object_name %in% ls(envir = .GlobalEnv)) {
    stop("`", object_name, "` already defined.")
  }

  decoded_out <- if (Region %chin% c("SA1", "POA")) out else decodeRegion(out)
  if (!anyDuplicated(decoded_out,
                     by = setdiff(names(decoded_out), value.name))) {
    out <- decoded_out
  } else {
    cat("\n\n\n\n\n")
    print(duplicated_rows(decoded_out,
                          by = setdiff(names(decoded_out),
                                       value.name)))
    cat("\n\n\n\n\n")
  }

  if (use.varI) {
    decoded_out[, varI := NULL]
  }

  if (verbose) cat("5")
  if (!dry.run) {
    if (use.varI) {
      varIused <<- sort(c(varIused, .varIused))
    }

    assign(object_name, out, envir = assign_into)
  } else {
    cat("OK\n")
  }
  if (exists("in_for_loop") &&
      exists("i_for_loop")) {
    i_for_loop <<- i_for_loop + 1L
    cat_object_name <-
      paste0(Region,
             "__",
             paste0(setdiff(out_noms, c(Region_key, value.name,
                                        "persons",
                                        "varI")),
                    collapse = "_"))
    cat(rep_len("=", i_for_loop), cat_object_name, "\r", sep = "")
  }
  decoded_out[]
}
