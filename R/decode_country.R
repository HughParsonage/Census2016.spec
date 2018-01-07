#' Decode country to ISO-3166
#' @param out A \strong{data.table} containing a column \code{CountryOfBirth}.
#' @return \code{out} with countries decoded to ISO-3166.
#' @export decode_country


decode_country <- function(out) {
  stopifnot(is.data.table(out),
            "CountryOfBirth" %in% names(out))
  out[, CountryOfBirth := trimws(gsub("_", " ", CountryOfBirth))]
  out[, CountryOfBirth := gsub("(?<!(F))Ye?a?r.*arrival.ns$", "", CountryOfBirth, ignore.case = TRUE)]
  out[CountryOfBirth %pin% "(Born )?[Ee]lsewhere", CountryOfBirth := NA_character_]
  out[CountryOfBirth %pin% "Bosnia", CountryOfBirth := "Bosnia and Herzegovina"]
  out[CountryOfBirth %pin% "Korea", CountryOfBirth := "KOR"]
  out[CountryOfBirth %pin% c("N[a-z]+n Ireland",
                             "England",
                             "Scotland",
                             "Wales"),
      CountryOfBirth := "United Kingdom"]
  out[CountryOfBirth %pin% "SE Europe nfd", CountryOfBirth := NA_character_]
  out[CountryOfBirth == "Vietnam", CountryOfBirth := "VNM"]
  # Negative lookbehind due to FYROM (Macedonia)


  out[CountryOfBirth %pin% "FYROM",                   CountryOfBirth := "MKD"]
  out[CountryOfBirth %pin% "China excl? (and )?SARs?.*Ta",  CountryOfBirth := "CHN"]
  out[CountryOfBirth %pin% c("USA", "United States"), CountryOfBirth := "USA"]
  out[CountryOfBirth %pin% "Ho?ng Ko?ng",             CountryOfBirth := "Hong Kong"]
  out[, CountryOfBirth := .decode_country(CountryOfBirth)]
  out[]
}



.decode_country <- function(x, to = "alpha-3") {
  xna <- is.na(x)
  x[!xna] <- trimws(x[!xna])
  stopifnot(to == "alpha-3",
            names(ISO3166)[1] == "name")
  nom <- ISO3166[["name"]]
  a3 <- ISO3166[["alpha_3"]]
  keep <- and(nchar(x, keepNA = FALSE) == 3L,
              x == toupper(x))

  res <- a3[pmatch(x, nom, duplicates.ok = TRUE)]
  res[keep] <- x[keep]
  if (anyNA(res[!xna])) {
    print(data.table(x = x, res = res) %>% unique)
    stop("Country code could not be decoded.")
  }
  res[xna] <- NA_character_
  res
}
