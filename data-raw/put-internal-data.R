SA16_decoder <- fread("data-raw/SA16_decoder.csv")
ISO3166 <- fread("data-raw/ISO-3166-countries.csv", encoding = "UTF-8")
setnames(ISO3166, "alpha-3", "alpha_3")
usethis::use_data(SA16_decoder, ISO3166,
                  overwrite = TRUE,
                  internal = TRUE)
