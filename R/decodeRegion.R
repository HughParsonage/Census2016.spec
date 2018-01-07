#' Decode ASGS/non-ASGS region
#' @param DT A \code{data.table} whose first column is a region key will be decoded consistently.
#' @return \code{DT} with the first column coded consistently
#' @export decodeRegion

decodeRegion <- function(DT) {
  out <- DT
  decoder <-
    switch(Region,
           "SA2" = SA16_decoder[ASGS_Structure == Region,
                                .(SA2_MAIN16 = Census_Code_2016,
                                  SA2_NAME16 = Census_Name_2016)],
           "SA3" = SA16_decoder[ASGS_Structure == Region,
                                .(SA3_MAIN16 = Census_Code_2016,
                                  SA3_NAME16 = Census_Name_2016)],
           "SA4" = SA16_decoder[ASGS_Structure == Region,
                                .(SA4_MAIN16 = Census_Code_2016,
                                  SA4_NAME16 = Census_Name_2016)],
           "STE" = SA16_decoder[ASGS_Structure == Region,
                                .(STE_MAIN16 = Census_Code_2016,
                                  STE_NAME16 = Census_Name_2016)],
           "LGA" = {
             NonABS_decoder[ASGS_Structure == Region,
                            .(Census_Code_2016, LGA_NAME16 = Census_Name_2016)]
           },
           "CED" = {
             NonABS_decoder[ASGS_Structure == Region,
                            .(Census_Code_2016, CED_NAME16 = Census_Name_2016)]
           },
           "SED" = {
             NonABS_decoder[ASGS_Structure == Region,
                            .(Census_Code_2016, SED_NAME16 = Census_Name_2016)]
           },
           "SSC" = {
             NonABS_decoder[ASGS_Structure == Region,
                            .(Census_Code_2016, SSC_NAME16 = Census_Name_2016)]
           },
           NULL) %>%
    unique %>%
    setnames(1, Region_key)

  decoded_out <- decoder[out, on = Region_key]
  decoded_out[, (Region_key) := NULL]
  decoded_out
}
