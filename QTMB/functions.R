##############################
# Functions for Modifying UI #
##############################


#' Title
#'
#' @param df the soil data frame
#' @param x soil texture
#' @param y soil moisture status
#' @param z sampling depth upper limit
#' @param a quick test nitrate reaults in mg/L
#' @param zz sampling depth lower limit
#'
#' @return a data frame with one row observation
#' @export
#'
#' @import dplyr
#'
#'
cf_filter <- function(df, x, y, z, zz, a){
  #
  # if(is.na(z) | is.na(zz)){
  #
  # }
  cf <- df %>%
    dplyr::filter(Texture == x,
                  Moisture == y,
                  upper < z + 1 & zz <= lower ) %>%
    dplyr::mutate(qTestN.mg.L = as.integer(a),
                  qTestN.mg.kg = round(qTestN.mg.L/CF, digits = 0))

  cf
}

