#' Take a statcast data set and compare release speed densities by pitch type
#'
#' @param file statcast file for specified player
#' @import ggplot2
#' @import dplyr
#' @return ggplot graphic
#' @examples
#' velo_bp(file = deGrom)
#' @export

globalVariables(c("pfx_x", "pfx_z", "pitch_type", "average_x", "average_z"))
velo_bp <- function(file = NULL) {
   den_plot <-  ggplot(data = file, aes(x = release_speed))+
    geom_density(aes(fill = pitch_type)) +
    facet_wrap(~ as.factor(pitch_type), ncol = 1) +
    labs(x = "Release Speed (mph)", y = "Density")

   return(den_plot)}





