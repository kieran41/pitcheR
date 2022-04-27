#' Take a statcast data set and compare release speed densities by pitch type
#'
#' @param file statcast file for specified player
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return ggplot graphic
#'
#' @export
velo_bp <- function(player) {
  player %>%
    ggplot(aes(x = release_speed))+
    geom_density(aes(fill = pitch_type)) +
    facet_wrap(~ as.factor(pitch_type), ncol = 1) +
    labs(x = "Release Speed", y = "Density")}

velo_bp(deGrom)



