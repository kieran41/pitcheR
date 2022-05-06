#' Take a statcast data set and compare release speed densities by pitch type
#'
#' @param file statcast file for specified player
#' @import ggplot2
#' @import dplyr
#' @return ggplot graphic
#' @examples
#' velo_bp(file = deGrom)
#' @export
velo_bp <- function(file = NULL) {
  first <- stringr::str_trim(str_split(file$player_name[1], pattern = ",")[[1]][2])
  last <- stringr::str_split(file$player_name[1], pattern = ",")[[1]][1]
   den_plot <-  ggplot(data = file, aes(x = release_speed))+
    geom_density(aes(fill = pitch_type)) +
    facet_wrap(~ as.factor(pitch_type), ncol = 1) +
    labs(x = "Release Speed (mph)", y = "Density", title = paste("Pitch Velocity Densities for", first, last))

   return(den_plot)}





