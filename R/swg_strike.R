#' Take a statcast data set and acquire the swinging strike percentage
#'
#' @param file statcast file
#'
#' @import dplyr
#' @return summary table
#' @examples
#' swg_strike(deGrom)
#' @export
swg_strike <- function(file){
  assertthat::assert_that(is.character(file$pitch_type))
  player_table <- file %>%
  group_by(pitch_type) %>%
    summarise(swginging_strike_per = length(which(description == "swinging_strike" | description == "swinging_strike_blocked"))/(length(which(description == "foul" | description == "hit_into_play" | description == "foul_tip" | description == "swinging_strike" | description == "swinging_strike_blocked"))),
              average_velo = mean(release_speed, na.rm = T),
              average_spin_rate = mean(release_spin_rate, na.rm = T))

  return(player_table)
}

