#' Take a statcast data set and plot aver velocity per game of a certai pitch type
#'
#' @param file statcast file
#' @param pitch pitch type, default is FF
#'
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#' @import stringr
#'
#' @return summary table
#' @examples
#' velo_time(deGrom)
#' @export
velo_time <- function(file, pitch = "FF"){
  first <- stringr::str_trim(str_split(file$player_name[1], pattern = ",")[[1]][2])
  last <- stringr::str_split(file$player_name[1], pattern = ",")[[1]][1]

  velo_graph <- file %>%
    mutate(game_date == as_date(game_date)) %>%
    filter(pitch_type == pitch) %>%
    group_by(game_date) %>%
    summarise(mean_velo_game = mean(release_speed)) %>%
    mutate(game_date = as_date(game_date)) %>%
    ggplot(mapping = aes(x = game_date, y = mean_velo_game)) +
    geom_point() +
    geom_line() +
    scale_x_date(date_labels = "%B") +
    labs(x = "Month", y = "Average Velocity",
         title = paste("Average Velocity of", pitch, "Per Game for", first, last))
  return(velo_graph)
}


