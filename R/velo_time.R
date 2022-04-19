##function to graph game by game average velocity of specified pitch type
velo_time <- function(file, pitch = "FF"){
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
    labs(x = "Month", y = "Average Velocity", title = paste("Average Velocity of", pitch, "Per Game"))
  return(velo_graph)
}

