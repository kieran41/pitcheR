library(baseballr)

##function to graph game by game average velocity of specified pitch type (default is fastball)
velo_time <- function(pitcher_name, pitch = "FF"){
  last <- str_split(pitcher_name, " ")[[1]][2]
  first <- str_split(pitcher_name, " ")[[1]][1]
  id <- playerid_lookup(last_name = last, first_name = first)$mlbam_id
  player <- baseballr::statcast_search_pitchers(start_date = "2021-04-01",end_date = "2021-10-02", pitcherid = id)

  velo_graph <- player %>%
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

