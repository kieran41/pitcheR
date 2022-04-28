
##function displays summary table of pitch type and swinging strike %, along with the velocity and spin rate
##for the pitch type
globalVariables(c("pfx_x", "pfx_z", "pitch_type", "average_x", "average_z"))
swg_strike <- function(file){
  player_table <- file %>%
  group_by(pitch_type) %>%
    summarise(swginging_strike_per = length(which(description == "swinging_strike" | description == "swinging_strike_blocked"))/(length(which(description == "foul" | description == "hit_into_play" | description == "foul_tip" | description == "swinging_strike" | description == "swinging_strike_blocked"))),
              average_velo = mean(release_speed, na.rm = T),
              average_spin_rate = mean(release_spin_rate, na.rm = T))

  return(player_table)
}


