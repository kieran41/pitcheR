#create a function that summarizes player stats (per player)



summary_func <- function(file = NULL){


  file%>%
    dplyr::group_by(pitch_type)%>%
    summarise(average_x = mean(pfx_x), average_z = pfx_z)

}


summary_func(file = deGrom)
