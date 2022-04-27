#' Take a statcast data set and created a heatmap of hit pitch locations
#'
#' @param file statcast file
#' @param by_pitch_type boolean to facet by pitch type, default is TRUE
#' @param hit.status boolean to filter contacts into only events resulting in hits
#' @param min.exit.velocity double to define the minimum exit velocity on a contact that you want to consider in the graphic
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return ggplot graphic
#'
#' @export

contact_location_heatmap<- function(file =NULL, by_pitch_type = TRUE, hit.status = FALSE, min.exit.velocity =0.0){

  if(is.null(file)){
    file = system.file("extdata" ,"deGrom.csv", package = "pitcheR")
    warning('default file names')
  }

  if(!as.numeric(min.exit.velocity)){
    warning("min.exit.velocity should be numeric value.")
    #return(NA)
  }


  low.color<- "gray"
  high.color<- "darkred"

  first <- str_trim(str_split(file$player_name[1], pattern = ",")[[1]][2])
  last <- str_split(file$player_name[1], pattern = ",")[[1]][1]


  file <- file %>%
    filter(description == "hit_into_play" , launch_speed >= min.exit.velocity)

  if(hit.status == T){
  file <- file %>%
    filter(events %in% c("single", "double", "triple", "home_run"))
  }

  if(by_pitch_type==T){
    graphic <- file %>%
      group_by(pitch_type) %>%
      ggplot(aes(x= plate_x, y= plate_z))+
      geom_hex(bins = 10)+
      scale_fill_gradient(low=low.color,high=high.color,trans="log10") +
      add_zone()+
      labs(title = paste("Location Heatmap by Pitch from", first, last),
           x = "Location from Catcher's Perspective (ft)",
           y = "Location off the Ground (ft)")+
      facet_wrap(~pitch_type)
  }

  if(by_pitch_type==F){
    graphic <- file %>%
      ggplot(aes(x= plate_x, y= plate_z))+
      geom_hex(bins = 10)+
      scale_fill_gradient(low=low.color,high=high.color,trans="log10") +
      labs(title = paste("Location Heatmap by Pitch from", first, last),
           x = "Location from Catcher's Perspective (ft)",
           y = "Location off the Ground (ft)")+
      add_zone()
  }

  return(graphic)

}
