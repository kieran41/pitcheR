#' Take a statcast data set and created a heatmap of pitch locations
#'
#' @param file statcast file
#' @param by_pitch_type boolean to facet by pitch type, default is TRUE
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return ggplot graphic
#'
#' @export

location_heatmap<- function(file =NULL, by_pitch_type = TRUE){

  if(is.null(file)){
    file = system.file("extdata" ,"deGrom.csv", package = "pitcheR")
    warning('default file names')
  }


  low.color<- "lightblue1"
  high.color<- "black"

  first <- str_trim(str_split(file$player_name[1], pattern = ",")[[1]][2])
  last <- str_split(file$player_name[1], pattern = ",")[[1]][1]


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
