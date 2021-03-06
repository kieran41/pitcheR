#' Take a statcast data set and visualize horizontal and vertical break by pitch type
#'
#' @param file statcast file for specified player
#' @import ggplot2
#' @import dplyr
#' @return ggplot graphic
#' @examples
#' break_func(file = deGrom)
#' @export
break_func <- function(file = NULL){
  first <- stringr::str_trim(str_split(file$player_name[1], pattern = ",")[[1]][2])
  last <- stringr::str_split(file$player_name[1], pattern = ",")[[1]][1]

file <- file%>%
  filter(!is.na(pfx_x), !is.na(pfx_z))

minx = min(file$pfx_x)
maxx= max(file$pfx_x)

minz = min(file$pfx_z)
maxz = max(file$pfx_z)
  file%>%
    group_by(pitch_type)%>%
    summarise(average_x = mean(pfx_x,na.rm=TRUE), average_z = mean(pfx_z,na.rm=TRUE))%>%
    ggplot(aes(x= average_x, y = average_z ,color = pitch_type))+
    geom_point()+
    geom_segment(aes( xend=0, yend=0))+
    xlim(minx,maxx)+ylim(minz,maxz)+
    xlab("Horizontal Break (feet)")+
    ylab("Verticle Break (feet)")+
    labs(title = paste("Average Break for", first, last))




}




