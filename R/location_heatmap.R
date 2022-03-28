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

location_heatmap<- function(file =NULL, by_pitch_type = TRUE, sq=3){

  if(is.null(file)){
    file = system.file("extdata" ,"deGrom.csv", package = "pitcheR")
    warning('default file names')
  }


  low.color<- "gray"
  high.color<- "darkslateblue"

  location.information <- count_points(file = file, sq=sq)
  loc.matrix <- location.information[[1]]
  x.list <- location.information[[2]]
  y.list <- location.information[[3]]

  data <-expand.grid(X=x.list, Y=y.list)
  data$count <- loc.matrix

  if(by_pitch_type==T){
  graphic <- file %>%
    group_by(pitch_type) %>%
    ggplot(aes(x= plate_x, y= plate_z))+
    geom_hex(bins = 10)+
    scale_fill_gradient(low=low.color,high=high.color,trans="log10") +
    add_zone()+
    facet_wrap(~pitch_type)
  }

  if(by_pitch_type==F){
    graphic.test<- ggplot(aes())

    graphic <- file %>%
      ggplot(aes(x= plate_x, y= plate_z))+
      geom_hex(bins = 10)+
      scale_fill_gradient(low=low.color,high=high.color,trans="log10") +
      add_zone()
  }

  return(graphic)

}
