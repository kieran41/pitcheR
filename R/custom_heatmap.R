#' Take a statcast data set and created a heatmap of pitch locations that are specific to the strikezone (always 3x3 in the zone)
#'
#' @param file statcast file
#'
#' @import ggplot2
#' @import dplyr
#' @import reshape2
#'
#' @return ggplot graphic
#'
#' @example
#' cust_heatmap <- custom_heatmap(file = deGrom)
#' cust_heatmap
#'
#' @export

globalVariables(c("pfx_x", "pfx_z", "pitch_type", "average_x", "average_z"))
custom_heatmap<- function(file =NULL){


  first <- str_trim(str_split(file$player_name[1], pattern = ",")[[1]][2])
  last <- str_split(file$player_name[1], pattern = ",")[[1]][1]


      location.information <- count_points(file = file)
      loc.matrix <- location.information[[1]]

      loc.matrix <- loc.matrix[-1,]

      x.list <- location.information[[2]]
      y.list <- location.information[[3]]

      x.update<-c()
      for(i in 1:length(x.list)-1){
        x.update[i] <- (x.list[i]+x.list[i+1])/2
      }

      y.update<-c()
      for(i in 1:length(y.list)-1){
        y.update[i] <- (y.list[i]+y.list[i+1])/2
      }


      colnames(loc.matrix) <- x.update
      rownames(loc.matrix) <- y.update


      longData <- reshape2::melt(loc.matrix)

      graphic.test <- ggplot(longData, aes(x = Var2, y = Var1)) +
                geom_raster(aes(fill=value)) +
                scale_fill_gradient(low="grey90", high="red") +
                labs(x="Location (Catcher's Perspective ft)", y="Height (ft)",
                     title = paste("Location Heatmap by Pitch from", first, last)) +
                theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                           axis.text.y=element_text(size=9),
                           plot.title=element_text(size=11))+
        add_zone()

    return(graphic.test)



}

