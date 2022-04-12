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

custom_heatmap<- function(file =NULL, by_pitch_type = TRUE, sq=3){

  if(is.null(file)){
    file = system.file("extdata" ,"deGrom.csv", package = "pitcheR")
    warning('default file names')
  }


  low.color<- "lightblue1"
  high.color<- "black"



  if(by_pitch_type==T){

    file_list <- split(file, f = file$pitch_type)

    loc.matrix<-list()
    location.information <- list()
    x.list <- list()
    y.list<- list()


    for(i in 1:length(file_list)){
      location.information[[i]] <- count_points(file = file_list[[i]], sq=sq)
    }

    for(i in 1:length(location.information)){
      loc.matrix[[i]] <- location.information[[i]][[1]]
      x.list[[i]] <- location.information[[i]][[2]]
      y.list[[i]]  <- location.information[[i]][[3]]
    }

    x.update<-list()
    temp <- list()
    for(i in 1:length(x.list)){
      for(j in 1:length(x.list[[i]])-1){
        temp[j] <- (x.list[[i]][j]+x.list[[i]][j+1])/2

      }
      x.update[[i]] <- temp
    }


    y.update<-list()
    temp <- list()
    for(i in 1:length(y.list)){
      for(j in 1:length(y.list[[i]])-1){
        temp[j] <- (y.list[[i]][j]+y.list[[i]][j+1])/2

      }
      y.update[[i]] <- temp
    }

    for(i in 1:length(file_list)){
      data[[i]] <-expand.grid(X=x.update[[i]], Y=y.update[[i]])
      count.order[[i]] <- as.vector(t(loc.matrix[[i]][c(dim(loc.matrix[[i]])[1]:1),]))
    }



    if(by_pitch_type==F){

      location.information <- count_points(file = file, sq=sq)
      loc.matrix <- location.information[[1]]
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


      data <-expand.grid(X=x.update, Y=y.update)
      count.order <- as.vector(t(loc.matrix[c(dim(loc.matrix)[1]:1),]))

      data$count <- count.order



      graphic.test<-ggplot(data)+
        geom_tile(aes(X,Y, fill = count))+
        scale_fill_gradient(low=low.color,high=high.color) +
        add_zone()

      graphic <- file %>%
        ggplot(aes(x= plate_x, y= plate_z))+
        geom_hex(bins = 10)+
        scale_fill_gradient(low=low.color,high=high.color,trans="log10") +
        add_zone()
    }

    return(graphic.test)

  }
