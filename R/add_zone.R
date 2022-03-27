#' adds zone to ggplot graphic
#'
#' @author Jim Albert
#'
#' @export
#' @import ggplot2

add_zone <- function(Color = "black"){
  topKzone <- 3.5
  botKzone <- 1.6
  inKzone <- -0.85
  outKzone <- 0.85
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  geom_path(aes(.data$x, .data$y), data=kZone,
            lwd=1, col=Color)
}
