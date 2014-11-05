#'
#'
#' @title A function to allow for expressions in ggplot facet titles. 
#' @param gg.plot a ggplot object
#' @return a ggplot object with new facet titles. 
#' @references \url{http://stackoverflow.com/questions/11979017}
#' @seealso plotmath
#' @examples 
#' x_cars <- data.frame(cars, variable = sample(letters[1:5], 50, replace = TRUE))
#' plt1 <- ggplot() + geom_point(aes(speed, dist), x_cars) + facet_wrap(~variable, ncol=1)
#' plt
#' facet_wrap_labeller(plt1)
#' facet_wrap_labeller(plt1, labels = c(expression(a^2), expression(b[1]), expression(frac(c,2)), "d", expression(infinity)))


 
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}