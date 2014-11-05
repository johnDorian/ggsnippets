#'
#'
#' @title A function to allow for expressions in ggplot facet titles. 
#' @param gg.plot a ggplot object
#' @return a ggplot object with new facet titles. 
#' @references \url{http://stackoverflow.com/questions/11979017}
#' @seealso plotmath
#' @examples 
#' ## Add a factor to the cars example data to facet the plots
#' x_cars <- data.frame(cars, variable = sample(letters[1:5], 50, replace = TRUE))
#' ## Create the plot with facets
#' plt <- ggplot() + geom_point(aes(speed, dist), x_cars) + facet_wrap(~variable, ncol=1)
#' plt
#' ## An example without labels - no real point in this. 
#' facet_wrap_labeller(plt)
#' ## Adding expressions within the facet labels. 
#' facet_wrap_labeller(plt, labels = c(expression(a^2), expression(b[1]), expression(frac(c,2)), "d", expression(infinity)))


 
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  if(!("gg" %in% class(gg.plot)||"ggplot" %in% class(gg.plot))){
    stop( "gg.plot must be a ggplot object (see ?ggplot)")
  }
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg)) #Get the names - should be same length as labels. 
  if(length(strips)==0){
    stop("Your ggplot does not seem to have any facets.")
  }
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}