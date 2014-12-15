#' A function to extract the legend from a ggplot object
#' 
#' This function pulls out the legend as a grob object from a ggplot object. This lets you
#' play around with a plots layout by using the arrangeGrob function.
#'
#' @title Extract legend from ggplot geom object
#' @description Lets you extract a ggplot legend to be used elsewhere. 
#' @param gg.plot A ggplot object
#' @return The legend as a grob object from the plot
#' @references Obtained from \url{http://stackoverflow.com/questions/11883844}
#' @export
#' @examples
#' ## Create a plot with a legend
#' library(ggplot2)
#' plt1 <- ggplot() + geom_boxplot(aes(factor(gear), mpg, fill=factor(gear)), mtcars)
#' ## Extract the legend from the plot.
#' plt_legend <- g_legend(plt1)
#' ## Create a second boxplot with the same fill 
#' plt2 <- ggplot() + geom_boxplot(aes(factor(gear), disp, fill=factor(gear)), mtcars)
#' ## Suppress the legends from both plots
#' plt1 <- plt1 +  theme(legend.position="none")
#' plt2 <- plt2 +  theme(legend.position="none")
#' library(gridExtra)
#' combined_plot <- arrangeGrob(plt1, plt2, plt_legend, ncol=3)
#' combined_plot
#' ## Or a slightly nicer example
#' combined_plot <- arrangeGrob(arrangeGrob(plt1, plt2, ncol=1), plt_legend, ncol=2, widths=c(0.8,0.2))
#' combined_plot

g_legend<-function(gg.plot){
  if(!inherits(gg.plot, "ggplot")){
    stop( "gg.plot must be a ggplot object (see ?ggplot)")
  }
  if(!contains_legend(gg.plot)){
    stop("Does your plot have a legend in it?")
  }
  
  tmp <- ggplot2::ggplot_gtable(ggplot_build(gg.plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

