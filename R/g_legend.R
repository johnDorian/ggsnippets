#'
#'
#' @title Extract legend from ggplot geom object
#' @param gg.plot A ggplot object
#' @return The legend as a grob object from the plot
#' @references Obtained from \url{http://stackoverflow.com/questions/11883844}
#' @examples
#' ## Create a plot with a legend
#' plt1 <- ggplot() + geom_boxplot(aes(factor(gear), mpg, fill=factor(gear)), mtcars)
#' ## Extract the legend from the plot.
#' plt_legend <- g_legend(plt1)
#' ## Create a second boxplot with the same fill 
#' plt2 <- ggplot() + geom_boxplot(aes(factor(gear), disp, fill=factor(gear)), mtcars)
#' ## Supress the legends from both plots
#' plt1 <- plt1 +  theme(legend.position="none")
#' plt2 <- plt2 +  theme(legend.position="none")
#' grid.arrange(plt1, plt2, plt_legend, ncol=3)
#' ## Or a slightly nicer example
#' grid.arrange(arrangeGrob(plt1, plt2, ncol=1), plt_legend, ncol=2, widths=c(0.8,0.2))

g_legend<-function(gg.plot){
  if(!inherits(gg.plot, "ggplot")){
    stop( "gg.plot must be a ggplot object (see ?ggplot)")
  }
  tmp <- ggplot_gtable(ggplot_build(gg.plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg)<1){
    stop("Does your plot have a legend in it?")
  }
  legend <- tmp$grobs[[leg]]
  return(legend)
}

