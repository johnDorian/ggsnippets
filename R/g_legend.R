#'
#'
#' @title Extract legend from ggplot geom object
#' @param a.gplot A ggplot object
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

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

