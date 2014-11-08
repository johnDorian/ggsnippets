#' A function to determine if the x axis scale is datetime or date formatted
#'  @export rbind_ggplot_timeseries
#'  
.ggplot_x_date_scale <- function(x){
  # make sure the imput is a ggplot object
  if(!is.ggplot(x)){
    stop("Not a ggplot object")
  }
  # Build the plot
  build <- ggplot_build(x)
  # Check to see if the y axis is a datetime or date scale on the x axis  
  res <- 0
  if(inherits(build$panel$x_scales[[1]], "datetime")){
    return("datetime")
  }
  if(inherits(build$panel$x_scales[[1]], "date")){
    return("date")
  }
  stop("X axis scale must be either date or datetime")
}

#' A function to set the date range limit of the x axis of a list of ggplots
.set_x_scales <- function(limits, ggplot_list){
  # Check the limtits 
  if(!inherits(limits, "POSIXt")){
    stop("limits must be of class POSIXt")
  }
  date_format <- lapply(ggplot_list, .ggplot_x_date_scale)
  scale_list<- list()
  for(i in 1:length(ggplot_list)){
    if(date_format[[i]]=="datetime"){
      scale_list[[i]] <- ggplot_list[[i]] + scale_x_datetime(limits=limits)
    } else {
      scale_list[[i]] <- ggplot_list[[i]] + scale_x_date(limits=as.Date(limits))
    }
  }
  scale_list
}

#' A fucntion to suppress the x axis labels, tick marks and title of all plots excluding the 
#' last plot in the list.
.hide_x_labels <- function(ggplot_list){
  for(i in 1:(length(ggplot_list)-1)){
    ggplot_list[[i]] <- ggplot_list[[i]] + 
      theme(axis.ticks.x = element_blank(), 
            axis.text.x = element_blank(), 
            axis.title.x = element_blank())
  }
  ggplot_list
  
}

#' A function to change the plot margins and therefore shrinking the gap between the plots
.shrink_space <- function(ggplot_list, shrink_factor){
  for(i in 1:length(ggplot_list)){
    if(i==1){
      ggplot_list[[i]]<- ggplot_list[[i]] + theme(plot.margin = unit(c(1,1,-1*shrink_factor,1), "line"))
    } else {
      ggplot_list[[i]]<- ggplot_list[[i]] + theme(plot.margin = unit(c(-1*shrink_factor,1,-1*shrink_factor,1), "line"))
    }
    if(i==length(ggplot_list)){
      ggplot_list[[i]]<- ggplot_list[[i]] + theme(plot.margin = unit(c(-1*shrink_factor,1,1,1), "line"))
    }
  }
  ggplot_list
}


#' 
#' @title combine multiple time series ggplots
#' @param ggplot_list A \code{list} of ggplot objects in order to be plotted. Ordered from top to bottom
#' @param limits a \code{vector} (n = 2) of \code{as.POSIXct} objects representing the minimum and maximum dates of the timeseries
#' @param hide_x_labels a logical value indicating if the x axis labels, ticks and title should be suppressed from all but the last plot. Default is \code{TRUE}
#' @param shrink_space a logical value indicating if the space between the plots should be reduced. Ignored if \code{hide_x_labels = FALSE}. Default is \code{TRUE}
#' @param shrink_factor a numerical value indicating the distance between each of the plots.
#' @return returns a \code{grob} object using the \code{grid.arrange} function.
#' @description This function sets the range of timeseries ggplots
#'  and combines them in one column and returns a \code{grob} object. 
#'  @examples
#'  # Load some example time series data
#'  data(breamardata)
#'  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
#'  
#'  # Create three plots without subsetting the data.
#'  rain_plot <- ggplot() + geom_line(aes(date, rain_mm), breamardata) + theme_few(20)
#'  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_few(20)
#'  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_few(20)
#'  
#'  # Plot all three together
#'  rbind_ggplot_timeseries(ggplot_list = list(rain_plot,
#'                                             min_temp_plot,
#'                                             max_temp_plot
#'                                        ),
#'                          limits = c(dmy("01011960", "31122014"))
#'  )
#' # Plot all together with zooming in on 1990 - 2000
#'  rbind_ggplot_timeseries(ggplot_list = list(rain_plot,
#'                                             min_temp_plot,
#'                                             max_temp_plot
#'                                        ),
#'                          limits = c(dmy("01011990", "31122000"))
#'  )
#'  # An example with data from different time frames
#'  # Plot monthly rain from 1960 - 1980
#'  rain_plot <- breamardata %>%
#'      filter(date<=dmy("01011980")) %>%
#'      ggplot() + geom_line(aes(date, rain_mm)) + theme_few(20)
#'  rain_plot
#'  # Plot monthly minimum temp from 1980 - 2000
#'  min_temp_plot <- breamardata %>%
#'    filter(date>=dmy("01011980")&date<=dmy("01012000")) %>%
#'    ggplot() + geom_line(aes(date, min_temp)) + theme_few(20)
#'  min_temp_plot
#'  # Plot  monthly maximum temp from 2000 onwards
#'  max_temp_plot <- breamardata %>%
#'    filter(date>=dmy("01012000")) %>%
#'    ggplot() + geom_line(aes(date, max_temp)) + theme_few(20)
#'  max_temp_plot
#'  
#'  # Plot all three timeseries together. 
#'  rbind_ggplot_timeseries(ggplot_list = list(rain_plot,
#'                                             min_temp_plot,
#'                                             max_temp_plot
#'                                            ),
#'                          limits = c(dmy("01011960", "31122014"))
#'  )
#'    
#'  

rbind_ggplot_timeseries <- function(ggplot_list=list(), limits, hide_x_labels = TRUE, 
                                    shrink_space = TRUE, shrink_factor = 0.2){
  ## Taken and modified from http://stackoverflow.com/questions/13294952/
  if(!all(sapply(ggplot_list, is.ggplot))){
    stop("all plots must be of class ggplot")
  }
  if(!is.logical(hide_x_labels)){
    stop("hide_x_labels must be logical")
  }
  if(!is.logical(shrink_space)){
    stop("shrink_space must be logical")
  }
  if(!is.numeric(shrink_factor)&length(shrink_factor)>1){
    stop("shrink_factor must be numeric and of length 1.")
  }
  ggplots <- .set_x_scales(limits=limits, ggplot_list)
  
  
  if(hide_x_labels){
    ggplots <- .hide_x_labels(ggplots)
    if(shrink_space){
      ggplots <- .shrink_space(ggplots, shrink_factor)
    }
  }
  

  if(!all(sapply(ggplots, is.ggplot))){
    stop("All objects must be of class ggplot (see ?is.ggplot)")
  }
  gtl <- lapply(ggplots, ggplotGrob)
  
  bind2 <- function (x, y) 
  {
    if(ncol(x) != ncol(y)){
      stop("plots have incorrect dimensions")
    }
    if (nrow(x) == 0) 
      return(y)
    if (nrow(y) == 0) 
      return(x)
    y$layout$t <- y$layout$t + nrow(x)
    y$layout$b <- y$layout$b + nrow(x)
    x$layout <- rbind(x$layout, y$layout)
    x$heights <- gtable:::insert.unit(x$heights, y$heights)
    x$rownames <- c(x$rownames, y$rownames)
    x$widths <- grid::unit.pmax(x$widths, y$widths)
    x$grobs <- append(x$grobs, y$grobs)
    x
  }
  
  grid.arrange(Reduce(bind2, gtl), ncol=1)
}