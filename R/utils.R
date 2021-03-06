insert.unit <- function (x, values, after = length(x)) {
  lengx <- length(x)
  if (lengx == 0) return(values)
  if (length(values) == 0) return(x)
  
  if (after <= 0) {
    unit.c(values, x)
  } else if (after >= lengx) {
    unit.c(x, values)
  } else {
    unit.c(x[1L:after], values, x[(after + 1L):lengx])
  }
}

contains_legend <- function(gg.plot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(gg.plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg)<1){
    return(FALSE)
  } else {
    return(TRUE)
  }
}
 
ggplot_x_date_scale <- function(x){
  # make sure the imput is a ggplot object
  if(!ggplot2::is.ggplot(x)){
    stop("Not a ggplot object")
  }
  # Build the plot
  build <- ggplot2::ggplot_build(x)
  # Check to see if the y axis is a datetime or date scale on the x axis  
  if(inherits(build$panel$x_scales[[1]], "ScaleContinuousDatetime")){
    return("datetime")
  }
  if(inherits(build$panel$x_scales[[1]], "ScaleContinuousDate")){
    return("date")
  }
  stop("X axis scale must be either date or datetime")
}

set_x_scales <- function(limits, ggplot_list){
  # Check the limtits 
  if(!inherits(limits, "POSIXt")){
    stop("limits must be of class POSIXt")
  }
  date_format <- lapply(ggplot_list, ggplot_x_date_scale)
  scale_list<- list()
  for(i in 1:length(ggplot_list)){
    if(date_format[[i]]=="datetime"){
      scale_list[[i]] <- ggplot_list[[i]] + ggplot2::scale_x_datetime(limits=limits)
    } else {
      scale_list[[i]] <- ggplot_list[[i]] + ggplot2::scale_x_date(limits=as.Date(limits))
    }
  }
  scale_list
}

hide_x_labels <- function(ggplot_list){
  for(i in 1:(length(ggplot_list)-1)){
    ggplot_list[[i]] <- ggplot_list[[i]] + 
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(), 
            axis.text.x = ggplot2::element_blank(), 
            axis.title.x = ggplot2::element_blank())
  }
  ggplot_list
  
}


shrink_space <- function(ggplot_list, shrink_factor){
  for(i in 1:length(ggplot_list)){
    if(i==1){
      ggplot_list[[i]]<- ggplot_list[[i]] + ggplot2::theme(plot.margin = unit(c(1,1,-1*shrink_factor,1), "line"))
    } else {
      ggplot_list[[i]]<- ggplot_list[[i]] + ggplot2::theme(plot.margin = unit(c(-1*shrink_factor,1,-1*shrink_factor,1), "line"))
    }
    if(i==length(ggplot_list)){
      ggplot_list[[i]]<- ggplot_list[[i]] + ggplot2::theme(plot.margin = unit(c(-1*shrink_factor,1,1,1), "line"))
    }
  }
  ggplot_list
}


# 
bolus <- function(x) UseMethod("bolus")
bolus.proto <- function(x) x$bolus()

bolus.ggplot <- function(x, ...) {
  sort.by.name <- function(x) {
    if (is.null(names(x))) return(x)
    x[order(names(x))]
  }
  
  with(x, list(
    data = digest::digest(data),
    mapping = sort.by.name(mapping),
    layers = sapply(layers, function(x) x$hash()),
    scales = digest::digest(scales),
    facet = facet$hash(),
    coord = coordinates$hash(),
    theme = digest::digest(defaults(x$theme, theme_get()))
  ))
}

digest.proto <- function(x, ...) x$hash(, ...)
digest.ggplot <- function(x, ...) {
  if (is.null(x)) return()
  digest(bolus(x), ...)
}

