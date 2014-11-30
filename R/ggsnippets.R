#' everything.
#'
#' @name ggsnippets
#' @docType package
#' @import ggplot2 gridExtra grid gtable
NULL

#' Long term historical climate data for Breamar, UK
#'
#' A dataset containing monthly maximum, minimum temperature, rainfall and sunlight hours
#'
#' \describe{
#'   \item{year}{year of record}
#'   \item{month}{month of record}
#'   \item{max_temp}{maximum temperature in this month}
#'   \item{min_temp}{minimum temperature in this month}
#'   \item{rain_mm}{total rainfall of the month}
#'   \item{sun_hours}{total sunlight hours in the month}
#' }
#' @source \url{http://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/braemardata.txt}
#' @docType data
#' @keywords datasets
#' @name braemardata
#' @usage data(braemardata)
#' @format A data frame with 669 rows and 6 variables.
NULL