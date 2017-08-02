#' Breeding status and climate conditinos of Fulanus at specific locations
#'
#' A dataset containing information on if Fulanus is breeding or not at specific locations and times, as well as the climatic conditions then.
#'
#' @format A data frame with 7380 rows and 7 variables:
#' \describe{
#'   \item{Lon}{Longitude of occurrence records in decimal degrees}
#'   \item{Lat}{Latitude of occurrence records in decimal degrees}
#'   \item{month}{Month of occurrence record}
#'   \item{breeding}{Binary breeding status during occurrence record, 1 means breeding, 0 means not breeding}
#'   \item{prec}{Precipitation during occurrence month, in milimeters}
#'   \item{tmin}{Minimum temperature during occurrence month, in degrees Celsius}
#'   \item{tmax}{Maximum temperature during occurrence month, in degrees Celsius}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"FulanusBreeding"
