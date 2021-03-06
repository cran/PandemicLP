#' pandemicData objects: Covid-19 Pandemic Data
#'
#' The \code{\link{load_covid}} function returns an object of S3 class
#' \code{pandemicData} in a list format containing the components described below.
#'
#' @name pandemicData-objects
#'
#' @section Elements for \code{pandemicData} object:
#' \describe{
#'   \item{\code{data}}{
#'   data frame with the number of cumulative cases, new cases, cumulative deaths and new deaths associated
#'   with Covid-19 for each date, up to the \code{last_date} in the specified region.}
#'   \item{\code{name}}{
#'   string with the country name (and state name, if available).}
#'   \item{\code{population}}{
#'   numeric object that contains the population size of the given region.}
#' }
#'
NULL
