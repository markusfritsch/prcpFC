#' Global Historical Climate Network daily (GHCNd) precipitation data from 2000-2023
#'
#' Monthly precipitation data from the monitoring stations of
#' the Global Historical Climate Network daily (GHCNd) from 
#' January 1960 until December 2024. The dataset contains the
#' variables station identifier, latitude, longitude, altitude,
#' station name, US state, network, aa, koppen climate
#' classification, koppen climate zone, koppen precipitation
#' subgroup, koppen temperature subgroup.
#'
#' The dataset was collected by the monitoring stations of the
#' GHCNd, as described in \insertCite{menne2012overview}{prcpFC},
#' \insertCite{durre2010comprehensive}{prcpFC}, \insertCite{durre2008strategies}{prcpFC}
#' and employed in \insertCite{fritsch2025}{prcpFC} which also
#' provides forecasts based on the memory properties of the
#' collected time series. The climate zone classification
#' assigned to the stations was derived by \insertCite{kottek2006}{prcpFC}.
#'
#' @name ghcndStations
#'
#' @docType data
#'
#' @usage data(ghcndStations)
#'
#' @format A dataset with 118492 rows and 11 variables containing:
#' \describe{
#' \item{statID}{station identifier}
#' \item{lat}{latitude (CRS WGS84)}
#' \item{lon}{longitude (CRS WGS84)}
#' \item{alt}{altitude (in meters)}
#' \item{statName}{station name}
#' \item{stateUS}{US state in which the station is located}
#' \item{koppen2}{Koppen climate zone classification}
#' \item{koppen2.tmp}{Number of letters of Koppen climate zone classification}
#' \item{koppen2.cz}{Koppen main climate zone}
#' \item{koppen2.ps}{Koppen precipitation subgroup}
#' \item{koppen2.ts}{Koppen temperature subgroup}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#'   data(ghcndStations, package = "prcpFC")
#'   n <- ghcndStations$alt
#'   mean(n)
#' }
#'
NULL

