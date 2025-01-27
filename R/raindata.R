#' Monthly precipitation data from January 1960 until December 2024
#'
#' Monthly precipitation data (in mm) collected by the
#' monitoring stations of the Global Historical Climate
#' Network daily (GHCNd) from January 1960 until December
#' 2024. The original data of daily frequency were
#' aggregated to monthly total precipitation. The column
#' headers refer to the data (first column) and the
#' station identifiers (all other columns).
#'
#' For more details on data collection by the GHCNd, see
#' \insertCite{menne2012overview}{prcpFC},
#' \insertCite{durre2010comprehensive}{prcpFC}, and
#' \insertCite{durre2008strategies}{prcpFC}.
#' The dataset is employed in \insertCite{fritsch2025}{prcpFC}
#' to analyze the memory properties of the time series and
#' generate forecasts based on the memory parameter estimates.
#'
#' @name raindata
#'
#' @docType data
#'
#' @usage data(raindata)
#'
#' @format A dataset with 780 rows and 607 columns containing:
#' \describe{
#' \item{date}{date in format YYYY-mm}
#' \item{}{column names refer to station identifiers of the time series}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#'   data(raindata, package = "prcpFC")
#'   median(raindata[,2])
#' }
#'
NULL

