#' Renewables Ninja Benchmark Data
#'
#' Hourly PV and weather data for a 2000 MW simulated plant.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{time}{Timestamp}
#'   \item{local_time}{Timestamp}
#'   \item{electricity}{Output in MW}
#'   \item{irradiance_direct}{Direct radiation (W/m2)}
#'   \item{irradiance_diffuse}{Diffuse radiation (W/m2)}
#'   \item{temperature}{Ambient temperature (C)}
#' }
#' @source \url{https://www.renewables.ninja/}
"ninja"

#' National Solar Production Data
#'
#' 15-minute interval data for the Greek national grid.
#'
#' @format A data frame with timestamps and PV production.
"solar"
#' @format A data frame with columns:
#' \describe{
#'   \item{tstamp}{Timestamp}
#'   \item{pv}{Output in MW}
#'   \item{irrad}{Direct radiation (W/m2)}
#'   \item{clds}{Cloud cover %}
#'   \item{temp}{Ambient temperature (C)}
#' }