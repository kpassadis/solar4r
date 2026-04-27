#' Calculate the position of an interval within a day
#' 
#' @param date_time A POSIXct/POSIXt vector of timestamps
#' @param period_in_minutes An integer that represents the duration of the interval in minutes
#' 
#' @returns A vector of integers where each value represents the position of the corresponding timestamp within the day
#' @examples
#' get_interval(lubridate::ymd_hms("2026-01-03 22:00:00"), 15)
#' 
#' @export
get_interval <- function(date_time, period_in_minutes) {

  stopifnot(any(c("POSIXct", "POSIXt") %in% class(date_time)))

  if(period_in_minutes > 60) {
    warning("interval_duration_in_minutes should be less than 60, setting to 60")
  }
  interval_duration_in_minutes <- min(period_in_minutes, 60)
  hh <- lubridate::hour(date_time) * as.integer(60 / period_in_minutes)
  mm <- as.integer(lubridate::minute(date_time) / period_in_minutes)
  1L + hh + mm
}


#' Calculate the timestamp of the solar noon for a given date and geographical postion
#' 
#' @param date_time A POSIXct/POSIXt vector of timestamps
#' @param lat A real number to specify geographical latitude
#' @param lon A real number to specify geographical longitude
#' @param tz A character vector to specify timezone
#' @returns A data frame with two columns: the date and the corresponding interval that is calculated by the function
#' @examples
#' get_solar_noon(as.Date("2026-03-01"))
#' 
#' @export
get_solar_noon <- function(date_time, lat = 37.983810, lon = 23.727539, tz = "UTC") {
  stopifnot(any(c("POSIXct", "POSIXt", "Date") %in% class(date_time)), is.numeric(lat), is.numeric(lon))

  dates <- as.Date(date_time, tz = tz)
  ud <- sort(unique(dates))
  st <- suncalc::getSunlightTimes(
    date = ud, lat = lat, lon = lon,
    keep = c("solarNoon"), tz = tz
  )
  sn <- st$solarNoon
  start_dates <- lubridate::floor_date(ud, unit = "day")

  data.frame(
    date = start_dates,
    solar_noon = sn
  )
}

#' Add solar geometry weights to a dataframe
#' 
#' @param df A dataframe.
#' @param tstamp_col The name of the timestamp column.
#' @param beta The exponential decay parameter.
#' @param lat Latitude of the location.
#' @param lon Longitude of the location.
#' @param method A character vector to specify the weight calculation methodology (gaussian, laplacian)
#' @param tz A character vector to specify timezone
#' @param period An integer that represents the periodicity of the time series in minutes
#' 
#' @export
add_weights <- function(df, tstamp_col, beta, period, lat, lon, method = "gaussian", tz = "UTC") {
  df$weight <- get_distance_from_solar_noon(df[[tstamp_col]], beta, period, method, lat, lon, tz)
  return(df)
}

#' Calculate a vector of weights between 0 and 1 for each interval. 
#' The weight represents the relative importance a value has on a specific time interval within the day
#' and are calculated as the exponential difference between the current timestamp and the 
#' 
#' @param date_time A POSIXct/POSIXt vector of timestamps
#' @param beta A real number that dictates the steepness of the exponental function. The larger the beta the more aggressively the values will decay from the position of the solar noon interval.
#' @param period An integer that represents the periodicity of the time series in minutes
#' @param lat A real number to specify geographical latitude
#' @param lon A real number to specify geographical longitude
#' @param tz A character vector to specify timezone
#' @param method A character vector to specify the weight calculation methodology (gaussian, laplacian)
#' 
#' @returns A vector of real numbers between 0 and 1, and their sum is equal to 1.
#' @examples
#' get_distance_from_solar_noon(date_time = lubridate::ymd_hms("2026-01-03 22:00:00"), period = 15)
#' 
#' @export
get_distance_from_solar_noon <- function(date_time, beta = 0.01, period = 15, method = "gaussian", lat = 37.983810, lon = 23.727539, tz = "UTC") {
  stopifnot(any(c("POSIXct", "POSIXt") %in% class(date_time)), is.numeric(beta))
  
  dates <- as.Date(date_time, tz = tz)
  df <- data.frame(date = dates, date_time = date_time)
  solar_noon <- get_solar_noon(date_time, lat, lon, tz)

  df <- dplyr::inner_join(solar_noon, df, by = c("date" = "date"))

  gaussian <- \(beta, weight) exp(-beta * (weight ^ 2))
  laplace <- \(beta, weight) exp(-beta * abs(weight))
  echo <- \(beta, weight) weight

  weight_func <- if (method == "gaussian") {
    gaussian
  } else if (method == "laplacian"){
    laplace
  } else {
    echo
  }

  df <- df |>
    dplyr::mutate(weight = lubridate::interval(solar_noon, date_time) %/% lubridate::minutes(period)) |>
    dplyr::mutate(weight = weight_func(beta, weight))

  weights <- df |> dplyr::pull(weight)

  return(weights)
}

#' Calculate sunrise and sunset for a specific geographical location and timezone
#' 
#' @param date_time A POSIXct/POSIXt vector of timestamps
#' @param lat A real number that represents latitude
#' @param lon A real number that represents longitude
#' @param tz A character vector that represents the timezone
#' @return A data frame with dates, sunrise, and sunset timestamps
#' 
#' @examples
#' get_sunrise_sunset(as.POSIXct("2026-02-02 23:00", tz = "UTC"))
#' 
#' @export
get_sunrise_sunset <- function(date_time, lat = 37.983810, lon = 23.727539, tz = "UTC") {
  
  stopifnot(any(c("POSIXct", "POSIXt") %in% class(date_time)))

  dates <- unique(as.Date(date_time, tz = tz))
  
  # Fetch the data and subset the exact columns directly
  ss <- suncalc::getSunlightTimes(date = dates, lat = lat, lon = lon, tz = tz)
  
  # Return just the three columns we need
  ss[, c("date", "sunrise", "sunset")]
}

#' Calculate Daylight Flag
#' 
#' Indicates whether a specific timestamp falls between sunrise and sunset.
#' 
#' @param date_time A POSIXct/POSIXt vector of timestamps
#' @param lat A numeric value representing latitude (default is Athens)
#' @param lon A numeric value representing longitude (default is Athens)
#' @param tz A string representing the timezone
#' @return A logical vector (TRUE if during daylight, FALSE otherwise)
#' 
#' @examples
#' #Assuming tstamp is a vector of POSIXct times
#' data(solar)
#' solar <- solar |> dplyr::mutate(is_sunny = is_daylight(tstamp))
#' 
#' @export
is_daylight <- function(date_time, lat = 37.983810, lon = 23.727539, tz = "UTC") {
  
  stopifnot(any(c("POSIXct", "POSIXt") %in% class(date_time)))
  
  #enforce the timezone when extracting the date)
  unique_dates <- unique(as.Date(date_time, tz = tz))
  
  ss <- suncalc::getSunlightTimes(
    date = unique_dates, 
    lat = lat, 
    lon = lon, 
    tz = tz, 
    keep = c("sunrise", "sunset")
  )
  
  # match() is dramatically faster and safer than a dplyr join for vector operations.
  # It guarantees the output vector is exactly the same length as the input vector.
  date_indices <- match(as.Date(date_time, tz = tz), ss$date)
  
  aligned_sunrise <- ss$sunrise[date_indices]
  aligned_sunset <- ss$sunset[date_indices]
  
  # (Evaluating > and < inherently creates a TRUE/FALSE vector, no ifelse needed)
  is_day_vec <- date_time > aligned_sunrise & date_time < aligned_sunset
  
  return(is_day_vec)
}


#' Add Cyclic Time Components to Dataframe
#'
#' @param data A dataframe that contains a timestamp of type POSIXct or POSIXt
#' @param time_col The name of the timestamp column (as a string)
#' @return The original dataframe with qsin and qcos appended
#' 
#' @examples
#' my_data <- data.frame(tstamp = as.POSIXct("2026-01-01 12:00:00", tz = "UTC"))
#' add_cyclic_components(my_data, time_col = "tstamp")
#' 
#' @export
add_cyclic_components <- function(data, time_col = "tstamp") {
  stopifnot(any(c("POSIXct", "POSIXt") %in% class(data[[time_col]])))
  # Extract the time vector using tidy evaluation
  time_vec <- data[[time_col]]
  
  mins_since_midnight <- lubridate::hour(time_vec) * 60 + lubridate::minute(time_vec)
  
  # Project onto a circle
  data |>
    dplyr::mutate(
      qsin = sin(2 * pi * mins_since_midnight / 1440),
      qcos = cos(2 * pi * mins_since_midnight / 1440)
    )
}

#' Add a Date column to the input data frame
#' 
#' @param data A data frame that contains a timestamp
#' @param time_col The name of the timestamp column (as a string)
#' @param tz A character vector that represents the timezone
#' 
#' @return The original data frame with a date column appended
#' 
#' @examples
#' data(solar)
#' solar <- add_date(solar, "tstamp")
#' 
#' @export 
add_date <- function(data, time_col, tz = "UTC") {
  stopifnot(any(c("POSIXct", "POSIXt") %in% class(data[[time_col]])))
  data |>
    dplyr::mutate(date = as.Date(data[[time_col]], tz = tz))
}

#'Extract the month from a timestamp
#' 
#' @param date_time A POSIXct/POSIXt vector of timestamps
#' 
#' @return An integer value between 1 and 12
#' @examples
#' get_month(as.POSIXct("2026-01-03 22:00:00", tz = "UTC"))
#' 
#' @export 
get_month <- function(date_time) {

  stopifnot(any(c("POSIXct", "POSIXt") %in% class(date_time)))

  lubridate::month(date_time)
}