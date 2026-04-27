#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(optparse)
  library(solar4r)
  library(purrr)
  library(cli)
  library(e1071)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

option_list <- list(
  make_option(c("-c", "--config"), type="character", default="config.json", help="Path to JSON config file"),
  make_option(c("--mode"), type="character", help="The mode for the engine: train = for training a new model or eval - for recalling a saved model"),
  make_option(c("--lat"), type="double", help="latitude"),
  make_option(c("--lon"), type="double", help="longitude"),
  make_option(c("--cap"), type="integer", help="nominal capacity"),
  make_option(c("--period"), type="integer", help="period of time series in minutes"),
  make_option(c("--inFile"), type="character", help="Input filename that contains the data for training and/or recalling a trained model"),
  make_option(c("--outFile"), type="character", help="Output filename to write the model predictions"),
  make_option(c("--modelFile"), type="character", help="Filename to write a model to a file if mode = train / read a model from a file if mode = eval"),
  make_option(c("--tstamp"), type="character", help="The timestamp column in the data frame"),
  make_option(c("--formula"), type="character", help="The model formula in R formula format: target ~ covariates"),
  make_option(c("--fmt"), type="character", help="The format of the timestamp in the dataset")
)

parser <- OptionParser(option_list=option_list)
args <- parse_args(parser)

# Load JSON Config
if (!file.exists(args$config)) {
  cli_abort("Config file not found at {.path {args$config}}")
} else {
  cli_alert_info("Config file located at {.path {args$config}}")
}

config <- fromJSON(args$config)

# Retrieve the mode of operation
mode <- args$mode %||% config$mode

if (is.null(mode)) {
  cli_abort("Mode of operation must be defined as a command line argument or in JSON ({.val train} or {.val eval})")
}

# Retrieve files
input_file <- args$inFile %||% config$input_file
output_file <- args$outFile %||% config$output_file
model_file <- args$modelFile %||% config$model_file

if (!file.exists(input_file)) {
  cli_abort("Could not locate input file {.path {input_file}}")
}

# Retrieve timestamp column
tstamp_col <- args$tstamp %||% config$tstamp
if (is.null(tstamp_col)) {
  cli_abort("Please specify the name of the timestamp column")
}

fmt <- args$fmt %||% config$fmt

# --- TRAIN MODE ---
if (mode == "train") {
  lat <- args$lat %||% config$new$lat
  lon <- args$lon %||% config$new$lon
  capacity <- args$cap %||% config$new$cap
  tz <- config$new$tz
  period <- args$period %||% config$new$period
  metrics <- config$new$metrics
  
  # Standardizing metric names
  metrics <- sapply(metrics, function(x) if (x=="nmae") paste0("solar4r::", x, "(capacity)") else paste0("solar4r::", x))

  metric_funcs <- list(
    solar4r::mae,
    solar4r::mape,
    solar4r::nmae(capacity),
    solar4r::rmse,
    solar4r::rse
  )

  metric_quos <- rlang::quos(
    solar4r::mae,
    solar4r::mape,
    solar4r::nmae(capacity),
    solar4r::rmse,
    solar4r::rse
  )
  
  metric_funcs <- purrr::map2(.x = metric_quos, .y = metric_funcs, .f = function(fq, func) {
    func_name <- rlang::as_label(fq)
    if (func_name %in% metrics) func else NULL
  }) |> purrr::discard(.p = is.null)

  if (length(metric_funcs) == 0) {
    cli_abort("Could not identify any of the specified metric functions")
  }

  # Initialize model
  engine <- SolarModel$new(lat, lon, capacity, period, tz, metric_funcs)
  cli_alert_success("Model initialized: lat={.val {lat}}, lon={.val {lon}}, capacity={.val {capacity}}MW, tz={.val {tz}}")

  # Read data
  data_frame <- read.csv(input_file)
  cli_alert_success("Input file {.path {input_file}} successfully read")

  if (!any(c("POSIXct", "POSIXt") %in% class(data_frame[1, tstamp_col]))) {
    cli_alert_info("Converting column {.var {tstamp_col}} to POSIXct using format {.val {fmt}}")
    date_time <- as.POSIXct(data_frame[[tstamp_col]], fmt, tz = tz)
    data_frame[[tstamp_col]] <- date_time
  }

  # Prepare datasets
  prop <- config$prepare_data$prop %||% 0.8
  prop_cal <- config$prepare_data$prop_cal %||% 0.1
  method <- config$method %||% "laplacian"
  beta <- config$prepare_data$beta %||% 0.01
  add_weights <- config$prepare_data$add_weights %||% FALSE
  detect_outliers <- config$prepare_data$detect_outliers %||% FALSE
  drop_outliers <- config$prepare_data$drop_outliers %||% FALSE
  perc_conf <- config$prepare_data$perc_conf %||% 99
  col_names <- config$prepare_data$col_names

  if (is.null(col_names) && detect_outliers) {
    cli_abort("Detecting outliers requires specifying column names in {.code col_names}")
  }

  model_formula <- args$formula %||% config$prepare_data$model_formula
  if (is.null(model_formula)) {
    cli_abort("Model formula must be provided")
  } else {
    model_formula <- tryCatch(expr = as.formula(model_formula), error = function(msg) NULL)
    if (is.null(model_formula)) cli_abort("Invalid model formula: {.val {args$formula}}")
  }

  df_list <- engine$prepare_data(
    data_frame, tstamp_col, model_formula, 
    prop = prop, prop_cal = prop_cal, method = method,  
    beta = beta, add_weights = add_weights, 
    detect_outliers = detect_outliers, drop_outliers = drop_outliers,
    perc_conf = perc_conf, col_names = col_names
  )

  cli_alert_info("Data splitting complete: {.val {nrow(df_list$train)}} training rows, {.val {nrow(df_list$test)}} test rows")

  if (detect_outliers & !drop_outliers) {
    n <- nrow(df_list$outliers)
    cli_alert_warning("Detected {.val {n}} outliers in the dataset")
  }
  
  # Fit model
  parallel <- config$fit$parallel %||% FALSE
  n_folds <- config$fit$n_folds %||% 1
  train_grid <- config$fit$train_grid
  ctrl <- e1071::tune.control(
    sampling = config$fit$ctrl$sampling %||% "cross", 
    cross = config$fit$ctrl$cross %||% 5, 
    nrepeat= config$fit$ctrl$nrepeat %||% 1
  )
      
  cli_alert_info("Starting model fit (Parallel: {.val {parallel}}, Folds: {.val {n_folds}})")
  engine$fit(
      train_df = df_list$train,
      test_df = df_list$test,
      parallel = parallel,
      n_folds = n_folds, 
      ctrl = ctrl,
      train_grid = train_grid
  )
  
  if (!is.null(model_file)) {
    engine$save_model(model_file)
    cli_alert_success("Model successfully saved to {.path {model_file}}")
  }

  cli_alert_info("Generating performance report...")
  report <- generate_performance_report(test_df = df_list$test, model = engine)

  if (!is.null(output_file)) {
    jsonlite::write_json(report, path = output_file, auto_unbox = TRUE, pretty = TRUE)
    cli_alert_success("Report saved to {.path {output_file}}")
  } else {
    print(report)
  }

# --- EVAL MODE ---
} else if (mode == "eval") {
  
  if (is.null(model_file)) cli_abort("Model file not specified; unable to load trained model")
   
  engine <- load_solar_model(model_file)
  cli_alert_success("Model loaded from {.path {model_file}}")
  
  data_frame <- read.csv(input_file)
  cli_alert_success("Input file {.path {input_file}} read")

  if (!any(c("POSIXct", "POSIXt") %in% class(data_frame[1, tstamp_col]))) {
    cli_alert_info("Converting timestamp column {.var {tstamp_col}}")
    date_time <- as.POSIXct(data_frame[[tstamp_col]], fmt, tz = engine$tz)
    data_frame[[tstamp_col]] <- date_time
  }
  
  cli_alert_info("Generating predictions and report...")
  report <- generate_performance_report(test_df = data_frame, model = engine)

  if (!is.null(output_file)) {
    jsonlite::write_json(report, path = output_file, auto_unbox = TRUE, pretty = TRUE)
    cli_alert_success("Predictions saved to {.path {output_file}}")
  } else {
    print(report)
  }
} 

cli_rule(left = "Process Complete")