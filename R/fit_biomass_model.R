#' Fit sdmTMB model to WCGBT data to be used to estimate biomass weighted
#' weight-at-age estimates by are.
#'
#' @author Eric Ward and Chantel Wetzel
fit_biomass_model <- function() {
  library(tidyverse) # 2.0.0
  library(nwfscSurvey)
  library(indexwc)
  library(sdmTMB) # version 0.6.0.9013
  
  config <- configuration
  config <- dplyr::filter(config, species == "sablefish", source == "NWFSC.Combo",
                          family == "sdmTMB::delta_gamma()")

  sablefish_data <- config |>
    dplyr::rowwise() |>
    dplyr::mutate(
      data_raw = list(format_data(  dplyr::rename(eval(parse(text = fxn)))  )),
      data_filtered = list(data_raw |>
                             dplyr::filter(
                               depth <= min_depth, depth >= max_depth,
                               latitude >= min_latitude, latitude <= max_latitude,
                               year >= min_year, year <= max_year
                             ))
    ) |>
    dplyr::ungroup()
  catch <- sablefish_data$data_filtered[[1]]
  saveRDS(
    catch,
    here::here("data-raw", "survey", "trawl", "indices", "wcgbt_data_filtered.rds")
  )
  
  # missing the date -- need to pull that in
  haul <- nwfscSurvey::pull_haul(survey = "NWFSC.Combo") |>
    dplyr::rename(latitude = latitude_dd, longitude = longitude_dd)
  join <- dplyr::left_join(
    catch, 
    dplyr::select(haul, latitude, longitude, year, date_formatted))
  join$doy <- lubridate::yday(join$date_formatted)

  join <- sdmTMB::add_utm_columns(join, ll_names = c("longitude", "latitude"))
  # 761 knots
  mesh <- sdmTMB::make_mesh(data = join, xy_cols = c("X","Y"), n_knots = config$knots)

  fit <- sdmTMB::sdmTMB(
    as.formula(config$formula),
    time="year",
    mesh = mesh,
    family = delta_gamma(),
    spatiotemporal = "iid",
    share_range = config$share_range,
    data = join)
  #extra_time = extra_time)
  saveRDS(
    fit, 
    here::here("data-raw", "survey", "trawl", "indices", "wcgbt_fit_biomass.rds"))

  # make predictions. this is wcbt sampling grid with depth added
  cells <- readRDS(here::here("data", "wc_grid.rds"))
  range_yrs <- range(c(unique(join$year)))
  cells <- sdmTMB::replicate_df(cells, "year", seq(range_yrs[1], range_yrs[2]))
  cells$fyear <- as.factor(cells$year)
  cells$pass_scaled <- 0
  cells <- dplyr::filter(cells, year %in% unique(join$year))

  pred <- predict(fit, cells)
  pred$est <- plogis(pred$est1) * exp(pred$est2)
  pred <- dplyr::select(pred, year, X, Y, est)
  saveRDS(
    pred, 
    here::here("data-raw", "survey", "trawl", "indices", "wcgbt_pred_biomass.rds"))
}
