#' Fill in necessary years of weight-at-age file
#'
#' @details
#' Note that this function can also be used to pad maturity information
#' such that it has the same format as the weight-at-age information allowing
#' for the multiplication of maturity by weight at age to get fecundity.
#'
#' @param data A data frame of weight-at-age data in long form where the
#'   weight-at-age information is stored in a column called `pred_weight`,
#'   the four-digit year is in a column called `year`, and integer ages are
#'   in a column called `age`.
#' @param n_forecast_years An integer specifying the number of forecast years
#'   you want to extend the time series by.
#' @param n_years_used_for_forecast An integer specifying the number of years
#'   of data that you want to average over for the forecast period.
#' @param year_global_average A four digit integer, typically `-1940`, that is
#'   used to delineate the equilibrium year, all years after this and prior to
#'   the first year of data will be filled in by the information stored in this
#'   row.
#' @param ages A vector of integers specifying the ages you want in your file.
#'   If there are no ages in the data to represent the older ages present in
#'   vector, then the information for the oldest age will be repeated for any
#'   given year.
#' @author Kelli F. Johnson
#' @return
#' A data frame of weight-at-age information is returned that can immediately
#' be passed to [r4ss::SS_writewtatage()].
pad_weight_at_age <- function(
  data,
  first_year = 1997,
  n_forecast_years = 5,
  n_years_used_for_forecast = 5,
  year_global_average = -1892,
  ages = 0:30) {
  data <- dplyr::mutate(
    data,
    pred_weight = ifelse(is.infinite(pred_weight), NA, pred_weight),
    period = "annual"
  )
  years_used_for_forecast <- sort(unique(data[["year"]]))[length(sort(unique(data[["year"]]))) -
    0:(n_years_used_for_forecast - 1)]
  global_average <- data |>
    dplyr::group_by(sex, age) |>
    dplyr::reframe(
      pred_weight = mean(pred_weight, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      year = year_global_average,
      period = "all-years") |>
    dplyr::ungroup()
  forecast_average <- data |>
    dplyr::filter(
      year %in% years_used_for_forecast
    ) |>
    dplyr::group_by(sex, age) |>
    dplyr::reframe(
      pred_weight = mean(pred_weight, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      year = max(data[["year"]]) + 1,
      period = "forecast") |>
    dplyr::ungroup()
  forecast_df <- forecast_average
  for(y in 1:(n_forecast_years-1)) {
    to_add <- forecast_average
    to_add[["year"]] <- forecast_average[["year"]] + y
    forecast_df <- dplyr::bind_rows(forecast_df, to_add)
  }
  
  ave_weights <- dplyr::bind_rows(
    global_average,
    forecast_average
  ) |> 
    dplyr::filter(sex != "U")
  
  gg <- ggplot(ave_weights, aes(x = age, y = pred_weight, color = period)) +
    geom_line(linewidth = 1.5) +
    theme_bw() + 
    xlab("Age") + ylab("Average Weight (kg)") +
    scale_color_viridis_d() + 
    facet_grid(sex~.)
  ggplot2::ggsave(
    gg,
    width = 10, height = 7, units = "in",
    filename = here::here("data-raw", "weight_at_age", "plots", "average_weight_by_period.png")
  )
  
  years_to_keep <- c(year_global_average, first_year:max(forecast_df[["year"]]))
  temp <- dplyr::bind_rows(data, global_average, forecast_df) |>
    dplyr::select(-period) |>
    dplyr::filter(year %in% years_to_keep) |>
    dplyr::ungroup() |>
    tidyr::complete(
      year = c(
        years_to_keep,
        max(data[["year"]]) + 1:n_forecast_years
      ),
      age = ages
    ) |>
    tidyr::pivot_wider(
      id_cols = c(year, sex),
      names_from = age,
      values_from = pred_weight
    ) #|>
    # this fills in NA years, but is not doing it by sex
    #tidyr::fill(dplyr::matches("[0-9]+")) #|>
    #tidyr::pivot_longer(
    #  cols = -year,
    #  names_to = "age"
    #) #|>
    #dplyr::mutate(age = as.numeric(age)) |>
    #dplyr::arrange(year, age) |>
    #tidyr::fill(value) |>
    #tidyr::pivot_wider(
    #  id_cols = year,
    #  names_from = age,
    #  values_from = value
    #)
  
  # TODO: 
  # 1. Decide how/when to remove unsexed fish
  # 2. Deal with missing 2020 estimates
  
  finish <- temp |>
    dplyr::rename(yr = year) |>
    dplyr::mutate(
      Seas = 1,
      Sex = 1,
      Bio_Pattern = 1,
      BirthSeas = 1,
      fleet = 1,
      .after = "yr"
    ) |>
    as.data.frame()
  return(finish)
}