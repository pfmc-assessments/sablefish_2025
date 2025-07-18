# Create the data file that will be used
process_weight_at_age_survey(savedir = here::here())
process_weight_at_age_fishery(savedir = here::here())

#=======================================================================
# Time-varying weight-at-age
#=======================================================================
# Currently uses data from the WCGBT, Triennial, and AFSC Slope surveys
# The below function uses output from fit_biomass_model() that fits
# WCGBT data for a species distribution model that is then used to
# create a prediction grid to biomass weight the weight-at-age model
# estimates.
wtatage <- estimate_tv_wtatage_weighted(
  data = read.csv(here::here("data-processed", "data_weight_at_age_survey.csv")) |>
    dplyr::filter(source == "Groundfish Slope and Shelf Combination Survey") |> 
    dplyr::mutate(data_type = "wcgbt"),
  max_age = 30)

format_wtatage <- pad_weight_at_age(
  data = wtatage,
  first_year = 2003,
  n_forecast_years = 12,
  n_years_used_for_forecast = 5,
  year_global_average = -1890,
  ages = 0:30)

write_wtatage_file(
  file = here::here("data-processed", "wtatage_model_biomass_weighted_fleet=11.ss"),
  data = format_wtatage,
  maturity = maturity$maturity[1:31],
  max_age = 30,
  n_fleet = 11
)

ggplot(wtatage, aes(x = age, y = year, z = pred_weight)) +
  stat_summary_2d(geom = "tile") + #, fun = function(x) cut(mean(x), breaks = breaks, right = FALSE))  +
  #xlim(0, 15) +
  facet_grid(sex~.)

#=======================================================================
# Time-varying weight-at-age: fishery and survey
#=======================================================================

wtatage <- estimate_tv_wtatage_weighted(
  data = dplyr::bind_rows(
    read.csv(here::here("data-processed", "data_weight_at_age_survey.csv")),
    read.csv(here::here("data-processed", "data_weight_at_age_fishery.csv"))) |> 
    dplyr::mutate(data_type = "all_fishery_survey"), 
  max_age = 30,
  do_plots = FALSE)

format_wtatage <- pad_weight_at_age(
  data = wtatage,
  first_year = 2003,
  n_forecast_years = 12,
  n_years_used_for_forecast = 5,
  year_global_average = -1890,
  ages = 0:30)

write_wtatage_file(
  file = here::here("data-processed", "wtatage_model_biomass_weighted_fishery_survey_fleet=11.ss"),
  data = format_wtatage,
  maturity = maturity$maturity[1:31],
  max_age = 30,
  n_fleet = 11
)

#=======================================================================
# Weight-at-age
#=======================================================================

wtatage <- estimate_weight_at_age(
  max_age = 30)

format_wtatage <- wtatage |>
  tidyr::pivot_wider(
    id_cols = c(year, sex),
    names_from = age,
    values_from = pred_weight
  ) |>
  dplyr::mutate(
    seas = 1,
    sex = dplyr::case_when(sex == "F" ~ 1, .default = 2),
    gp = 1,
    bseas = 1,
    fleet = 1,
    .after = "year"
  ) |>
  dplyr::relocate(sex, .after = seas) |>
  as.data.frame()

write_wtatage_file(
  file = here::here("data-processed", "wtatage_model_static_fleet=11.ss"),
  data = format_wtatage,
  maturity = maturity$maturity[1:31],
  max_age = 30,
  n_fleet = 11
)


#===============================================================================
# Empirical weight-at-age 
# Old functions that calculate empirical weight-at-age
# Work horse function that estimates, plots, and writes weight-at-age files for SS3
#===============================================================================

process_weight_at_age(
  dir = here::here(),
  max_age = 30,
  years = 1997:2024,
  n_avg_years = 5,
  n_forecast = 12,
  maturity = maturity$maturity
)
 
# wt_by_cohort <- estimate_tv_weight_at_age(
#   max_age = 30, 
#   first_year = 1997)# 