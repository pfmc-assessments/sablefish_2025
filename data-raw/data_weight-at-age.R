library(ggplot2)

# Maturity
pop_max_age <- 70
# Load in the maturity data
maturity_data <- readRDS(
  here::here("data-raw", "maturity", "sablefish_maturity.rds")) |>
  dplyr::mutate(
    Area = dplyr::case_when(latitude >= 36 ~ "North", .default = "South")
  )

gg <- ggplot(maturity_data, aes(x = age, y = functional_maturity)) +
  geom_point() +
  ylab("Functional Maturity") +
  xlab("Age years") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  facet_wrap(Area ~ .)
ggsave(
  gg,
  width = 7, height = 7, units = "in",
  filename = here::here("data-raw", "maturity",  "plots", "maturity_by_area.png")
)

samples <- maturity_data |>
  dplyr::group_by(year, Area) |>
  dplyr::summarise(
    Samples = dplyr::n()
  ) |>
  tidyr::pivot_wider(
    names_from = Area,
    values_from = Samples,
    names_prefix = "Samples-"
  )
utils::write.csv(
  samples,
  file = here::here("data-processed", "data_maturity_samples.csv"),
  row.names = FALSE
)
maturity_at_age <- readRDS(
  here::here("data-raw", "maturity", "ogives_for_assessment.rds")
)

gg <- ggplot(maturity_at_age, aes(x = age, y = p, color = Model)) +
  geom_line(linewidth = 1) +
  ylab("Probability Mature") +
  xlab("Age years") +
  scale_colour_viridis_d() + 
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))
ggsave(
  gg,
  width = 7, height = 7, units = "in",
  filename = here::here("data-raw", "maturity",  "plots", "maturity_ogive.png")
)

all_ages <- data.frame(age = 0:pop_max_age)

maturity <- all_ages |>
  dplyr::left_join(maturity_at_age |> dplyr::filter(Model == "Spatial") ) |>
  tidyr::fill(p, .direction = "downup") |>
  dplyr::select(p) |>
  as.matrix()

#=======================================================================
# Weight-at-age
#=======================================================================
# Currently uses data from the WCGBT, Triennial, and AFSC Slope surveys
survey_watage_data <- process_weight_at_age_survey(
  savedir = here::here())

# The below function uses output from fit_biomass_model() that fits
# WCGBT data for a species distribution model that is then used to
# create a prediction grid to biomass weight the weight-at-age model
# estimates.
wtatage <- estimate_tv_wtatage_weighted(
  max_age = 30)

format_wtatage <- pad_weight_at_age(
  data = wtatage,
  first_year = 2003,
  n_forecast_years = 12,
  n_years_used_for_forecast = 5,
  year_global_average = -1892,
  ages = 0:30)

write_wtatage_file(
  file = here::here("data-processed", "wtatage.ss"),
  data = format_wtatage,
  maturity = maturity,
  max_age = 70,
  n_fleet = 8
)

# Old functions that calculate empirical weight-at-age
# Work horse function that estimates, plots, and writes weight-at-age files for SS3
# process_weight_at_age(
#   dir = here::here(),
#   max_age = 30,
#   years = 1997:2024,
#   n_avg_years = 5,
#   n_forecast = 12,
#   maturity = maturity
# )
# 
# wt_by_cohort <- estimate_tv_weight_at_age(
#   max_age = 30, 
#   first_year = 1997)# 