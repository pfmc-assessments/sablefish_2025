
# Maturity
pop_max_age <- 70
# Load in the maturity data
maturity_data <- readRDS(
  here::here("data-raw", "maturity", "sablefish_maturity.rds")) |>
  dplyr::mutate(
    Area = dplyr::case_when(latitude >= 36 ~ "North", .default = "South")
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

spatial_maturity <- readRDS(
  here::here("data-raw", "maturity", "ogives_for_assessment.rds")
)

all_ages <- data.frame(age = 0:pop_max_age)

maturity <- all_ages |>
  dplyr::left_join(spatial_maturity |> dplyr::filter(Model == "Spatial") ) |>
  tidyr::fill(p, .direction = "downup") |>
  dplyr::select(p) |>
  dplyr::mutate(
    age = 0:70
  ) |>
  dplyr::rename(
    maturity = p
  )

usethis::use_data(
  maturity,
  overwrite = TRUE
)

usethis::use_data(
  maturity_data,
  overwrite = TRUE
)

usethis::use_data(
  spatial_maturity,
  overwrite = TRUE
)