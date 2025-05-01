# Environmental index
data_enviro_index <- read.csv(
  "C:/Assessments/2025/sablefish-risk-table/Environmental-index-4Assessement-1993-2024.csv"
) |>
  dplyr::select(-Y_rec) |>
  dplyr::rename(
    obs = fit,
    se_log = se
  ) |>
  dplyr::mutate(
    month = 1,
    index = 11,
    obs = round(obs, 3),
    se_log = round(se_log, 4)
  ) |>
  dplyr::relocate(
    month, .after = year
  ) |>
  dplyr::relocate(
    index, .after = month
  ) |>
  dplyr::filter(year > 2014)

write_named_csvs(
  data_enviro_index,
  dir = here::here("data-processed")
)
