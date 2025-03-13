library(r4ss)

wtatage_model <- SS_readwtatage(
  file = here::here("data-processed", "wtatage_model_biomass_weighted.ss")) |>
  dplyr::filter(fleet == 1) |>
  dplyr::select(-seas, -bio_pattern, -birthseas) |>
  tidyr::pivot_longer(
    cols = 4:34,
    names_to = "age",
    values_to = "weight" 
  ) |>
  dplyr::filter(year < 2025, year > 2002) |>
  dplyr::mutate(
    sex = dplyr::case_when(sex == 1 ~ "Female", .default = "Male"),
    #year = dplyr::case_when(year == -1890 ~ 1890, .default = year),
    age = as.numeric(age),
    year = as.factor(year)
  ) |>
  dplyr::rename(Year = year)
wtatage_empirical <- SS_readwtatage(
  file = here::here("data-processed", "wtatage_interploations.ss")) |>
  dplyr::filter(fleet == 1) |>
  dplyr::select(-seas, -bio_pattern, -birthseas) |>
  tidyr::pivot_longer(
    cols = 4:74,
    names_to = "age",
    values_to = "weight" 
  ) |>
  dplyr::filter(year < 2025, age < 31) |>
  dplyr::mutate(
    sex = dplyr::case_when(sex == 1 ~ "Female", .default = "Male"),
    year = dplyr::case_when(year == -1892 ~ 1890, .default = year),
    age = as.numeric(age),
    year = as.factor(year)
  )
wtatage_growth <- SS_readwtatage(
  file = here::here("model", "_bridging", "1_fleet_numbering", "wtatage.ss_new")) |>
  dplyr::filter(fleet == 1) |>
  dplyr::select(-seas, -bio_pattern, -birthseas) |>
  tidyr::pivot_longer(
    cols = 4:74,
    names_to = "age",
    values_to = "weight" 
  ) |>
  dplyr::filter(year == 1890, age < 31) |>
  dplyr::mutate(
    sex = dplyr::case_when(sex == 1 ~ "Female", .default = "Male"),
    age = as.numeric(age),
    Year = "Model (2023)"
  )

watage <- dplyr::bind_rows(
  wtatage_model, wtatage_growth
)

ggplot() +
  geom_line(data = wtatage_model, aes(x = age, y = weight, color = Year), linewidth = 1.0) +
  geom_line(data = wtatage_growth, aes(x = age, y = weight), 
            color = "black", linewidth = 1.5, linetype = 2) +
  scale_color_viridis_d() +
  xlab("Age (years)") + ylab("Weight (kg)") +
  theme_bw() +
  theme(
    text = element_text(size = 15),
    axis.text = element_text(size = 15)
  ) +
  facet_grid(sex~.)






