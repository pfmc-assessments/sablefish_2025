
data <- read.csv(here::here("data-processed", "data-commercial-comps_age-0-50.csv")) 

data_combine <- data |>
  dplyr::filter(fleet %in% 2:3) |>
  #dplyr::select(-month, -fleet, -ageerr, -Lbin_lo, -Lbin_hi) |>
  tidyr::pivot_longer(
    cols = 10:111,
    names_to = "age", 
    values_to = "comp") |>
  dplyr::group_by(year, sex, age) |>
  dplyr::summarise(
    total_comp = sum(comp)
  ) |>
  tidyr::pivot_wider(
    names_from = age,
    values_from = total_comp
  )
write.csv(
  data_combine,
  file = here::here("data-processed", "data-commercial-fixed-gear-comps_age-0-50.csv"),
  row.names = FALSE)

data_long <- data |>
  dplyr::filter(fleet %in% 2:3) |>
  #dplyr::select(-month, -fleet, -ageerr, -Lbin_lo, -Lbin_hi) |>
  tidyr::pivot_longer(
    cols = 10:111,
    names_to = "age", 
    values_to = "comp") |>
  dplyr::group_by(year, sex, age) |>
  dplyr::summarise(
    total_comp = sum(comp)
  ) |>
  dplyr::mutate(
    sex_chara = as.factor(substring(age, first = 1, last = 1)),
    sex_chara = dplyr::case_when(sex == 0 ~ "u", .default = sex_chara),
    sex_chara = as.factor(sex_chara),
    sex_type = dplyr::case_when(sex_chara == "u" ~ "u", .default = "b"),
    age_number = as.numeric(substring(age, first = 2, last = 3))
  ) |>
  dplyr::group_by(year, sex_type) |>
  dplyr::mutate(
    prop = 100 * total_comp / sum(total_comp),
    prop = dplyr::case_when(sex_chara == "m" ~ -1 * prop, .default = prop)
  )

library(ggplot2)
ggplot(data_long |> 
      dplyr::filter(sex_chara %in% c("m", "f"), year <= 1995), 
       aes(x = age_number, y = prop, color = sex_chara)) +
  geom_line(linewidth = 1) + 
  facet_wrap("year")

ggplot(data_long |> 
         dplyr::filter(sex_chara %in% c("m", "f"), year %in% c(1996:2004)), 
       aes(x = age_number, y = prop, color = sex_chara)) +
  geom_line(linewidth = 1) + 
  facet_wrap("year")

ggplot(data_long |> 
         dplyr::filter(sex_chara %in% c("m", "f"), year %in% c(2005:2013)), 
       aes(x = age_number, y = prop, color = sex_chara)) +
  geom_line(linewidth = 1) + 
  facet_wrap("year")
  

ggplot(data_long |> 
         dplyr::filter(sex_chara %in% c("m", "f"), year %in% c(2014:2022)), 
       aes(x = age_number, y = prop, color = sex_chara)) +
  geom_line(linewidth = 1) + 
  facet_wrap("year")
