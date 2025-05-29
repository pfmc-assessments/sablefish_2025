ashop_lengths_early <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sablefish_Lengths_1978-83_021319.xlsx"),
  sheet = "Sheet1") |>
  dplyr::rename_with(
    tolower
  ) |>
  tidyr::uncount(frequency) |>
  dplyr::mutate(
    sex = nwfscSurvey::codify_sex(sex),
    common_name = "sablefish",
    trawl_id = haul_join,
    length = size_group
  ) |> as.data.frame()


ashop_lengths <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sablefish Lengths_102224.xlsx"),
  sheet = "2019-2024") |>
  dplyr::rename_with(
    tolower
  ) |>
  tidyr::uncount(frequency) |>
  dplyr::mutate(
    sex = nwfscSurvey::codify_sex(sex),
    common_name = "sablefish",
    trawl_id = haul_join
  ) |> as.data.frame()

all_ashop <- dplyr::bind_rows(ashop_lengths_early, ashop_lengths)

ggplot(all_ashop, aes(x = length, fill = as.factor(year))) +
  geom_density(alpha = 0.4) +
  scale_color_viridis_c() +
  theme_bw() 

ggplot(all_ashop, aes(x = length)) +
  geom_density(alpha = 0.4, linewidth = 1) +
  scale_color_viridis_c() +
  theme_bw() + xlim(25, 80) +
  facet_wrap(facets = "year", ncol = 2, dir = "v")

table(all_ashop$year)
