

cols_to_keep <- c(
  "Project",
  "Year",
  "Sex",
  "Age_years", 
  "Length_cm", 
  "Weight_kg",
  "Depth_m", 
  "Latitude_dd"
)

survey_ages <- dplyr::bind_rows(
  data_survey_bio$nwfsc_combo |> dplyr::select(dplyr::all_of(cols_to_keep)), 
  data_survey_bio$nwfsc_slope |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$afsc_slope$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_early$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_late$age_data |> dplyr::select(dplyr::all_of(cols_to_keep))
) |>
  dplyr::rename(
    source = Project,
    latitude= Latitude_dd) |>
  dplyr::mutate(
    geargroup = "TWL",
    state = dplyr::case_when(latitude > 46.25 ~ "WA", latitude < 42 ~ "CA", .default = "OR"),
    area = dplyr::case_when(latitude > 36 ~ "North", .default = "South")
  ) |>
  dplyr::rename_all(tolower)


fishery_ages <- data_commercial_bds |>
  dplyr::mutate(
    source = "Commercial",
    weight_kg = NA,
    depth_m = NA)


data_length_ages <- dplyr::bind_rows(
  survey_ages, fishery_ages
)

#usethis::use_data(
#  data_length_ages,
#  overwrite = TRUE
#)

ages <- data_length_ages |> 
  dplyr::filter(!is.na(age_years), sex != "U") |>
  dplyr::group_by(source, sex) |>
  dplyr::reframe(
    n_70_plus = sum(age_years >= 70),
    n_80_plus = sum(age_years >= 80),
    min_age = min(age_years),
    median_age = median(age_years),
    age_99 = quantile(age_years, 0.995),
    max_age = max(age_years)
  )

ggplot(data_length_ages |> dplyr::filter(!is.na(age_years), sex != "U"), 
       aes(x = age_years, fill = sex)) +
  geom_density() +
  scale_fill_viridis_d() +
  theme_bw() +
  facet_grid(sex~.)


ages_trend <- data_length_ages |> 
  dplyr::filter(!is.na(age_years), sex != "U") |>
  dplyr::group_by(source, year, sex) |>
  dplyr::summarise(
    ave_age = mean(age_years)
  )
ggplot(ages_trend, aes(x = year, y = ave_age, color = sex)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(source~.)

max_age_by_year <- data_length_ages |> 
  dplyr::filter(!is.na(age_years), sex != "U") |>
  dplyr::group_by(year, sex) |>
  dplyr::summarize(
    max_age = max(age_years)
  )

ggplot(max_age_by_year, aes(x = year, y = max_age, color = sex)) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  theme_bw()

#===============================================================================
# Evaluate California otolith collections, ages, and landings
#===============================================================================
ca_otoliths <- read.csv(here::here("data-raw", "bds", "ca_otolith_counts_by_port.csv")) |>
  dplyr::mutate(
    state = "CA"
  ) |>
  tidyr::pivot_longer(
    cols = 2:10,
    names_to = "port",
    values_to = "count"
  ) |>
  dplyr::mutate(
    count = dplyr::case_when(is.na(count) ~ 0, .default = count),
    area = dplyr::case_when(
      port %in% c("OSF", "OSD", "OSB", "MRO") ~ "South", .default = "North"
    )
  )

ggplot(ca_otoliths, aes(x = Year, y = count, fill = port)) +
  ggplot2::geom_bar(position="stack", stat="identity") +
  scale_fill_viridis_d() + 
  ylab("Otoliths Collected in CA") +
  theme_bw() +
  facet_grid(area~.)

gg1 <- ggplot(ca_otoliths, aes(x = Year, y = count, fill = area)) +
  ggplot2::geom_bar(position="stack", stat="identity") +
  scale_fill_viridis_d() + 
  ylab("Otoliths Collected in CA") +
  theme_bw()

gg2 <- ggplot(fishery_ages |> 
       dplyr::filter(state == "CA", year >= 2007) |>
       dplyr::mutate(count = dplyr::case_when(!is.na(age_years) ~ 1, .default = 0)), 
       aes(x = year, y = count, fill = area)) +
  ggplot2::geom_bar(position="stack", stat="identity") +
  ylim(c(0, 1550)) + 
  scale_fill_viridis_d() + 
  ylab("Ages in CA") +
  theme_bw()

gg3 <- ggplot(data_commercial_catch |> 
                dplyr::filter(state == "CA", year >= 2007, area != "rec") |>
                dplyr::mutate(
                  area = dplyr::case_when(area == "north" ~ "North", .default = "South")
                ),
  aes(x = year, y = catch_mt, fill = area)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Landings (mt)") + xlab("Year") + 
  scale_fill_viridis_d()


ggsave(
  cowplot::plot_grid(gg1, gg2, gg3, ncol = 1),
  filename = here::here("data-raw", "bds", "plots", "CA_age_otolith_collections.png"),
  height = 12, width =  7
)
