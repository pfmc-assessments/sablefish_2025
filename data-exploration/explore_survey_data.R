library(nwfscSurvey)
library(here)
library(dplyr)
library(ggplot2)

# This is looking at only WCGBT data
survey_catch <-
  fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "catch_sablefish_NWFSC.Combo") |>
  purrr::map(
    .f = function(x) {load(x); return(x)}
  ) 
catch <- survey_catch[[1]] |>
  dplyr::mutate(
    positive = dplyr::case_when(total_catch_numbers > 0 ~ 1, .default = 0),
    depth_bin = plyr::round_any(Depth_m, 50, floor)
  )

survey_bio <-
  fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_sablefish_NWFSC.Combo") |>
  purrr::map(
    .f = function(x) {load(x); return(x)}
  ) 
bio <- survey_bio[[1]] |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(
    state = dplyr::case_when(latitude_dd > 46.25 ~ "WA", latitude_dd < 42 ~ "CA", .default = "OR"),
    area = dplyr::case_when(latitude_dd >= 36 ~ "north of 36", .default = "south of 36"),
    length_bin = plyr::round_any(length_cm, 50, floor)
  )

ggplot(bio[bio$sex != "U",], aes(length_cm, fill = area)) +
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) +
  xlab("Length (cm)") + ylab("Density") +
  scale_fill_viridis_d() +
  facet_grid(sex~.)

ggplot(bio |> filter(!is.na(age), !is.na(length_cm)), aes(y = length_cm, x = age)) +
  geom_point(aes(col = sex), alpha = 0.30) +
  theme_bw() +
  facet_grid(area~.) +
  scale_colour_viridis_d() +
  xlab("Age") + ylab("Length (cm)")

ggplot(bio |> filter(!is.na(age), !is.na(length_cm), sex == "F"), aes(y = length_cm, x = age)) +
  geom_point(aes(col = area),) +
  theme_bw() +
  xlim(c(0, 50)) + 
  scale_colour_viridis_d() +
  xlab("Age") + ylab("Length (cm)")

ggplot(bio, aes(x = depth_m, y = age)) +
  geom_point(aes(col = sex)) +
  facet_grid(area~.)  +
  scale_colour_viridis_d() +
  ylab("Age") + xlab("Depth (m)")

ggplot(bio |> filter(!is.na(length_cm)), aes(x = depth_m, y = length_cm)) +
  geom_point(aes(col = sex)) +
  facet_grid(area~.)  +
  scale_colour_viridis_d() +
  ylab("Length (cm)") + xlab("Depth (m)")

#======================================================================
# Observed age and length by depth
#======================================================================

samples_by_depth <- bio |>
  dplyr::filter(sex != "U") |>
  dplyr::mutate(
    depth_bin = plyr::round_any(depth_m, 50, floor)
  ) |>
  dplyr::group_by(state, sex, depth_bin) |>
  dplyr::summarise(
    n_ages = sum(!is.na(age)),
    ave_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    age_lwr = qnorm(0.025, ave_age, sd_age), 
    age_upr = qnorm(0.975, ave_age, sd_age),
    n_lengths = sum(!is.na(length_cm)),
    ave_length = mean(length_cm, na.rm = TRUE),
    sd_length = sd(length_cm, na.rm = TRUE),
    len_lwr = qnorm(0.025, ave_length, sd_length), 
    len_upr = qnorm(0.975, ave_length, sd_length)
  ) |>
  dplyr::mutate(
    age_lwr = ifelse(age_lwr < 0, 0, age_lwr)
  )

ggplot(bio |> dplyr::filter(sex != "U"), aes(x = depth_m, y = age, color = sex)) +
  geom_smooth(method = "loess", na.rm = TRUE, se = TRUE) +
  xlab("Depth (m)") + ylab("Age") 

ggplot(samples_by_depth, aes(x = depth_bin, y = ave_age, fill = state, linetype = state)) +
  geom_line() +
  geom_ribbon(aes(ymin = age_lwr, ymax = age_upr), alpha = 0.1) +
  scale_fill_viridis_d() +
  theme_bw() +
  xlab("Depth (m)") + ylab("Average Age") +
  facet_grid(sex~.)

ggplot(samples_by_depth, aes(x = depth_bin, y = ave_length, fill = state, linetype = state)) +
  geom_line() +
  geom_ribbon(aes(ymin = len_lwr, ymax = len_upr), alpha = 0.1) +
  scale_fill_viridis_d() +
  theme_bw() +
  xlab("Depth (m)") + ylab("Average Length (cm)") +
  facet_grid(sex~.)

#======================================================================
# Plot a map with catch rates
#======================================================================
PlotMap.fn(
  dir = here::here("data-raw", "survey", "trawl"),
  dat = survey_catch[[1]],
  main = "all_data"
)

PlotMap.fn(
  dir = here::here("data-raw", "survey", "trawl"),
  dat = survey_catch[[1]] |>
    dplyr::filter(cpue_kg_km2 < quantile(cpue_kg_km2, 0.99)),
  main = "filtered_data"
)

large_tows <- survey_catch[[1]] |>
  dplyr::filter(cpue_kg_km2 >= quantile(cpue_kg_km2, 0.99)) |>
  dplyr::rename_all(tolower)
large_tows[large_tows$cpue_kg_per_ha_der == max(large_tows$cpue_kg_per_ha_der), "Trawl_id"]
# 202403017101

tow_bio <- bio |>
  dplyr::filter(trawl_id == "202403017101")
hist(tow_bio$length_cm)
# all 1 and 3 year olds

bio_big_tows <- dplyr::right_join(
  bio, large_tows
) |>
  dplyr::filter(year > 2020) |>
  dplyr::mutate(
    count = 1, 
    len_bin = plyr::round_any(length_cm, 1, floor)) 

ggplot(bio_big_tows |> dplyr::filter(year != 2024), aes(x = len_bin, y = count)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Length (cm)") + ylab("Count") +
  facet_grid(year~.)

bio |>
  dplyr::filter(year > 2020) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    n = sum(length_cm < 30),
    age_0 = sum(age == 0, na.rm = TRUE)
  )

bio$count <- 1
bio$length_bin <- plyr::round_any(bio$length_cm, 1, floor)
ggplot(bio |> dplyr::filter(year >= 2016), aes(x = length_bin, y = count)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Length (cm)") + ylab("Count") +
  facet_grid(year~.)

#======================================================================
# Explore the number of positive tows and the size/age of fish seen south of 
# 34.5 in the shallow depth bin 55-183m. The 2011 assessment excluded this
# area from the strata used to expand data.
#======================================================================
pos_tows <- catch |>
  dplyr::rename_all(tolower) |>
  dplyr::filter(latitude_dd < 34.5, cpue_kg_km2 > 0, depth_m <= 183)
south_bio <-  bio |>
  dplyr::filter(latitude_dd < 34.5, depth_m <= 183) |>
  dplyr::mutate(
    len_count = dplyr::if_else(!is.na(south_bio[, "length_cm"]), true = 1, false = 0),
    age_count = dplyr::if_else(!is.na(south_bio[, "age_years"]), true = 1, false = 0)
  )

ggplot(south_bio, aes(x = length_cm, y = len_count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Length (cm)") + ylab("Count of Observations")
ggplot(south_bio, aes(x = age_years, y = age_count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Age") + ylab("Count of Observations")
ggplot(south_bio, aes(x = age_years, y = length_cm)) + 
  geom_point() +
  theme_bw() + 
  xlab("Age") + ylab("Length (cm)")
# Given the frequency of observing age-0 and age-1 fish in this area, we should 
# include this area within the stratification similar to what was done in 2019.

# Where area age-0 being observed across years
young_fish <- bio |>
  dplyr::filter(age_years %in% 0:2) |>
  dplyr::mutate(
    lat_bin = plyr::round_any(latitude_dd, 1, floor),
    count = 1)

ggplot(young_fish |> dplyr::filter(age_years == 0), aes(x = lat_bin, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") 

ggplot(young_fish |> dplyr::filter(age_years == 1), aes(x = lat_bin, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") 

ggplot(young_fish |> dplyr::filter(age_years == 2), aes(x = lat_bin, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") 

ggplot(young_fish |> dplyr::filter(age_years == 0), aes(x = lat_bin, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") +
  facet_wrap(facets = "year")

gg <- ggplot2::ggplot(
  data = bio |>
    dplyr::filter(
      !is.na(age),
      age < 2
    ) |>
    dplyr::mutate(year = factor(year, levels = min(year):max(year))),
  ggplot2::aes(x = length_cm, y = year, fill = as.factor(pass))) +
  ggridges::geom_density_ridges2(alpha = 0.5,
                                 jittered_points = TRUE,
                                 point_alpha = 0.7,
                                 point_shape = 21,
                                 col = "blue")  +
  ggplot2::scale_fill_viridis_d(begin = 0, end = 0.5, name = "pass") +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::scale_y_discrete(drop = FALSE) +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 20)) +
  ggplot2::ylab("Year") + ggplot2::xlab("Length (cm)") +
  ggplot2::facet_grid(c("age"), labeller = ggplot2::label_both)
ggplot2::ggsave(
  filename = fs::path(
    "data-raw", "survey", "trawl", "figures",
    "data_survey_wcgbt_young-length-by-year.png"
  ),
  width = 16, 
  height = 16,
  plot = gg
)

gg <- ggplot2::ggplot(
  data = bio |>
    dplyr::filter(
      !is.na(Age),
      Age == 0
    ) |>
    dplyr::mutate(Year = factor(Year, levels = min(Year):max(Year))),
  ggplot2::aes(x = Length_cm, y = Year, fill = as.factor(Pass))) +
  ggridges::geom_density_ridges2(alpha = 0.5,
                                 jittered_points = TRUE,
                                 point_alpha = 0.7,
                                 point_shape = 21,
                                 col = "blue")  +
  ggplot2::scale_fill_viridis_d(begin = 0, end = 0.5, name = "Pass") +
  ggplot2::theme_bw(base_size = 20) +
  ggplot2::scale_y_discrete(drop = FALSE) +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 20)) +
  ggplot2::ylab("Year") + ggplot2::xlab("Length (cm)") 
ggplot2::ggsave(
  filename = fs::path(
    "data-raw", "survey", "trawl", "figures",
    "data_survey_wcgbt_age-0-length-by-year.png"
  ),
  width = 10, 
  height = 13,
  plot = gg
)
