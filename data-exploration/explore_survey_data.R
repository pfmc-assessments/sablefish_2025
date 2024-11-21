library(nwfscSurvey)
library(here)
library(dplyr)
library(ggplot2)

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
    area = dplyr::case_when(latitude_dd >= 36 ~ "north of 36", .default = "south of 36"),
    length_bin = plyr::round_any(length_cm, 50, floor)
  )

ggplot(bio[bio$sex != "U",], aes(length_cm, fill = sex, color = sex)) +
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) +
  xlab("Length (cm)") + ylab("Density") +
  scale_fill_viridis_d() +
  facet_grid(area~.)

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

# Explore the number of positive tows and the size/age of fish seen south of 
# 34.5 in the shallow depth bin 55-183m. The 2011 assessment excluded this
# area from the strata used to expand data.
pos_tows <- catch |>
  dplyr::filter(Latitude_dd < 34.5, cpue_kg_km2 > 0, Depth_m <= 183)
south_bio <-  bio |>
  dplyr::filter(Latitude_dd < 34.5, Depth_m <= 183) 
south_bio[, "len_count"] <- dplyr::if_else(!is.na(south_bio[, "Length_cm"]), true = 1, false = 0)
south_bio[, "age_count"] <- dplyr::if_else(!is.na(south_bio[, "Age_years"]), true = 1, false = 0)

ggplot(south_bio, aes(x = Length_cm, y = len_count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Length (cm)") + ylab("Count of Observations")
ggplot(south_bio, aes(x = Age_years, y = age_count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Age") + ylab("Count of Observations")
ggplot(south_bio, aes(x = Age_years, y = Length_cm)) + 
  geom_point() +
  theme_bw() + 
  xlab("Age") + ylab("Length (cm)")
# Given the frequency of observing age-0 and age-1 fish in this area, we should 
# include this area within the stratification similar to what was done in 2019.

# Where area age-0 being observed across years
young_fish <- bio |>
  dplyr::filter(Age_years %in% 0:2)
young_fish[, "lat"] <- plyr::round_any(young_fish[, "Latitude_dd"], 1, floor)
young_fish[, "count"] <- 1

ggplot(young_fish |> dplyr::filter(Age_years == 0), aes(x = lat, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") 

ggplot(young_fish |> dplyr::filter(Age_years == 1), aes(x = lat, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") 

ggplot(young_fish |> dplyr::filter(Age_years == 2), aes(x = lat, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") 

ggplot(young_fish |> dplyr::filter(Age_years == 0), aes(x = lat, y = count)) + 
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  xlab("Latitude") + ylab("Count") +
  facet_wrap(facets = "Year")

gg <- ggplot2::ggplot(
  data = bio |>
    dplyr::filter(
      !is.na(Age),
      Age < 2
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
  ggplot2::ylab("Year") + ggplot2::xlab("Length (cm)") +
  ggplot2::facet_grid(c("Age"), labeller = ggplot2::label_both)
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
