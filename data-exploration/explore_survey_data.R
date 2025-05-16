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
    depth_bin = plyr::round_any(Depth_m, 50, floor),
    state = dplyr::case_when(Latitude_dd > 46.25 ~ "WA", 
                             Latitude_dd < 36.0 ~ "SCA", 
                             Latitude_dd >= 36.0 & Latitude_dd < 42.0 ~ "NCA",
                             .default = "OR"),
    area = dplyr::case_when(Latitude_dd > 36 ~ "North of 36", .default = "South of 36")
  )

survey_bio <-
  fs::dir_ls(here::here("data-raw", "survey", "trawl"), regex = "bio_sablefish_NWFSC.Combo") |>
  purrr::map(
    .f = function(x) {load(x); return(x)}
  ) 
bio <- survey_bio[[1]] |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(
    state = dplyr::case_when(latitude_dd > 46.25 ~ "WA", 
                             latitude_dd < 36.0 ~ "SCA", 
                             latitude_dd >= 36.0 & latitude_dd < 42.0 ~ "NCA",
                             .default = "OR"),
    area = dplyr::case_when(latitude_dd >= 36 ~ "North of 36", .default = "South of 36"),
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
cpue <- catch |>
  group_by(Year, state) |>
  summarise(
    med_cpue = median(cpue_kg_km2)
  )
ggplot(cpue, aes(x = Year, y = med_cpue, color = state)) +
  geom_line(linewidth = 1.5) +
  scale_color_viridis_d() +
  ylab("Median CPUE kg/km2") +
  theme_bw()


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
#=================================================================================
#
#===============================================================================
age_comps <- read.csv(here::here("data-processed", "data-survey-comps-ages-wcgbt-unsexed.csv")) |>
  dplyr::select(-month, -fleet, -partition, -ageerr, -Lbin_lo, -Lbin_hi, -input_n) |>
  tidyr::pivot_longer(
    cols = 3:53,
    names_to = "age",
    values_to = "prop"
  ) |>
  dplyr::mutate(
    sex_chara = as.factor(substring(age, first = 1, last = 1)),
    sex_chara = dplyr::case_when(sex == 0 ~ "u", .default = sex_chara),
    sex_chara = as.factor(sex_chara),
    sex_type = dplyr::case_when(sex_chara == "u" ~ "u", .default = "b"),
    age_number = as.numeric(substring(age, first = 2, last = 3)), 
    prop = round(prop / 100, 3)
  ) |>
  dplyr::select(-sex, -age) |>
  dplyr::rename(
    sex = sex_chara,
    age = age_number) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    prop_to_add = dplyr::case_when(age >= 15 ~ prop, .default = 0),
    plus_group = dplyr::case_when(age >=15 ~ sum(prop_to_add), .default = 0),
    prop = dplyr::case_when(age == 15 ~ plus_group, .default = prop),
    group = dplyr::case_when(year <= 2013 ~ 1, .default = 2)
  )


gg <- ggplot(age_comps |> dplyr::filter(age < 16) |>dplyr::mutate(age = as.factor(age)), aes(x = age, y = prop, fill = age)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  geom_text(aes(label = year),
            x = 10,
            y = 0.4,
            size = 6) +
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0, 0.50, by = 0.25)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0, "cm"),
        strip.text.x = element_blank(),
        plot.margin = margin(12, 12, 6, 12),
        text = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        axis.text = element_text(size = 18),
        legend.position = "none") +
  ylab("Proportion") +
  xlab("Age") + 
  facet_wrap("year", dir = "v", ncol = 2)

ggplot2::ggsave(
  gg,
  filename = here::here(
    "quarto_website",
    "figures",
    "survey_ages.png"
  ),
  width = 10, 
  height = 10
)


length_comps <- read.csv(here::here("data-processed", "data-survey-comps-lengths-wcgbt-unsexed.csv")) |>
  dplyr::select(-month, -fleet, -partition, -input_n) |>
  tidyr::pivot_longer(
    cols = 3:39,
    names_to = "length",
    values_to = "prop"
  ) |>
  dplyr::mutate(
    sex_chara = as.factor(substring(length, first = 1, last = 1)),
    sex_chara = dplyr::case_when(sex == 0 ~ "u", .default = sex_chara),
    sex_chara = as.factor(sex_chara),
    sex_type = dplyr::case_when(sex_chara == "u" ~ "u", .default = "b"),
    length_bin = as.numeric(substring(length, first = 2, last = 3)), 
    prop = round(prop / 100, 3)
  ) |>
  dplyr::select(-sex, -length) |>
  dplyr::rename(
    sex = sex_chara) 

gg <- ggplot(length_comps |> dplyr::filter(length_bin <= 80) |> dplyr::mutate(length_bin = as.factor(length_bin)), aes(x = length_bin, y = prop, fill = length_bin)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +
  facet_wrap("year", ncol = 2, dir = "v") +
  geom_text(aes(label = year),
            x = 25,
            y = 0.10,
            size = 8) +
  scale_y_continuous(limits = c(0, 0.22), breaks = seq(0, 0.25, by = 0.10)) +
  scale_x_discrete(
    breaks = seq(18, 90, 6)) +
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.spacing = unit(0, "cm"),
        strip.text.x = element_blank(),
        plot.margin = margin(12, 12, 6, 12),
        text = element_text(size = 20),
        axis.text = element_text(size = 18),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.position = "none") +
  ylab("Proportion") +
  xlab("Length (cm)") 
ggplot2::ggsave(
  gg,
  filename = here::here(
    "quarto_website",
    "figures",
    "survey_lengths.png"
  ),
  width = 10, 
  height = 10
)

#===============================================================================
# Look at the data by sex and age
#===============================================================================

sex_ratio  <- data_survey_bio$nwfsc_combo |>
  dplyr::filter(Sex != "U") |>
  dplyr::group_by(Age_years) |>
  dplyr::summarise(
    female = sum(Sex == "F"),
    male = sum(Sex == "M"),
    ratio = female / (female + male)
  )
ggplot(sex_ratio |> dplyr::filter(Age_years <= 50), aes(x = Age_years, y = ratio)) +
  geom_line() + ylim(c(0, 1)) +
  ylab("Ratio Female")


sex_ratio_length  <- data_survey_bio$nwfsc_combo |>
  dplyr::filter(Sex != "U", 
                !is.na(Length_cm), 
                Length_cm < 70, Length_cm >= 30) |>
  dplyr::group_by(Length_cm) |>
  dplyr::summarise(
    female = sum(Sex == "F"),
    male = sum(Sex == "M"),
    ratio = female / (female + male)
  )
ggplot(sex_ratio_length, aes(x = Length_cm, y = ratio)) +
  geom_line() + ylim(c(0, 1)) +
  ylab("Ratio Female")


#===============================================================================
# Plot the AFSC Slope lengths and lengths with ages
#===============================================================================

length_data <- data_survey_bio$afsc_slope$length_data |> dplyr::filter(Sex != "U")
age_data <- data_survey_bio$afsc_slope$age_data |> dplyr::filter(Sex != "U")

data <- dplyr::bind_rows(
  length_data[, c("Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Lengthed Fished"),
  age_data[, c("Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Aged Fished")
) |>
  dplyr::mutate(length_round = round(Length_cm, 0)) |>
  dplyr::group_by(length_round, Type) |>
  dplyr::mutate(
    Female = sum(Sex == "F"),
    Male = sum(Sex == "M"),
    Ratio = Female / (Female + Male)
  )

data |>
  dplyr::group_by(Type) |>
  dplyr::summarise(sex_ratio = sum(Sex == "F") / length(Sex))

ggplot(data, aes(Length_cm, color = Type)) +
  geom_density(size = 1) +
  theme_bw() + xlim(c(0, 90)) +
  xlab("Length (cm)") + ylab("Density") +
  facet_grid("Sex")

ggplot(data, aes(x = length_round, y = Ratio, color = Type, linetype = Type)) +
  geom_line(linewidth = 1)  +
  theme_bw() + xlim(c(30, 90)) +
  xlab("Length (cm)") + ylab("Density") 
table(data$Sex, data$Type)

data_survey_catch$afsc_slope |>
  dplyr::mutate(positive = dplyr::case_when(total_catch_numbers == 0 ~ 0, .default = 1)) |>
  dplyr::summarize(percent_positive = sum(positive) / length(Tow)) 

data_survey_catch$nwfsc_slope |>
  dplyr::mutate(positive = dplyr::case_when(total_catch_numbers == 0 ~ 0, .default = 1)) |>
  dplyr::summarize(percent_positive = sum(positive) / length(Tow)) 

ggplot(data_survey_bio$nwfsc_slope, aes(x = Age_years, color = Sex)) +
  geom_density() + 
  geom_vline(xintercept = 4)

#===============================================================================
# Triennial 
#===============================================================================
triennial_len <- dplyr::bind_rows(
  data_survey_bio$triennial_early$length_data,
  data_survey_bio$triennial_late$length_data
) 

triennial_age <- dplyr::bind_rows(
  data_survey_bio$triennial_early$age_data,
  data_survey_bio$triennial_late$age_data
) 

data <- dplyr::bind_rows(
  triennial_len[, c("Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Lengthed Fished"),
  triennial_age [, c("Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Aged Fished")
) |>
  dplyr::mutate(length_round = round(Length_cm, 0)) |>
  dplyr::group_by(length_round, Type) |>
  dplyr::mutate(
    Female = sum(Sex == "F"),
    Male = sum(Sex == "M"),
    Ratio = Female / (Female + Male)
  )
data |>
  dplyr::group_by(Type) |>
  dplyr::summarise(sex_ratio = sum(Sex == "F") / length(Sex))

ggplot(data, aes(Length_cm, color = Type)) +
  geom_density(size = 1) +
  theme_bw() + xlim(c(0, 90)) +
  xlab("Length (cm)") + ylab("Density") +
  facet_grid("Sex")

ggplot(data, aes(x = length_round, y = Ratio, color = Type, linetype = Type)) +
  geom_line(linewidth = 1)  +
  theme_bw() + xlim(c(30, 90)) +
  xlab("Length (cm)") + ylab("Density") 


data_year <- dplyr::bind_rows(
  triennial_len[, c("Year", "Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Lengthed Fished"),
  triennial_age [, c("Year", "Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Aged Fished")
  ) |>
  dplyr::filter(Sex != "U") |>
  dplyr::mutate(length_round = round(Length_cm, 0)) |>
  dplyr::group_by(Year, length_round, Type) |>
  dplyr::summarize(
    Female = sum(Sex == "F"),
    Male = sum(Sex == "M"),
    Ratio = Female / (Female + Male)
  )
ggplot(data_year, aes(x = length_round, y = Ratio, color = Type, linetype = Type)) +
  geom_line(linewidth = 1)  +
  theme_bw() + ylim(c(0,1)) + 
  xlab("Length (cm)") + ylab("Density") + 
  facet_grid("Year")

data_year <- dplyr::bind_rows(
  triennial_len[, c("Year", "Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Lengthed Fished"),
  triennial_age[, c("Year", "Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Aged Fished")
) |>
  dplyr::filter(Sex != "U", Year != 1980) |>
  dplyr::mutate(length_round = round(Length_cm, 0)) |>
  dplyr::group_by(Year, length_round, Type) |>
  dplyr::mutate(
    Female = sum(Sex == "F"),
    Male = sum(Sex == "M"),
    Ratio = Female / (Female + Male)
  )

ggplot(data_year, aes(Length_cm, color = Type)) +
  geom_density(size = 1) +
  theme_bw() + xlim(c(0, 90)) +
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = "Year", nrow = 3)

tows <- triennial_age[, c("Trawl_id", "Year", "Sex", "Length_cm")] |> 
  dplyr::filter(!is.na(Length_cm)) |> 
  dplyr::group_by(Year) |>
  dplyr::summarise(n = length(unique(Trawl_id)))


data_year <- dplyr::bind_rows(
  triennial_len[, c("Year", "Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Lengthed Fished"),
  triennial_age [, c("Year", "Sex", "Length_cm")] |> dplyr::filter(!is.na(Length_cm)) |> dplyr::mutate(Type = "Aged Fished")
) |>
  dplyr::filter(Sex != "U") 


triennial_test <- list()
for (y in sort(unique(data_year$Year))[-1]) {
  temp <- wilcox.test(Length_cm ~ Type, data = data_year[which(data_year$Year == y), ])
  triennial_test <- rbind(triennial_test, c(y, round(temp$p.value, 3)))
}


#===============================================================================
# WCGBT
#===============================================================================

data <- data_survey_bio$nwfsc_combo[, c("Sex", "Length_cm", "Age_years")] |>
  dplyr::mutate(
    length_round = round(Length_cm, 0),
    Type = dplyr::case_when(!is.na(Age_years) ~ "Aged Fish", .default = "Lengthed Fish")
  ) |>
  dplyr::group_by(length_round, Type) |>
  dplyr::mutate(
    Female = sum(Sex == "F"),
    Male = sum(Sex == "M"),
    Ratio = Female / (Female + Male)
  )
data |>
  dplyr::group_by(Type) |>
  dplyr::summarise(sex_ratio = sum(Sex == "F") / length(Sex))

ggplot(data, aes(Length_cm, color = Type)) +
  geom_density(size = 1) +
  theme_bw() + xlim(c(0, 90)) +
  xlab("Length (cm)") + ylab("Density") +
  facet_grid("Sex")

ggplot(data, aes(x = length_round, y = Ratio, color = Type, linetype = Type)) +
  geom_line(linewidth = 1)  +
  theme_bw() + xlim(c(30, 90)) +
  xlab("Length (cm)") + ylab("Density") 


data_year <- data_survey_bio$nwfsc_combo[, c("Year", "Sex", "Length_cm", "Age_years")] |>
  dplyr::mutate(
    length_round = round(Length_cm, 0),
    Type = dplyr::case_when(!is.na(Age_years) ~ "Aged Fish", .default = "Lengthed Fish")
  ) |>
  #dplyr::filter(Sex != "U") |>
  dplyr::group_by(Year, length_round, Type) |>
  dplyr::mutate(
    Female = sum(Sex == "F"),
    Male = sum(Sex == "M"),
    Ratio = Female / (Female + Male)
  )

ggplot(data_year, aes(Length_cm, color = Type)) +
  geom_density(size = 1) +
  theme_bw() + xlim(c(0, 90)) +
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = "Year", nrow = 3)

ggplot(data_year, aes(Length_cm, color = Type)) +
  geom_density(size = 1) +
  theme_bw() + xlim(c(0, 90)) +
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = "Year", nrow = 3)

data_year <- data_survey_bio$nwfsc_combo[, c("Year", "Sex", "Length_cm", "Age_years")] |>
  dplyr::mutate(
    length_round = round(Length_cm, 0),
    Type = dplyr::case_when(!is.na(Age_years) ~ "Aged Fish", .default = "Lengthed Fish")
  ) |>
  dplyr::filter(Sex != "U")

wcgbt_test <- list()
for (y in sort(unique(data_year$Year))) {
  temp <- wilcox.test(Length_cm ~ Type, data = data_year[which(data_year$Year == y), ])
  wcgbt_test <- rbind(wcgbt_test, c(y, round(temp$p.value, 3)))
}
