library(dplyr)
library(ggplot2)

#===============================================================================
# Plot the weighted discard rates
#===============================================================================

discard_rates <- read.csv(here::here(
  file.path("data-processed", "data_commercial_discard_rates.csv"))) |>
  dplyr::mutate(
    Fleet = dplyr::case_match(fleet, 2 ~ "HKL",
                              3 ~ "Pot",
                              1 ~ "Trawl"),
    lwr = qnorm(0.025, discard_rate, sd),
    upr = qnorm(0.975, discard_rate, sd)
  ) |>
  dplyr::mutate(
    lwr = dplyr::case_when(lwr < 0 ~ 0, .default = lwr),
    year_offset = dplyr::case_when(fleet == 1 ~ year, fleet == 2 ~ year + 0.25, .default = year + 0.5)
  ) 

aggregate(discard_rate~fleet, discard_rates[discard_rates$year >= 2011, ], mean)
# fleet discard_rate
#   HKL    0.1865833
#   Pot    0.2140833
# Trawl    0.0439500

ggplot(discard_rates |> dplyr::filter(year > 2000), aes(x = year, y = discard_rate)) +
  geom_point() +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lwr, ymax = upr)) +
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Rates") + xlab("Year") +
  facet_grid("Fleet")
ggsave(file = here::here("data-raw", "discard", "wcgop", "figures", 
                         "wcgop_discard_rates_weighted_coastwide.png"),
       height = 7, width = 7)

ggplot(discard_rates |> dplyr::filter(year > 2000), 
       aes(x = year_offset, y = discard_rate, color = Fleet)) +
  geom_point() +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lwr, ymax = upr)) +
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Rates") + xlab("Year") 
ggsave(file = here::here("data-raw", "discard", "wcgop", "figures", 
                         "wcgop_discard_rates_weighted_coastwide_one_panel.png"),
       height = 7, width = 7)

#===============================================================================
# Look at the coastwide data
#===============================================================================
discard_rates_cs <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "coastwide", "discard_rates_combined_catch_share.csv"))) |>
  dplyr::select(year, fleet, n_obs, n_hauls, n_trips, n_vessels, discard_rate) |>
  dplyr::mutate(sd = 0.01, Source = "Catch Share")
discard_rates_ncs <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "coastwide", "discard_rates_noncatch_share.csv"))) |>
  dplyr::select(year, fleet, n_obs, n_hauls, n_trips, n_vessels, median_ratio, sd_ratio) |>
  dplyr::rename(discard_rate = median_ratio, sd = sd_ratio) |>
  dplyr::mutate(Source = "Non-catch Share") 

discard_rates <- dplyr::bind_rows(discard_rates_cs, discard_rates_ncs) |>
  dplyr::rename(Fleet = fleet) |>
  dplyr::mutate(
    Fleet = dplyr::case_match(Fleet, "hook-and-line-coastwide" ~ "HKL",
                              "pot-coastwide" ~ "Pot",
                              "trawl-coastwide" ~ "Trawl")
  )

body_weights <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "coastwide", "discard_mean_body_weights.csv"))) |>
  dplyr::rename(Fleet = fleet) |>
  dplyr::mutate(
    Fleet = dplyr::case_match(Fleet, "hook-and-line-coastwide" ~ "HKL",
                              "pot-coastwide" ~ "Pot",
                              "trawl-coastwide" ~ "Trawl")
  )

discard_lengths <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "coastwide", "biological_discard_lengths.csv"))) |>
  dplyr::rename(Fleet = fleet) |>
  dplyr::filter(sex == 0) |>
  dplyr::select(!ends_with(".1"))
colnames(discard_lengths)[7:length(discard_lengths)] <- gsub("X", "U", colnames(discard_lengths)[7:length(discard_lengths)])

ggplot(discard_rates, aes(x = year, y = discard_rate, col = Fleet)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Rate") + xlab("Year") +
  facet_grid(Source~.)
ggsave(file = here::here("data-raw", "discard", "wcgop", "figures", "discard_rates_catch_share_coastwide.png"),
       height = 7, width = 7)

ggplot(body_weights, aes(x = year, y = obs, col = Fleet)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Body Weight (kg)") + xlab("Year") 
ggsave(file = here::here("data-raw", "discard", "wcgop", "figures", "discard_body_weight_coastwide.png"),
       height = 7, width = 7)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "trawl-coastwide" & discard_lengths$sex == 0, ],
  add_save_name = "Trawl_coastwide"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "hook-and-line-coastwide" & discard_lengths$sex == 0, ],
  add_save_name = "HKL_coastwide"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "pot-coastwide" & discard_lengths$sex == 0, ],
  add_save_name = "Pot_coastwide"
)

#===============================================================================
# Compare model and GEMM rates
#===============================================================================
discard_rates_2025 <- read.csv(here::here(
  file.path("data-processed", "data_commercial_discard_rates.csv"))) |>
  dplyr::mutate(
    fleet = dplyr::case_match(fleet, 2 ~ "HKL",
                              3 ~ "Pot",
                              1 ~ "Trawl"),
    lwr = qnorm(0.025, discard_rate, sd),
    upr = qnorm(0.975, discard_rate, sd),
    assessment = as.factor(2025)
  ) |>
  dplyr::mutate(
    lwr = dplyr::case_when(lwr < 0 ~ 0, .default = lwr),
    year_offset = year
  )

data <- r4ss::SS_readdat(
  "C:/Assessments/2025/sablefish_2025/model/_bridging/0_2023_model/data.ss"
)
discard_rates_2023 <- data$discard_data |>
  dplyr::mutate(
    fleet = dplyr::case_match(fleet, 2 ~ "Trawl", 1 ~ "HKL"),
    lwr = qnorm(0.025, obs, stderr),
    upr = qnorm(0.975, obs, stderr),
    assessment = as.factor(2023)
  ) |>
  dplyr::mutate(
    lwr = dplyr::case_when(lwr < 0 ~ 0, .default = lwr),
    year_offset = year + 0.5
  ) |>
  dplyr::rename(
    discard_rate = obs,
    sd = stderr
  )

discard_rates_2023 <- dplyr::bind_rows(
  discard_rates_2023, 
  discard_rates_2023 |>
    dplyr::filter(fleet == "HKL") |>
    dplyr::mutate(
      fleet = "Pot"
    )
)

discard_rates <- dplyr::bind_rows(
  discard_rates_2023,
  discard_rates_2025
)

ggplot(discard_rates |> dplyr::filter(year > 2000), aes(x = year_offset, y = discard_rate, color = assessment)) +
  geom_point() +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lwr, ymax = upr)) +
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Rates") + xlab("Year") +
  facet_grid("fleet")

gemm_data <- nwfscSurvey::pull_gemm(common_name = "sablefish") |>
  dplyr::filter(!sector %in%  c("Research", 
                                "Washington Recreational",
                                "Oregon Recreational",
                                "California Recreational")) |>
  dplyr::mutate(
    catch_shares = "non-catch-shares",
    fleet = NA
  )
catch_shares <- c(
  "At-Sea Hake CP",
  "At-Sea Hake MSCV",
  "CS - Bottom and Midwater Trawl",
  "CS - Bottom Trawl",
  "CS - Hook & Line",
  "CS - Pot",
  "CS EM - Bottom Trawl",
  "CS EM - Pot",
  "Midwater Hake",
  "Midwater Hake EM",
  "Midwater Rockfish",
  "Midwater Rockfish EM"
)
gemm_data[which(gemm_data[, "sector"] %in% catch_shares & gemm_data[, "year"] >= 2011), "catch_shares"] <- "catch-shares"
pot <- c(
  "CS - Pot",
  "CS EM - Pot",
  "LE Sablefish - Pot",
  "LE Fixed Gear DTL - Pot",
  "OA Fixed Gear - Pot"
)
hkl <- c(
  "Combined LE & OA CA Halibut",
  "CS - Hook & Line",
  "Directed P Halibut",
  "Incidental",
  "LE CA Halibut",
  "LE Fixed Gear DTL - Hook & Line",
  "LE Sablefish - Hook & Line",
  "Nearshore",
  "OA CA Halibut",
  "OA Fixed Gear - Hook & Line"
)
trawl <- c(
  "At-Sea Hake CP",
  "At-Sea Hake MSCV",
  "Midwater Hake",
  "Midwater Hake EM",
  "Shoreside Hake",
  "Tribal At-Sea Hake",
  "CS - Bottom and Midwater Trawl",
  "CS - Bottom Trawl",
  "CS EM - Bottom Trawl",
  "Limited Entry Trawl",
  "Midwater Rockfish",
  "Midwater Rockfish EM",
  "Pink Shrimp",
  "Research",
  "Tribal Shoreside"
)
gemm_data[which(gemm_data[, "sector"] %in% hkl), "fleet"] <- "HKL"
gemm_data[which(gemm_data[, "sector"] %in% trawl), "fleet"] <- "Trawl"
gemm_data[which(gemm_data[, "sector"] %in% pot), "fleet"] <- "Pot"
gemm_totals <- gemm_data |>
  dplyr::group_by(year, fleet) |>
  dplyr::summarize(
    discard_mt = round(sum(total_discard_mt),1),
    landed_mt = round(sum(total_landings_mt), 1),
    catch = round(sum(total_discard_with_mort_rates_applied_and_landings_mt), 1),
    gemm_discard_rate = discard_mt / (landed_mt + discard_mt)
  ) 
ggplot() +
  geom_point(data = gemm_totals, aes(x = year, y = gemm_discard_rate, color = "GEMM", shape = "GEMM"), size = 3) +
  geom_point(data = discard_rates_2025 |> dplyr::filter(year > 2000), aes(x = year, y = discard_rate, color = "Assessment", shape = "Assessment"), size = 3) +
  theme_bw() +
  scale_color_viridis_d(begin = 0, end = 0.5) +
  ylab("Discard Rates") + xlab("Year") +
  facet_grid("fleet")



#===============================================================================
# Look at the data by area north and south
#===============================================================================
discard_rates_cs <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "area", "discard_rates_combined_catch_share.csv"))) |>
  dplyr::select(year, fleet, n_obs, n_hauls, n_trips, n_vessels, discard_rate) |>
  dplyr::mutate(sd = 0.01, Source = "Catch Share")
discard_rates_ncs <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "area", "discard_rates_noncatch_share.csv"))) |>
  dplyr::select(year, fleet, n_obs, n_hauls, n_trips, n_vessels, median_ratio, sd_ratio) |>
  dplyr::rename(discard_rate = median_ratio, sd = sd_ratio) |>
  dplyr::mutate(Source = "Non-catch Share") 

discard_rates <- dplyr::bind_rows(discard_rates_cs, discard_rates_ncs) |>
  dplyr::rename(Fleet = fleet) 


body_weights <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "area", "discard_mean_body_weights.csv"))) |>
  dplyr::rename(Fleet = fleet) 


discard_lengths <- read.csv(here::here(
  file.path("data-raw", "discard", "wcgop", "area", "biological_discard_lengths.csv"))) |>
  dplyr::rename(Fleet = fleet) |>
  dplyr::filter(sex == 0)  |>
  dplyr::select(!ends_with(".1"))
colnames(discard_lengths)[7:length(discard_lengths)] <- gsub("X", "U", colnames(discard_lengths)[7:length(discard_lengths)])

ggplot(discard_rates, aes(x = year, y = discard_rate, col = Fleet)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Rate") + xlab("Year") +
  facet_grid(Source~.)
ggsave(file = here::here("data-raw", "discard", "wcgop", "figures", "discard_rates_catch_share_area.png"),
       height = 7, width = 7)

ggplot(body_weights, aes(x = year, y = obs, col = Fleet)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  scale_color_viridis_d() +
  ylab("Discard Body Weight (kg)") + xlab("Year") 
ggsave(file = here::here("data-raw", "discard", "wcgop", "figures", "discard_body_weight_area.png"),
       height = 7, width = 7)


nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "trawl-north" & discard_lengths$sex == 0, ],
  add_save_name = "Trawl_north"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "trawl-south" & discard_lengths$sex == 0, ],
  add_save_name = "Trawl_south"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "hook-and-line-north" & discard_lengths$sex == 0, ],
  add_save_name = "HKL_north"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "hook-and-line-south" & discard_lengths$sex == 0, ],
  add_save_name = "HKL_south"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "pot-south" & discard_lengths$sex == 0, ],
  add_save_name = "Pot_south"
)

nwfscSurvey::plot_comps(
  dir = here::here("data-raw", "discard", "wcgop", "figures"),
  data = discard_lengths[discard_lengths$Fleet == "pot-north" & discard_lengths$sex == 0, ],
  add_save_name = "Pot_north"
)


