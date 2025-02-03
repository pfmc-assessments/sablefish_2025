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


