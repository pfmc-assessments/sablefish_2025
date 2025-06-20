library(dplyr)
library(tidyr)
library(ggplot2)
library(PEPtools)
library(readxl)

# PacFIN Landings
port_lats <- pacfin_ports_withlatlong |>
  dplyr::rename(
    pacfin_port_code = pcid
  ) |>
  dplyr::select(
    c(-name, -agencydesc, -agid)
  ) |>
  dplyr::distinct(pacfin_port_code, .keep_all = TRUE) |>
  tibble::tibble()

raw_pacfin_catch <-
  fs::dir_ls(here::here("data-raw", "landings"), regex = "PacFIN\\..+Comp") |>
  purrr::map_df(
    .f = function(x) {load(x); return(catch.pacfin)}
  ) |>
  tibble::tibble()

pacfin <- raw_pacfin_catch |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::left_join(
    y = port_lats
  ) |>
  # Filter out 1981-86 for Oregon since we use the reconstruction
  # and WDFW records for those years, particularly 1981-82.
  dplyr::filter(
    !(landing_year < 1987 & agency_code == "O"), landing_year != 2025
  ) |>
  dplyr::mutate(
    landings_mt = round_weight_mtons,
    year = landing_year,
    gear = pacfin_group_gear_code,
    state = dplyr::case_match(agency_code, "W" ~ "WA", "O" ~ "OR", "C" ~ "CA"),
    area = dplyr::case_when(latitude <= 36 ~"south", .default = "north"),
    # Currently this is lumping DRG MSC NET TWS into trawl
    gear_group = dplyr::case_when(
      gear %in% c("TLS", "HKL") ~ "hkl",
      gear %in% c("POT") ~ "pot",
      TRUE ~ "trawl")
  )  |>
  dplyr::group_by(year, state, area, gear_group) |>
  dplyr::summarise(
    landings_mt = sum(landings_mt),
    vessel_n = length(unique(vessel_id))
  ) 

landings_region <- pacfin |>
  dplyr::group_by(year, area, gear_group) |>
  dplyr::summarise(
    landings_mt = round(sum(landings_mt), 0)
  )

library(ggplot2)

ggplot(landings_region, aes(x = year, y = landings_mt, fill = gear_group)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Landings (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d() +
  facet_wrap("area", ncol = 1)

percent_by_area <- landings_region |>
  dplyr::group_by(year) |>
  dplyr::mutate(total = sum(landings_mt)) |>
  dplyr::group_by(year, area) |>
  dplyr::summarise(
    prop = round(sum(landings_mt) / sum(unique(total)), 2)
  )
  

landings_state <- pacfin |>
  dplyr::group_by(year, state, gear_group) |>
  dplyr::summarise(
    landings_mt = round(sum(landings_mt), 0)
  )
percent_by_state <- landings_state |>
  dplyr::group_by(year) |>
  dplyr::mutate(total = sum(landings_mt)) |>
  dplyr::group_by(year, state) |>
  dplyr::summarise(
    prop = round(sum(landings_mt) / sum(unique(total)), 2)
  )

ggplot(landings_state, aes(x = year, y = landings_mt, fill = gear_group)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Landings (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d() +
  facet_wrap("state", ncol = 1)
