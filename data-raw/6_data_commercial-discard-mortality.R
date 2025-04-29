# 1. Start with the all_mortality_rda.rda that contains catch and landings data
# from available data sources: PacFIN, historical state reconstructions,
# foreign, at-sea, and RecFIN.
# 2. Estimate discard mortality for PacFIN and RecFIN data only using discard
# rate data with mortality assumptions (trawl 50%, pot/hkl 20%).  Note that I 
# am not using the GEMM estimated dead discards since they only apply the DMR 
# rates in 2019+ and to only catch-share vessels. Previous assessments have 
# applied the DMR rates for all landings and years, and I think that is generally
# reasonable. 

landings_by_year <- all_mortality_data |>
  # only PacFIN and RecFIN data had a state assigned
  dplyr::group_by(year, gear_group) |>
  dplyr::summarise(
    landings_mt = sum(landings_mt),
    catch_mt = sum(catch_mt)
  ) |>
  dplyr::mutate(
    fleet = recode_fleet_cw(gear_group)
  )

# Assumptions:
# 1. No discarding during WWII 1942-1946
# 2. Trawl discard rates pre-2002 are based upon the average of Pikitch rates
# 3. HKL and Pot discard rates pre-2002 are based upon the average of rates 
# between 2002-2010.
# 4. 2024 Discard rates are equal to the 2023 rates
discard_rates <- read.csv(here::here("data-processed", "data_commercial_discard_rates.csv"))
# Change the fleet numbers to the discard rates to match the landings for joining later
discard_rates[, "fleet"] <- discard_rates[, "fleet"] - 3
ave_hkl_2002_2010 <- discard_rates |>
  dplyr::filter(fleet == 2, year < 2011) |>
  dplyr::summarise(ave_rate = mean(discard_rate))
# 0.1278111
ave_pot_2002_2010 <- discard_rates |>
  dplyr::filter(fleet == 3, year < 2011) |>
  dplyr::summarise(ave_rate = mean(discard_rate))
# 0.218133
ave_trawl_1890_2001 <- discard_rates |>
  dplyr::filter(fleet == 1, year < 2002) |>
  dplyr::summarise(ave_rate = mean(discard_rate))
# 0.363333
# if 2002-2010 is included in the average calculation = 0.2416

rates_and_landings <- dplyr::left_join(landings_by_year, discard_rates) |>
  dplyr::select(-month, -sd) |>
  dplyr::mutate(
    discard_rate = dplyr::case_when(
      year %in% c(1890:1941, 1947:2001) & gear_group == "trawl" ~ 0.36333,
      year %in% c(1890:1941, 1947:2001) & gear_group == "hkl" ~ 0.1278111,
      year %in% c(1890:1941, 1947:2001) & gear_group == "pot" ~ 0.218133,
      year %in% 1942:1946 ~ 0,
      .default = discard_rate
    ),
    discard_mortality_rate = dplyr::case_when(
      gear_group == "trawl" ~ 0.50, .default = 0.20
    )
  ) |>
  dplyr::group_by(gear_group) |>
  tidyr::fill(discard_rate, .direction = "down") |>
  dplyr::group_by(year, gear_group) |>
  dplyr::mutate(
    dead_discard_mt = discard_mortality_rate * discard_rate * landings_mt,
    landings_fleet = sum(landings_mt + catch_mt),
    total_mortality_mt = sum(dead_discard_mt + landings_mt + catch_mt)
  )

data_commercial_discards <- rates_and_landings |>
  dplyr::group_by(year, gear_group) |>
  dplyr::summarise(
    catch_mt = round(sum(dead_discard_mt), digits = 4),
  ) |>
  dplyr::rename(fleet = gear_group) |>
  dplyr::mutate(
    seas = 1,
    fleet = recode_fleet_cw(fleet) + 3,
    .after = year
  ) |>
  dplyr::mutate(catch_se = 0.01, .after = catch_mt) |>
  dplyr::arrange(fleet)  |>
  dplyr::filter(catch_mt > 0)

data_commercial_catch <- dplyr::bind_rows(
  data_commercial_landings,
  data_commercial_discards
)

write_named_csvs(
  data_commercial_catch,
  dir = "data-processed"
)

usethis::use_data(
  data_commercial_discards,
  overwrite = TRUE
)

