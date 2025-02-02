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
  fs::dir_ls(here::here("data-raw", "landings"), regex = "PacFIN\\..+Comp")[1] |>
  purrr::map_df(
    .f = function(x) {load(x); return(catch.pacfin)}
  ) |>
  tibble::tibble()

pacfin_catch <- raw_pacfin_catch |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::left_join(
    y = port_lats
  ) |>
  # Filter out 1981-86 for Oregon since we use the reconstruction
  # and WDFW records for those years, particularly 1981-82.
  dplyr::filter(
    !(landing_year < 1987 & agency_code == "O")
  ) |>
  dplyr::mutate(
    catch_mt = round_weight_mtons,
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
    catch_mt = sum(catch_mt)
  ) 

# Pull in the GEMM since the at-sea catches includes both discards and landed fish
gemm <- nwfscSurvey::pull_gemm(
  common_name = "sablefish") |>
  dplyr::filter(sector %in% c("At-Sea Hake CP", "At-Sea Hake MSCV")) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    landing_rate = sum(total_landings_mt) / sum(total_discard_and_landings_mt)
  )

# At-Sea Catches
at_sea_catch <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sabefish Catch_summarized_1978-2023_102224.xlsx"),
  sheet = "Sablefish Catch 1979-2023") |>
  dplyr::rename(
    year = YEAR
  ) |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    state = "at-sea",
    area = "north",
    gear_group = "trawl",
    discard_and_landings_mt = 0.001 * sum(EXPANDED_SumOfEXTRAPOLATED_WEIGHT_KG)
  ) |>
  dplyr::left_join(gemm, by = c("year")) |>
  dplyr::mutate(
    landing_rate = dplyr::case_when(is.na(landing_rate) ~ 1, .default = landing_rate),
    catch_mt = landing_rate * discard_and_landings_mt
  ) |>
  dplyr::select(-landing_rate, -discard_and_landings_mt)

# Oregon Historical
oregon_pre_historical <- readxl::read_excel(
  path = here::here("data-raw", "landings", "oregon_historical_landings_1892_1914.xlsx"),
  sheet = "Sheet1",
  skip = 3) |>
  dplyr::filter(all_gears_mt > 0) |>
  dplyr::rename(
    catch_mt = all_gears_mt) |>
  dplyr::mutate(
    state = "OR",
    area = "north",
    gear_group = "hkl"
  )
or_gear_code <- read.csv(here::here("data-raw", "landings", "ODFW_Gear_Codes_PacFIN.csv")) |>
  dplyr::mutate(
    CODE = as.character(CODE),
    STD_GEAR = as.character(STD_GEAR)
  )
oregon_historical <- read.csv(here::here("data-raw", "landings", "Oregon Commercial landings_477_2023.csv")) |>
  dplyr::select(-TOTAL, -FLEET) |>
  tidyr::pivot_longer(
    cols = starts_with("X"),
    names_to = "CODE",
    values_to = "catch_mt"
  ) |>
  dplyr::mutate(
    year = YEAR,
    state = "OR",
    area = "north",
    CODE = gsub("X", "", CODE)
  ) |>
  dplyr::filter(year < 1987) |>
  dplyr::left_join(
    y = or_gear_code[, c("CODE", "STD_GEAR")]
  ) |>
  dplyr::mutate(
    gear_group = dplyr::case_when(
      STD_GEAR %in% c("LONGLINE",
                      "HANDTOOL",
                      "HOOK&LINE",
                      "SHELL_H&L",
                      "TROLL") ~ "hkl",
      STD_GEAR %in% c("FISHPOT", "CRABPOT", "SHRIMP_POT") ~ "pot",
      TRUE ~ "trawl"
    )
  ) |>
  dplyr::group_by(year, state, area, gear_group) |>
  dplyr::summarise(
    catch_mt = sum(catch_mt)
  ) |>
  dplyr::full_join(y = oregon_pre_historical, by = c("year", "state", "area", "gear_group", "catch_mt"))

# California Historical
# between 1931- 1940 area north = 0.83, south = 0.17
california_1900 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980.xlsx"),
  sheet = "1900-1930 Ian Recon.",
  skip = 5) |>
  dplyr::rename(
    year = Year,
    gear_group = Gear
  ) |>
  dplyr::filter(year > 1900) |>
  dplyr::mutate(
    gear_group = dplyr::case_match(gear_group, "HKL" ~ "hkl", "POT" ~ "pot", "TWL" ~ "trawl"),
    hkl_south = dplyr::case_when(gear_group == "hkl" ~ 0.17 * catch_mt, TRUE ~ 0),
    pot_south = dplyr::case_when(gear_group == "pot" ~ 0.17 * catch_mt, TRUE ~ 0),
    trawl_south = dplyr::case_when(gear_group == "trawl" ~ 0.17 * catch_mt, TRUE ~ 0),
    hkl_north = dplyr::case_when(gear_group == "hkl" ~ 0.83 * catch_mt, TRUE ~ 0),
    pot_north = dplyr::case_when(gear_group == "pot" ~ 0.83 * catch_mt, TRUE ~ 0),
    trawl_north = dplyr::case_when(gear_group == "trawl" ~ 0.83 * catch_mt, TRUE ~ 0),
  ) |>
  dplyr::select(year, trawl_south, hkl_south, pot_south, trawl_north, hkl_north, pot_south) |>
  tidyr::pivot_longer(
    values_to = "catch_mt",
    cols = c(trawl_south, hkl_south, pot_south, trawl_north, hkl_north, pot_south),
    names_to = "area_group"
  ) |>
  dplyr::mutate(
    state = "CA",
    area = case_match(area_group, c("trawl_south", "hkl_south", "pot_south") ~ "south",
                      c("trawl_north", "hkl_north", "pot_north") ~ "north"),
    gear_group = case_match(area_group, c("trawl_south", "trawl_north") ~ "trawl",
                            c("hkl_south", "hkl_north") ~ "hkl",
                            c("pot_south", "pot_north") ~ "pot")
  ) |>
  dplyr::select(-area_group) |>
  dplyr::relocate(catch_mt, .after = dplyr::last_col())
# This is all landings trawl + fixed gears 1930-1968
# pot gear does not start until 1969
# the percent of trawl between 1969:1971 is 68% - use this to allocate historical
california_1931 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980.xlsx"),
  sheet = "RALSTON ET AL. 2010 COMM.",
  skip = 2) |>
  dplyr::select(-species, -gear) |>
  dplyr::mutate(
    state = "CA",
    area = dplyr::case_when(region_caught %in% 6:8 ~ "south", TRUE ~ "north"),
    trawl = 0.68 * 0.000453592 * Total,
    pot = 0,
    hkl = 0.32 * 0.000453592 * Total
  ) |>
  dplyr::select(-Total, -region_caught) |>
  tidyr::pivot_longer(
    values_to = "catch_mt",
    cols = c(trawl, hkl, pot),
    names_to = "gear_group"
  )
california_1969 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980.xlsx"),
  sheet = "CA COMM. 1969-1977") |>
  dplyr::select(-Species, -Market_category, -Species_group) |>
  dplyr::rename(
    year = Year
  ) |>
  # There is 0.13 mt of catch by unknown gear caught in 1972 - put this in trawl 
  dplyr::mutate(
    state = "CA",
    area = dplyr::case_when(Port_complex %in% c("OSD", "OLA", "OSB", "MRO") ~ "south", TRUE ~ "north"),
    gear_group = dplyr::case_match(Gear, "FPT" ~ "pot", "HKL" ~ "hkl", c("TWL", "NET", "UNK") ~ "trawl"),
    catch_mt = 0.000453592 * `Total Pounds`
  ) |> 
  dplyr::group_by(year, state, area, gear_group) |>
  dplyr::summarise(
    catch_mt = sum(catch_mt)
  ) 
# This is all landings trawl + fixed gears 1948-1968 that were caught in OR/WA waters
# pot gear does not start until 1969
# the percent of trawl between 1969:1971 is 68% - use this to allocate historical
california_other_states <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980 2024-11-21.xlsx"),
  sheet = "RALSTON ET AL. OR+WA",
  skip = 4)  |>
  dplyr::mutate(
    state = "CA",
    area = "north",
    trawl = 0.68 * Oregon + 0.68 * Washington,
    pot = 0,
    hkl = 0.32 * Oregon + 0.32 * Washington
  ) |>
  dplyr::select(-Oregon, -Washington) |>
  dplyr::rename(year = Year) |>
  tidyr::pivot_longer(
    values_to = "catch_mt",
    cols = c(trawl, hkl, pot),
    names_to = "gear_group"
  )
california_1978 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980.xlsx"),
  sheet = "CALCOM 1978-1980",
  skip = 5) |>
  dplyr::select(-SPECIES, -MARK_CAT, -SPECIES_GRP) |>
  dplyr::rename(
    year = YEAR
  ) |>
  # Calculate the rate to allocate other + unknown in excel file of:
  # hkl = 0.06, pot = 0.54, trawl = 0.40
  dplyr::mutate(
    state = "CA",
    area = dplyr::case_when(PORT_COMPLEX %in% c("OSD", "OLA", "OSB", "MRO") ~ "south", TRUE ~ "north"),
    gear_group = dplyr::case_match(GEAR_GRP, "FPT" ~ "pot", "HKL" ~ "hkl", c("TWL", "NET") ~ "trawl", c("OTH", "UNK") ~ "other"),
    init_catch_mt = 0.000453592 * POUNDS,
    other_total = dplyr::case_when(gear_group == "other" ~ init_catch_mt, TRUE ~ 0),
    trawl_add = dplyr::case_when(gear_group == "other" ~ 0.40 * other_total, TRUE ~ 0),
    hkl_add = dplyr::case_when(gear_group == "other" ~ 0.06 * other_total, TRUE ~ 0),
    pot_add = dplyr::case_when(gear_group == "other" ~ 0.54 * other_total, TRUE ~ 0),
    trawl_reg = dplyr::case_when(gear_group == "trawl" ~ init_catch_mt + trawl_add, TRUE ~ 0),
    hkl_reg = dplyr::case_when(gear_group == "hkl" ~ init_catch_mt + hkl_add, TRUE ~ 0),
    pot_reg = dplyr::case_when(gear_group == "pot" ~ init_catch_mt + pot_add, TRUE ~ 0)
  ) |>
  dplyr::group_by(year, state, area) |>
  dplyr::summarise(
    trawl = sum(trawl_reg + trawl_add),
    hkl = sum(hkl_reg + hkl_add),
    pot = sum(pot_reg + pot_add)
  ) |>
  tidyr::pivot_longer(
    values_to = "catch_mt",
    cols = c(trawl, hkl, pot),
    names_to = "gear_group"
  )
california_historical <- dplyr::bind_rows(
  california_1900, california_1931, california_1969, california_1978, california_other_states
)

# Washington Historical
# To-Do fill in early years with missing catches
# WDFW sent replacement catches for 1970-1980 that should be used over the 
# landings that are in this spreadsheet
washington_historical <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Washington_SablefishCatch_03042019.xlsx"),
  sheet = "1890-2018 filled") |>
  dplyr::rename(year = Year) |>
  dplyr::filter(year < 1970) |>
  tidyr::pivot_longer(!year, names_to = "gear_group", values_to = "catch_mt") |>
  dplyr::mutate(
    state = "WA",
    area = "north",
    gear_group = dplyr::case_match(gear_group, "HKL_mt" ~ "hkl", "Trawl_mt" ~ "trawl", "Pot_mt" ~ "pot")
  ) |>
  tidyr::replace_na(list(catch_mt = 0))

# WDFW fish tickets for 1970-1980
washington_1970 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish_Commercial_Final_WaFT.xlsx"),
  sheet = "Sablefish_P-Council") |>
  dplyr::rename(year = BatchYear) |>
  dplyr::filter(year < 1981) |>
  dplyr::mutate(
    state = "WA",
    area = "north",
    gear_group = dplyr::case_match(GearGrouop, "HKL" ~ "hkl", "TWL" ~ "trawl", "POT" ~ "pot")
  ) |>
  dplyr::group_by(year, state, area, gear_group) |>
  dplyr::summarise(
    catch_mt = sum(RoundPound) * 0.000453592
  )

# Foreign
foreign <- readxl::read_excel(
  path = here::here("data-raw", "landings", "foreign_catch_lynde_1986.xlsx"),
  sheet = "foreign catches") |>
  dplyr::filter(
    INP %in% c(67, 71:74), SPCD == "SABL"
  ) |>
  dplyr::rename(
    year = YEAR,
    region = INP,
    country = AG,
    init_catch = `CATCH(MT)`
  ) |>
  dplyr::mutate(
    area = dplyr::case_when(region == 74 ~ "south", .default = "north"),
    year = as.numeric(paste0(19, year))
  ) |> 
  dplyr::group_by(year, country, area) |>
  dplyr::summarise(
    total_catch = sum(init_catch),
  ) |>
  dplyr::mutate(
    state = "foreign",
    hkl_sub = dplyr::case_when(country == 6 ~ 0.77 * total_catch, TRUE ~ 0), 
    pot_sub = dplyr::case_when(country == 8 ~ total_catch, TRUE ~ 0),
    trawl_japan = dplyr::case_when(country == 6 ~ 0.23 * total_catch, TRUE ~ 0),
    trawl_ussr = dplyr::case_when(country == 7 ~ total_catch, TRUE ~ 0),
    other = dplyr::case_when(country %in% c(9, 21) ~ sum(total_catch), TRUE ~ 0)
  ) |>
  dplyr::group_by(year, state, area) |>
  dplyr::summarise(
    trawl = sum(trawl_japan + trawl_ussr +  other),
    hkl = sum(hkl_sub),
    pot = sum(pot_sub)
  ) |>
  tidyr::pivot_longer(
    values_to = "catch_mt",
    cols = c(trawl, hkl, pot),
    names_to = "gear_group"
  )

rec <-  readxl::read_excel(
  path = here::here("data-raw", "landings", "CTE501-WASHINGTON-OREGON-CALIFORNIA-1990---2024.xlsx"),
  sheet = "Detail",
  skip = 21) |>
  dplyr::rename(year = `RecFIN Year`) |>
  dplyr::mutate(
    state = dplyr::case_when(`State Name` == "OREGON" ~ "OR", 
                             `State Name` == "WASHINGTON" ~ "WA",
                             .default = "CA"),
    area = "rec",
    gear_group = "hkl"
  ) |>
  dplyr::group_by(year, state, area, gear_group) |>
  dplyr::summarise(
    catch_mt = sum(`Total Mortality MT`)
  )

#===============================================================================
# Combine everything together
#===============================================================================
all_catches <- dplyr::bind_rows(
  pacfin_catch,
  at_sea_catch,
  foreign, 
  rec,
  washington_historical,
  washington_1970,
  oregon_historical,
  california_historical) 

data_commercial_catch_cw <- all_catches |>
  dplyr::group_by(year, gear_group) |>
  dplyr::summarise(
    catch_mt = round(sum(catch_mt), digits = 4),
  ) |>
  dplyr::rename(fleet = gear_group) |>
  dplyr::mutate(
    seas = 1,
    fleet = recode_fleet_cw(fleet),
    .after = year
  ) |>
  dplyr::mutate(catch_se = 0.01, .after = catch_mt) |>
  dplyr::arrange(fleet)

#data_commercial_catch_area <- all_catches |>
#  dplyr::mutate(area_gear = paste0(area, "-", gear_group)) |>
#  dplyr::group_by(year, area_gear) |>
#  dplyr::summarise(
#    seas = 1,
#    catch_mt = round(sum(catch_mt), digits = 4),
#  ) |>
#  dplyr::mutate(
#    fleet = recode_fleet_area(area_gear),
#    .after = year
#  ) |>
#  dplyr::select(-area_gear) |>
#  dplyr::mutate(catch_se = 0.01, .after = catch_mt) |>
#  dplyr::relocate(fleet, .after = seas) |>
#  dplyr::arrange(fleet) 

data_commercial_catch <- all_catches |>
  dplyr::group_by(year, state, area, gear_group) |>
  dplyr::summarise(
    catch_mt = round(sum(catch_mt), digits = 4),
  ) |>
  dplyr::ungroup()

data_commercial_catch_expansions <- data_commercial_catch |>
  dplyr::filter(state %in% c("WA", "OR", "CA"), area != "rec") |>
  dplyr::rename(geargroup = gear_group) |>
  dplyr::mutate(
    geargroup = dplyr::case_when(
      geargroup == "hkl" ~ "HKL", 
      geargroup == "pot" ~ "POT",
      .default = "TWL"
    )
  ) |>
  dplyr::group_by(year, state, geargroup) |>
  dplyr::summarise(
    catch_mt = sum(catch_mt)
  ) |>
  dplyr::ungroup() 

write_named_csvs(
  data_commercial_catch_cw,
  #data_commercial_catch_area,
  data_commercial_catch_expansions,
  dir = "data-processed"
)

usethis::use_data(
  data_commercial_catch,
  overwrite = TRUE
)


