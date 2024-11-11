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

pacfin_catch <- raw_pacfin_catch |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::left_join(
    y = port_lats
  ) |>
  dplyr::filter(
    !(landing_year < 1987 & agency_code == "O")
  ) |>
  # Attribute 50% (per OH) of landings in CATCH_AREA_CODE 58 to Canada
  # This only occurs in 1980-1982 with total catch of 2,630 mt in
  # catch area code 58
  dplyr::mutate(
    catch_mt = dplyr::case_when(
      catch_area_code == 58 ~ round_weight_mtons * 0.5,
      TRUE ~ round_weight_mtons),
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

# At-Sea Catches
at_sea_catch <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sabefish Catch_summarized_1978-2023_102224.xlsx"),
  sheet = "Sablefish Catch 1979-2023") |>
  dplyr::rename(
    year = YEAR
  ) |>
  dplyr::filter(year > 1980) |>
  #dplyr::mutate(
  #  catch_share = dplyr::case_when(
  #    year %in% 1981:2010 ~ "Non-catch Share", TRUE ~ "Catch Share")
  #) |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    state = "at-sea",
    area = "north",
    gear_group = "trawl",
    #catch_share = unique(catch_share),
    catch_mt = 0.001 * sum(EXPANDED_SumOfEXTRAPOLATED_WEIGHT_KG)
  )

# Oregon Historical
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
  )

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
california_all_1931 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980.xlsx"),
  sheet = "RALSTON ET AL. 2010 COMM.",
  skip = 2) |>
  dplyr::select(-species, -gear) |>
  dplyr::mutate(
    state = "CA",
    gear_group = "all",
    area = dplyr::case_when(region_caught %in% 6:8 ~ "south", TRUE ~ "north"),
    trawl_catch_mt = 0,
    all_catch_mt = 0.000453592 * Total
  ) |>
  dplyr::select(-Total, -region_caught)
# This is only trawl landings 1925-1957, subset to >= 1931 
california_trawl_1931 <- readxl::read_excel(
  path = here::here("data-raw", "landings", "Sablefish CA commercial landings 1931-1980.xlsx"),
  sheet = "BLOCK_SUMMARY_trawl") |>
  dplyr::select(-species, -species_grp, -mark_cat, -block) |>
  dplyr::filter(year > 1930) |>
  dplyr::mutate(
    state = "CA",
    gear_group = "trawl",
    area = dplyr::case_when(region_caught %in% 6:8 ~ "south", TRUE ~ "north"),
    trawl_catch_mt = 0.000453592 * pounds,
    all_catch_mt = 0
  ) |>
  dplyr::select(-pounds, -region_caught)
# Bring together the unknown gear and trawl landings
california_1931 <- california_all_1931 |>
  dplyr::bind_rows(california_trawl_1931) |>
  dplyr::group_by(year, state, area) |>
  # 0.22 of the catch between 1931-1957 was trawl, apply this rate from 1958-1968
  # in 1968+ the split by gear was hkl = 0.10, pot = 0.34, trawl = 0.56
  # where between fixed gear this was hkl = 0.24 and pot = 0.76
  dplyr::summarise(
    trawl_sub = sum(trawl_catch_mt),
    fixed_gear_sub = sum(all_catch_mt) - sum(trawl_catch_mt),
    hkl = dplyr::case_when(trawl_sub > 0 ~ 0.24 * fixed_gear_sub, TRUE ~ 0.10 * fixed_gear_sub),
    pot = dplyr::case_when(trawl_sub > 0 ~ 0.76 * fixed_gear_sub, TRUE ~ 0.34 * fixed_gear_sub),
    trawl = dplyr::case_when(trawl_sub > 0 ~ trawl_sub, TRUE ~ 0.56 * fixed_gear_sub)
  ) |>
  dplyr::select(year, state, area, hkl, pot, trawl) |>
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
  california_1900, california_1931, california_1969, california_1978
)

# Washington Historical


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
  dplyr::group_by(year, area) |>
  dplyr::mutate(
    state = "foreign",
    hkl_sub = dplyr::case_when(country == 6 ~ 0.77 * total_catch, TRUE ~ 0), 
    pot_sub = dplyr::case_when(country == 10 ~ total_catch, TRUE ~ 0),
    trawl_japan = dplyr::case_when(country == 6 ~ 0.23 * total_catch, TRUE ~ 0),
    trawl_ussr = dplyr::case_when(country == 7 ~ total_catch, TRUE ~ 0),
    other = dplyr::case_when(country %in% c(9, 21) ~ sum(total_catch), TRUE ~ 0),
    trawl_fraction = (trawl_japan + trawl_ussr) / (hkl_sub + pot_sub + trawl_japan + trawl_ussr),
    pot_fraction =  pot_sub / (hkl_sub + pot_sub + trawl_japan + trawl_ussr),
    hkl_fraction = hkl_sub / (hkl_sub + pot_sub + trawl_japan + trawl_ussr),
  ) |>
  tidyr::complete(
    fill = list(hkl_fraction = 0, pot_fraction = 0, trawl_fraction = 0)
  ) |>
  dplyr::group_by(year, state, area) |>
  dplyr::summarise(
    trawl = sum(trawl_japan + trawl_ussr + trawl_fraction * other),
    hkl = sum(hkl_sub + hkl_fraction * other),
    pot = sum(pot_sub + pot_fraction * other)
  ) |>
  tidyr::pivot_longer(
    values_to = "catch_mt",
    cols = c(trawl, hkl, pot),
    names_to = "gear_group"
  )


# Combine everything together
data_commercial_catch_cw <- dplyr::bind_rows(
  pacfin_catch,
  at_sea_catch,
  foreign, 
  oregon_historical,
  california_historical) |>
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
  dplyr::arrange(fleet) |>
  utils::write.csv(
    file = fs::path("data-processed", "data_commercial_catch_cw.csv"),
    row.names = FALSE
  )

data_commercial_catch_area <- dplyr::bind_rows(
  pacfin_catch,
  at_sea_catch,
  foreign, 
  oregon_historical,
  california_historical) |>
  dplyr::mutate(area_gear = paste0(area, "-", gear_group)) |>
  dplyr::group_by(year, area_gear) |>
  dplyr::summarise(
    seas = 1,
    catch_mt = round(sum(catch_mt), digits = 4),
  ) |>
  dplyr::mutate(
    fleet = recode_fleet_area(area_gear),
    .after = year
  ) |>
  dplyr::select(-area_gear) |>
  dplyr::mutate(catch_se = 0.01, .after = catch_mt) |>
  dplyr::relocate(fleet, .after = seas) |>
  dplyr::arrange(fleet) |>
  utils::write.csv(
    file = fs::path("data-processed", "data_commercial_catch_area.csv"),
    row.names = FALSE
  )

data_commercial_catch_for_expansions <- dplyr::bind_rows(
  pacfin_catch,
  oregon_historical,
  california_historical) |>
  dplyr::group_by(year, state, gear_group) |>
  dplyr::summarise(
    catch_mt = round(sum(catch_mt), digits = 4),
  ) |>
  utils::write.csv(
    file = fs::path("data-processed", "data_commercial_catch_for_expansions.csv"),
    row.names = FALSE
  )

usethis::use_data(
  data_commercial_catch_cw,
  overwrite = TRUE
)

usethis::use_data(
  data_commercial_catch_area,
  overwrite = TRUE
)

usethis::use_data(
  data_commercial_catch_for_expansions,
  overwrite = TRUE
)
