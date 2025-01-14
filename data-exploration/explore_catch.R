library(dplyr)
library(tidyr)
library(ggplot2)
library(PEPtools)

load(here::here("data", "data_commercial_catch.rda"))

landings <- data_commercial_catch |>
  dplyr::mutate(
    gear_group = dplyr::case_when(
      gear_group %in% c("hkl") ~ "HKL",
      gear_group %in% c("pot") ~ "Pot",
      TRUE ~ "Trawl")
  ) |>
  dplyr::rename(Gear = gear_group)

#==================================================================
# Compare landings with last assessment
#==================================================================
landings_2023 <- read.csv(
  here::here("data-raw", "2023", "model_landings_2023.csv")) |>
  dplyr::mutate(
    assessment = as.factor(assessment),
    gear = dplyr::case_when(
      gear %in% c("fixed") ~ "Fixed",
      TRUE ~ "Trawl")
  ) |>
  dplyr::rename(Gear = gear) |>
  dplyr::filter(assessment == 2023) |>
  tibble::tibble()

compare_landings <- landings |>
  dplyr::mutate(
    assessment = as.factor(2025),
    Gear = dplyr::case_when(
      Gear %in% c("HKL", "Pot") ~ "Fixed",
      TRUE ~ "Trawl")
  ) |>
  dplyr::group_by(year, Gear, assessment) |>
  dplyr::summarise(
    landings_mt = sum(catch_mt)
  ) |>
  dplyr::bind_rows(landings_2023)
ymax <- 1.05 * max(compare_landings[, "landings_mt"])

aggregate(landings_mt~assessment, compare_landings[compare_landings$year < 2023, ], sum)

ggplot(compare_landings, aes(x = year, y = landings_mt, group = assessment)) +
  geom_line(aes(linetype = assessment, color = assessment)) +
  geom_point(aes(shape = assessment, color = assessment)) +
  theme_bw() +
  ylim(c(0, ymax)) + 
  scale_color_viridis_d(begin = 0.2, end = 0.5) +
  ylab("Landings (mt)") + xlab("Year") +
  facet_grid(Gear~.)
ggsave(file = here::here("data-raw", "landings", "figures", "landings_comparison_2023.png"),
       height = 7, width = 7)

#==================================================================
# Exploratory plots of the PacFIN catches
#==================================================================
ggplot(landings, aes(x = year, y = catch_mt, fill = Gear)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Catch (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d(begin = 0.0, end = 0.5)
ggsave(file = here::here("data-raw", "landings", "figures", "landings_by_gear_group.png"),
       height = 7, width = 7)

ggplot(landings, aes(x = year, y = catch_mt, fill = Gear)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  facet_grid(area~., scales = "free_y") +
  xlab("Year") + ylab("Catch (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d(begin = 0.0, end = 0.5)
ggsave(file = here::here("data-raw", "landings", "figures", "landings_by_gear_area.png"),
       height = 7, width = 7)


ggplot(landings |> 
         filter(!state %in% c("at-sea", "foreign") & year > 1980), 
       aes(x = year, y = catch_mt, fill = Gear)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  facet_grid(state~., scales = "free_y") +
  xlab("Year") + ylab("Catch (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d(begin = 0.0, end = 0.5)
ggsave(file = here::here("data-raw", "landings", "figures", "landings_by_gear_states_exclude_at_sea_foreign.png"),
       height = 7, width = 7)

ggplot(landings, aes(x = year, y = catch_mt, fill = Gear)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  facet_grid(state~., scales = "free_y") +
  #facet_wrap("state", scales = "free_y") +
  xlab("Year") + ylab("Catch (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d(begin = 0.0, end = 0.5)
ggsave(file = here::here("data-raw", "landings", "figures", "landings_by_gear_states.png"),
       height = 7, width = 7)

#==================================================================
# Process PacFIN with unique identifiers (e.g., catch share)
#==================================================================
raw_pacfin_catch <-
  fs::dir_ls(here::here("data-raw", "landings"), regex = "PacFIN\\..+Comp")[1] |>
  purrr::map_df(
    .f = function(x) {load(x); return(catch.pacfin)}
  ) |>
  tibble::tibble()

port_lats <- pacfin_ports_withlatlong |>
  dplyr::rename(
    pacfin_port_code = pcid
  ) |>
  dplyr::select(
    c(-name, -agencydesc, -agid)
  ) |>
  dplyr::distinct(pacfin_port_code, .keep_all = TRUE) |>
  tibble::tibble()

pacfin_catch <- raw_pacfin_catch |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::left_join(
    y = port_lats
  ) |>
  dplyr::filter(
    !(landing_year < 1987 & agency_code == "O"),
    !(landing_year < 1991 & agency_code == "W")
  ) |>
  dplyr::mutate(
    catch_mt = round_weight_mtons,
    year = landing_year,
    gear = pacfin_group_gear_code,
    state = dplyr::case_match(agency_code, "W" ~ "Washington", "O" ~ "Oregon", "C" ~ "California"),
    area = dplyr::case_when(latitude <= 36 ~"South", .default = "North"),
    # Currently this is lumping DRG MSC NET TWS into trawl
    gear_group = dplyr::case_when(
      gear %in% c("TLS", "HKL") ~ "Hook-and-Line",
      gear %in% c("POT") ~ "Pot",
      TRUE ~ "Trawl"),
    catch_share = dplyr::case_when(
      gmt_sablefish_code %in% c("IFQN", "IFQS", "IFQU") ~ "Catch Share",
      TRUE ~ "Non-catch Share")
  )  |>
  dplyr::group_by(year, gear_group, state, area, catch_share) |>
  dplyr::summarise(
    catch_mt = sum(catch_mt)
  ) 

# Pull in the GEMM since the at-sea catches includes both discards and landed fish
gemm <- nwfscSurvey::pull_gemm(
  common_name = "sablefish") 
at_sea_discard <- gemm |>
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
    state = "At-Sea",
    area = "North",
    gear_group = "Trawl",
    discard_and_landings_mt = 0.001 * sum(EXPANDED_SumOfEXTRAPOLATED_WEIGHT_KG)
  ) |>
  dplyr::left_join(at_sea_discard, by = c("year")) |>
  dplyr::mutate(
    catch_share = dplyr::case_when(
      year %in% 1950:2010 ~ "Non-catch Share", TRUE ~ "Catch Share"),
    landing_rate = dplyr::case_when(is.na(landing_rate) ~ 1, .default = landing_rate),
    catch_mt = landing_rate * discard_and_landings_mt
  ) |>
  dplyr::select(-landing_rate, -discard_and_landings_mt)

recent_landings <- dplyr::bind_rows(
  pacfin_catch,
  at_sea_catch) |>
  dplyr::filter(year > 1990)


ggplot(recent_landings |> dplyr::filter(year > 2010), aes(x = year, y = catch_mt, fill = catch_share)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  facet_grid(area~., scales = "free_y") +
  xlab("Year") + ylab("Catch (mt)") +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(file = here::here("data-raw", "landings", "figures", "landings_by_catch_share_2011_2024.png"),
       height = 7, width = 7)

#==================================================================
# Compare the GEMM and PacFIN landings
#==================================================================
gemm <- gemm |>
  dplyr::rename(
    landings_mt = total_landings_mt,
    catch_mt = total_discard_with_mort_rates_applied_and_landings_mt
  ) |>
  dplyr::select(-cv, -species, -type) |>
  dplyr::filter(!sector %in% c(
    "California Recreational", "Oregon Recreational", "Washington Recreational"
  )) |>
  dplyr::mutate(
    gear_group = dplyr::case_when(
      sector %in%
        c("Combined LE & OA CA Halibut",
          "CS - Hook & Line",
          "Directed P Halibut",
          "Incidental",
          "LE CA Halibut",
          "LE Fixed Gear DTL - Hook & Line",
          "LE Sablefish - Hook & Line",
          "Nearshore",
          "OA CA Halibut",
          "OA Fixed Gear - Hook & Line")  ~ "Hook-and-Line",
      sector %in% c(
        "CS - Pot",
        "CS EM - Pot",
        "LE Fixed Gear DTL - Pot",
        "LE Sablefish - Pot",
        "OA Fixed Gear - Pot") ~ "Pot",
      TRUE ~ "Trawl"
    )
  ) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    landings_mt = sum(landings_mt)
  ) |>
  dplyr::mutate(
    Source = "GEMM"
  )

landings_gemm_years <- landings |>
  dplyr::filter(year > 2001 & year < 2024) |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    landings_mt = sum(catch_mt), 
    Source = "Model"
  )

gemm_model <- dplyr::bind_rows(
  landings_gemm_years, gemm
  ) 

ggplot(gemm_model, aes(x = year, y = landings_mt, group = Source)) +
  geom_line(aes(linetype = Source, color = Source)) +
  geom_point(aes(shape = Source, color = Source)) +
  theme_bw() +
  ylim(c(0, 7500)) + 
  scale_color_viridis_d(begin = 0.2, end = 0.5) +
  ylab("Landings (mt)") + xlab("Year")
ggsave(file = here::here("data-raw", "landings", "figures", "landings_model_vs_gemm.png"),
       height = 7, width = 7)

gemm_model_wide <- gemm_model |>
  tidyr::pivot_wider(
    names_from = Source,
    values_from = landings_mt
  ) |>
  dplyr::mutate(
    difference = Model - GEMM,
    proportion = Model / GEMM
  )

ggplot(gemm_model_wide) +
  geom_hline(yintercept = 1, color = 'grey') +
  geom_line(aes(x = year, y = proportion)) +
  geom_point(aes(x = year, y = proportion)) +
  ylim(0.90, 1.10) +
  xlab("Year") + ylab("Model / GEMM Landings")

ggplot(gemm_model_wide, aes(x = year, y = difference)) +
  geom_bar(stat = 'identity') +
  xlab("Year") + ylab("Model - GEMM Landings")
ggsave(file = here::here("data-raw", "landings", "figures", "landings_model_vs_gemm_difference.png"),
       height = 7, width = 7)

sector_2019 <- aggregate(
  ROUND_WEIGHT_MTONS~ FOS_GROUNDFISH_SECTOR_CODE, 
  data = raw_pacfin_catch[raw_pacfin_catch$LANDING_YEAR == 2019, ], function(x) round(sum(x),1))

ggplot(raw_pacfin_catch |> dplyr::filter(LANDING_YEAR == 2019),
       aes(y = FOS_GROUNDFISH_SECTOR_CODE, x = ROUND_WEIGHT_MTONS)) +
  geom_bar(stat = 'identity') +
  ylab("Sector") + xlab("2019 Landings (mt)")

combine_gemm_sectors <- function(data){
  data$sector[
    data$sector %in% c("OA Fixed Gear - Pot", "OA Fixed Gear - Hook & Line")] <-
    "OA Fixed Gear"
  data$sector[data$sector == "Incidental"] <- "Other Fisheries + EFP"
  data$sector[data$sector == "Tribal Shoreside"] <- "Tribal"
  data$sector[
    data$sector %in% c("CS - Bottom Trawl", "CS - Hook & Line",
                       "CS - Pot", "CS - Bottom and Midwater Trawl")] <- "Catch Shares"
  data$sector[
    data$sector %in% c("CS EM - Bottom Trawl", 
                       "CS EM - Pot")] <- "Catch Shares"
  data$sector[
    data$sector %in% c("Midwater Hake", 
                       "Midwater Hake EM")] <- "Midwater Hake"
  data$sector[
    data$sector %in% c("LE Fixed Gear DTL - Hook & Line", 
                       "LE Fixed Gear DTL - Pot")] <- "LE Fixed Gear DTL"
  data$sector[
    data$sector %in% c("LE Sablefish - Hook & Line", 
                       "LE Sablefish - Pot")] <- "Limited Entry Sablefish"
  data$sector[
    data$sector %in% c("At-Sea Hake CP", 
                       "At-Sea Hake MSCV")] <- "At-Sea Hake"
  
  combined_data <- data |>
    dplyr::filter(!sector %in% 
      c("Oregon Recreational", "California Recreational", "Washington Recreational")) |>
    dplyr::group_by(year, sector) |>
    dplyr::summarise(
      total_landings_mt = sum(total_landings_mt)
    )
  return(combined_data)
}
# 2019
# GEMM tribal = 454, PacFIN tribal = 519.4
# 2003
# GEMM research = 0, PacFIN research = 39.5
# GEMM Incidental = 273, PacFIN other fisheries = 2.2
# GEMM EFP = 0, PacFIN EFP = 118.7
# 2004
# GEMM EFP = 0, PacFIN EFP = 70.5
# GEMM Incidental = 154, PacFIN other fisheries = 8.9

gemm_by_sector <- combine_gemm_sectors(
  data = nwfscSurvey::pull_gemm(common_name = "sablefish")) |>
  dplyr::mutate(
    source = "gemm",
    total_landings_mt = round(total_landings_mt, 3)
  )
pacfin_sector_modified <- raw_pacfin_catch
pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE[pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE %in% c("Catch Shares EM", "Catch Share")] <- "Catch Shares"
pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE[pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE == "Midwater Hake EM"] <- "Midwater Hake"
pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE[pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE == "Shoreside Hake Pre-CS"] <- "Shoreside Hake"
pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE[
  pacfin_sector_modified$FOS_GROUNDFISH_SECTOR_CODE %in% c("Other Fisheries", "EFP")] <- "Other Fisheries + EFP"

pacfin_gemm <- pacfin_sector_modified |>
  dplyr::filter(LANDING_YEAR >= 2002) |>
  dplyr::group_by(LANDING_YEAR, FOS_GROUNDFISH_SECTOR_CODE) |>
  dplyr::summarise(
    source = "pacfin",
    total_landings_mt = round(sum(ROUND_WEIGHT_MTONS), 3)
  ) |>
  dplyr::rename(
    year = LANDING_YEAR, 
    sector = FOS_GROUNDFISH_SECTOR_CODE
  ) |>
  dplyr::bind_rows(gemm_by_sector) |>
  tidyr::pivot_wider(
    names_from = source,
    values_from = total_landings_mt,
    values_fill = 0
  ) |>
  dplyr::mutate(
    difference = round(pacfin - gemm, 3)
  ) |> 
  dplyr::filter(year != 2024, sector != "At-Sea Hake")

for (y in 2002:2023){
  xmin = plyr::round_any(1.05 * min(pacfin_gemm$difference), 100, floor)
  xmax = plyr::round_any(1.05 * max(pacfin_gemm$difference), 100, ceiling)
  ggplot(pacfin_gemm |> dplyr::filter(year == y),
         aes(y = sector, x = difference)) +
    geom_bar(stat = 'identity') +
    xlim(xmin, xmax) + 
    ylab("Sector") + xlab("PacFIN - GEMM Landings Difference") +
    facet_wrap("year") 
  ggsave(
    filename = here::here(
      "data-raw", "landings", "figures", "pacfin_gemm",
      paste0("PacFIN_GEMM_Comparison_", y, ".png")),
    height = 7,
    width = 10)
}

#===============================================================================
# Check catch for research and tribal
#===============================================================================
research <- aggregate(ROUND_WEIGHT_MTONS~ LANDING_YEAR, data = raw_pacfin_catch[raw_pacfin_catch$REMOVAL_TYPE_CODE == "R", ], function(x) round(sum(x),1))
tribal <- aggregate(ROUND_WEIGHT_MTONS~ LANDING_YEAR, data = raw_pacfin_catch[raw_pacfin_catch$FLEET_CODE == "TI", ], function(x) round(sum(x),1))

