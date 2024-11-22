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
ggsave(file = here::here("data-raw", "landings", "figures", "landings_by_gear_states_exclude_at_sea.png"),
       height = 7, width = 7)

#==================================================================
# Process PacFIN with unique identifiers (e.g., catch share)
#==================================================================
raw_pacfin_catch <-
  fs::dir_ls(here::here("data-raw", "landings"), regex = "PacFIN\\..+Comp") |>
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

# At-Sea Catches
at_sea_catch <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sabefish Catch_summarized_1978-2023_102224.xlsx"),
  sheet = "Sablefish Catch 1979-2023") |>
  dplyr::rename(
    year = YEAR
  ) |>
  dplyr::filter(year > 1980) |>
  dplyr::mutate(
    catch_share = dplyr::case_when(
      year %in% 1981:2010 ~ "Non-catch Share", TRUE ~ "Catch Share")
  ) |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    gear_group = "Trawl",
    state = "At-Sea",
    area = "North",
    catch_share = unique(catch_share),
    catch_mt = 0.001 * sum(EXPANDED_SumOfEXTRAPOLATED_WEIGHT_KG)
  )

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
# Pull gemm and compare the PacFIN landings
#==================================================================
gemm <- nwfscSurvey::pull_gemm(common_name = "sablefish") |>
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
  dplyr::filter(year < 2023 & year > 2001) |>
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

proportion <- data.frame(
  year = sort(unique(gemm_model$year)),
  prop = gemm_model[gemm_model$Source == "Model", "landings_mt"] /
  gemm_model[gemm_model$Source == "GEMM", "landings_mt"])
par(mfrow = c(1, 1))
plot(proportion$year, proportion[, 2], type = "b", lwd = 2, ylim = c(0.85, 1.15),
     ylab = "Model / GEMM Landings", xlab = "Year")
abline(h = 1, lty = 2, lwd = 2, col = "grey")

#===============================================================================
# Check catch for research and tribal
#===============================================================================
# research <- aggregate(catch_mt~ year + state, data = pacfin_catch[pacfin_catch$removal_type_code == "R", ], function(x) round(sum(x),1))
# tribal <- aggregate(catch_mt~ year + state, data = pacfin_catch[pacfin_catch$fleet_code == "TI", ], function(x) round(sum(x),1))


