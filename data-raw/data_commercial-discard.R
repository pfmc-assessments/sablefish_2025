
library(nwfscSurvey)
area <- "coastwide"

# ==============================================================================
# Create WCGOP data_commercial_discard_composition
# ==============================================================================
data_commercial_discard_composition <-
  # Length frequencies
  utils::read.csv(
    here::here("data-raw", "discard", "wcgop", area, "biological_discard_lengths.csv")
  ) |>
  # replace the fleet names with the fleet number
  dplyr::mutate(
    fleet = dplyr::case_when(
      fleet == "trawl-coastwide" ~ 1,
      fleet == "hook-and-line-coastwide" ~ 2,
      fleet == "pot-coastwide" ~ 3,
      TRUE ~ NA_integer_
    ),
    month = 7
  ) |>
  dplyr::rename(`#year` = year) |>
  dplyr::arrange(fleet) |>
  dplyr::rename_with(~gsub("X", "U-", .x), dplyr::starts_with("X"))
    
# ============================================================================== 
# Create WCGOP data_commercial_discard_weight  
# ==============================================================================
data_commercial_discard_weight <- utils::read.csv(
  here::here("data-raw", "discard", "wcgop", area, "discard_mean_body_weights.csv")) |>
  # replace the fleet names with the fleet number
  dplyr::mutate(
    fleet = dplyr::case_when(
      fleet == "trawl-coastwide" ~ 1,
      fleet == "hook-and-line-coastwide" ~ 2,
      fleet == "pot-coastwide" ~ 3,
      TRUE ~ NA_integer_
    ),
    month = 7,
    obs = round(obs, 4),
    cv = round(cv, 4)
  ) |>
  dplyr::rename(`#year` = year) |>
  dplyr::arrange(fleet) 

# ==============================================================================
# Process the gemm data to get relative discarding practices by gear type
# ==============================================================================
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
gemm_data[which(gemm_data[, "sector"] %in% hkl), "fleet"] <- "hook-and-line"
gemm_data[which(gemm_data[, "sector"] %in% trawl), "fleet"] <- "trawl"
gemm_data[which(gemm_data[, "sector"] %in% pot), "fleet"] <- "pot"
gemm_data[, "fleet"] <- apply(gemm_data[, c("catch_shares", "fleet")], 1, paste, collapse = "-")
catch_totals <- gemm_data |>
  dplyr::filter(year >= 2011) |>
  dplyr::group_by(year) |>
  dplyr::mutate(
    landed_mt_by_year = sum(total_landings_mt),
    discard_mt_by_year = sum(total_discard_mt),
    dead_discard_mt_by_year = sum(total_discard_with_mort_rates_applied_mt),
    catch_by_year = sum(total_discard_with_mort_rates_applied_and_landings_mt),
  ) |>
  dplyr::group_by(year, fleet) |>
  dplyr::summarise(
    discard_mt = sum(total_discard_mt),
    dead_discard_mt = sum(total_discard_with_mort_rates_applied_mt),
    landed_mt = sum(total_landings_mt),
    catch = sum(total_discard_with_mort_rates_applied_and_landings_mt),
    gemm_discard_rate = discard_mt / (landed_mt + discard_mt),
    gemm_dead_discard_rate = dead_discard_mt / (landed_mt + dead_discard_mt),
    prop_discard = discard_mt / unique(discard_mt_by_year),
    prop_landed = landed_mt / unique(landed_mt_by_year),
    prop_catch = catch / unique(catch_by_year)
  ) |>
  dplyr::ungroup()
catch_totals$cs_fleet <- catch_totals$fleet
catch_totals$catch_share <- TRUE
catch_totals[grep("non-catch-shares", catch_totals$fleet), "catch_share"] <- FALSE
catch_totals[grep("trawl", catch_totals$fleet), "fleet"] <- "trawl-coastwide"
catch_totals[grep("hook-and-line", catch_totals$fleet), "fleet"] <- "hook-and-line-coastwide"
catch_totals[grep("pot", catch_totals$fleet), "fleet"] <- "pot-coastwide"
gemm_weights <- catch_totals |>
  dplyr::group_by(year, fleet) |>
  dplyr::mutate(
    cs_weight = prop_discard / sum(prop_discard)
  ) |>
  tibble::as_tibble() |>
  tidyr::complete(year, fleet, catch_share, fill = list(cs_weight = 0)) |>
  dplyr::filter(catch_share == "TRUE") |>
  dplyr::select(year, fleet, cs_weight) 

# ==============================================================================
# Weight the WCGOP discard rate data based on the GEMM data
# ==============================================================================
commercial_discard_rates_pre_2011 <- 
  utils::read.csv(
    here::here("data-raw", "discard", "wcgop", area, "discard_rates_noncatch_share.csv")
  ) |>
  dplyr::filter(year < 2011) |>
  dplyr::mutate(
    discard_rate = round(median_ratio, 4),
    sd = round(dplyr::case_when(sd_ratio > 0.03 ~ sd_ratio, .default = 0.03), 3),
  ) |>
  dplyr::select(year, fleet, discard_rate, sd)
cs_discard_rates <- 
  utils::read.csv(
    here::here("data-raw", "discard", "wcgop", area, "discard_rates_combined_catch_share.csv")
  ) |> 
  dplyr::filter(year >= 2011) |>
  dplyr::mutate(sd_ratio = 0.03) |>
  dplyr::rename(cs_rate = discard_rate, cs_sd = sd_ratio) |>
  dplyr::select(year, fleet, cs_rate, cs_sd) 
ncs_discard_rates <- 
  utils::read.csv(
    here::here("data-raw", "discard", "wcgop", area, "discard_rates_noncatch_share.csv")
  ) |>
  dplyr::filter(year >= 2011) |>
  dplyr::rename(ncs_rate = median_ratio, ncs_sd = sd_ratio) |>
  dplyr::select(year, fleet, ncs_rate, ncs_sd) 
discard_rates_2011 <- dplyr::full_join(cs_discard_rates, ncs_discard_rates) |>
  dplyr::mutate(
    cs_rate = dplyr::coalesce(cs_rate, 0),
    cs_sd = dplyr::coalesce(cs_sd, 0),
    ncs_rate = dplyr::coalesce(ncs_rate, 0),
    ncs_sd = dplyr::coalesce(ncs_sd, 0)) 

wcgop_commercial_discard_rates <- dplyr::left_join(
  discard_rates_2011, gemm_weights) |>
  dplyr::mutate(
    discard_rate = round(cs_rate * cs_weight + ncs_rate * (1 - cs_weight), 4),
    sd = round(dplyr::case_when(ncs_sd > 0.03 ~ ncs_sd, .default = 0.03), 3)
  ) |>
  filter(!is.na(discard_rate)) |>
  dplyr::select(year, fleet, discard_rate, sd) |>
  dplyr::bind_rows(commercial_discard_rates_pre_2011) |>
  dplyr::mutate(
    month = 7,
    fleet = dplyr::case_when(
      fleet == "trawl-coastwide" ~ 1,
      fleet == "hook-and-line-coastwide" ~ 2,
      fleet == "pot-coastwide" ~ 3,
      TRUE ~ NA_integer_
    )
  ) |>
  dplyr::arrange(fleet, year) |>
  dplyr::relocate(month, .before = fleet)

# ==============================================================================
# Pikitch discard rates from the 2019 assessment
# ==============================================================================
pikitch_trawl_discards <- data.frame(
  year = 1985:1987,
  month = 7,
  fleet = 1,
  discard_rate = c(0.3731,  0.3637, 0.3532),
  sd = c(0.2577, 0.2368, 0.229)
)
data_commercial_discard_rates <- dplyr::bind_rows(
  pikitch_trawl_discards,
  wcgop_commercial_discard_rates
)

# ==============================================================================
# ashop discard lengths 
# ==============================================================================
ashop_lengths <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sablefish Lengths_102224.xlsx"),
  sheet = "2019-2024") |>
  dplyr::rename_with(
    tolower
  ) |>
  tidyr::uncount(frequency) |>
  dplyr::mutate(
    sex = nwfscSurvey::codify_sex(sex),
    common_name = "sablefish",
    trawl_id = haul_join
  ) |> as.data.frame()

comps <- nwfscSurvey::get_raw_comps(
  data = ashop_lengths,
  comp_bins = seq(18, 90, 2),
  comp_column_name = "length",
  input_n_method = "stewart_hamel",
  fleet = 1,
  month = 7,
  partition = 1
)
colnames(comps$unsexed) <- colnames(comps$sexed)

ashop_lengths_early <- readxl::read_excel(
  path = here::here("data-raw", "ashop", "A-SHOP_Sablefish_Lengths_1978-83_021319.xlsx"),
  sheet = "Sheet1") |>
  dplyr::rename_with(
    tolower
  ) |>
  tidyr::uncount(frequency) |>
  dplyr::mutate(
    sex = nwfscSurvey::codify_sex(sex),
    common_name = "sablefish",
    trawl_id = haul_join,
    length = size_group
  ) |> as.data.frame()

early_comps <- nwfscSurvey::get_raw_comps(
  data = ashop_lengths_early,
  comp_bins = seq(18, 90, 2),
  comp_column_name = "length",
  input_n_method = "stewart_hamel",
  fleet = 1,
  month = 7,
  partition = 1
)
colnames(early_comps$unsexed) <- colnames(early_comps$sexed)
data_ashop_discard_composition <- dplyr::bind_rows(
  early_comps$sexed,
  early_comps$unsexed,
  comps$sexed, 
  comps$unsexed
)

# Write the three objects to data-processed
write_named_csvs(
  data_commercial_discard_composition,
  data_commercial_discard_weight,
  data_commercial_discard_rates,
  data_ashop_discard_composition,
  dir = "data-processed"
)
