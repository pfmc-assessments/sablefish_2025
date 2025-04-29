
# Pull estimates from the 2023 model
model_2023 <- r4ss::SS_output(here::here("model", "_bridging", "0_2023_model"))
# Pull values from the revised model with discard fleets
#discard_fleets <- r4ss::SS_output(here::here("model", "0_init"))
discard_fleets <- read.csv(here::here("data-processed", "data_commercial_catch.csv")) 

#mortality <- dplyr::bind_rows(
#  model_2023[["catch"]][, c("Fleet_Name", "Yr", "kill_bio")] |>
#    dplyr::rename(dead_bio = kill_bio) |>
#    dplyr::mutate(Assessment = as.factor(2023)),
#  discard_fleets[["catch"]][, c("Fleet_Name", "Yr", "dead_bio")] |>
#    dplyr::mutate(Assessment = as.factor(2025))
#) |>
#  dplyr::rename(
#    Year = Yr
#  ) |>
#  dplyr::filter(Year < 2023)

mortality <- dplyr::bind_rows(
  model_2023[["catch"]][, c("Fleet_Name", "Yr", "kill_bio")] |>
    dplyr::rename(
      fleet = Fleet_Name,
      Year = Yr,
      dead_bio = kill_bio) |>
    dplyr::mutate(
      Assessment = as.factor(2023),
      fleet = dplyr::case_when(fleet == "TWL" ~ "trawl", .default = "hkl+pot")
  ),
  discard_fleets |>
    dplyr::select(-seas, -catch_se) |>
    dplyr::rename(
      dead_bio = catch_mt,
      Year = year
    ) |>
    dplyr::mutate(
      fleet = recode_fleet_text_cw(fleet),
      Assessment = as.factor(2025)
    )
) |>
  dplyr::filter(Year < 2023)

total_mortality <- mortality |>
  dplyr::group_by(Year, Assessment) |>
  dplyr::summarise(
    catch = sum(dead_bio)
  ) 

diff_catch <- total_mortality |>
  dplyr::filter(Year >= 1890) |>
  tidyr::pivot_wider(
    names_from = Assessment,
    values_from = catch
  ) |>
  dplyr::mutate(
    diff = `2025` - `2023`,
    percent = 100 * (`2025` / `2023`) - 100)

library(ggplot2)
ggplot(mortality, aes(x = Year, y = dead_bio, fill = fleet)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Catch (mt)") +
  facet_grid("Assessment")

ggplot(total_mortality, aes(x = Year, y = catch, linetype = Assessment, color = Assessment)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  xlab("Year") + ylab("Catch (mt)") +
  scale_color_viridis_d(begin = 0, end = 0.5)

ymax <- max(diff_catch[, "diff"]) + 200
ymin <- -1 * ymax
ggplot(diff_catch, aes(x = Year, y = diff)) +
  geom_line(linewidth = 1) +
  ylim(c(ymin, ymax)) + 
  theme_bw() +
  xlab("Year") + ylab("2025 Catch (mt) - 2023 Catch (mt)") 

ymax <- max(diff_catch[, "percent"]) + 0.1
ymin <- -1 * ymax
ggplot(diff_catch, aes(x = Year, y = percent)) +
  geom_line(linewidth = 1) +
  ylim(c(ymin, ymax)) + 
  theme_bw() +
  xlab("Year") + ylab("Percent Difference") 

# Calculate the totals for each
aggregate(dead_bio~Assessment, mortality, sum)
#   Assessment dead_bio
# 1       2023 623773.6
# 2       2025 633086.0
# The new calculations for mortality are 1.5% higher than those from 2023

# Compare calculated discard estimates to GEMM
gemm <- nwfscSurvey::pull_gemm(common_name = "sablefish")
gemm_totals <- gemm |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    discard = sum(total_discard_with_mort_rates_applied_mt),
    landings = sum(total_landings_mt),
    catch = sum(total_discard_with_mort_rates_applied_and_landings_mt),
    source = "gemm"
  )
model_totals <- discard_fleets |>
  dplyr::filter(year %in% 2002:2023) |>
  dplyr::mutate(
    discard = dplyr::case_when(fleet %in% 4:6 ~ catch_mt, .default = 0),
    landings = dplyr::case_when(fleet %in% 1:3 ~ catch_mt, .default = 0)
  ) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    discard = sum(discard),
    landings = sum(landings),
    catch = sum(discard + landings),
    source = "model"
  )
compare_mortality <- dplyr::bind_rows(
  gemm_totals,
  model_totals
)
ggplot(compare_mortality, aes(x = year, y = discard)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Discard (mt)") +
  facet_grid("source")
ggplot(compare_mortality, aes(x = year, y = landings)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Landings (mt)") +
  facet_grid("source")
ggplot(compare_mortality, aes(x = year, y = catch)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Catch (mt)") +
  facet_grid("source")

compare_wide <- compare_mortality |>
  dplyr::select(-discard, -landings) |>
  tidyr::pivot_wider(
    names_from = source,
    values_from = catch
  ) |>
  dplyr::mutate(
    diff = model - gemm
  )
ggplot(compare_wide, aes(x = year, y = diff)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Model - GEMM (mt)") 

sum(compare_wide$diff)
# -1306
sum(compare_wide$diff[compare_wide$year > 2004])
# -323

#===============================================================================
# Break it down by gear type
#===============================================================================
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
  "OA Fixed Gear - Hook & Line",
  "Washington Recreational",
  "Oregon Recreational",
  "California Recreational"
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
gemm_gear <- gemm
gemm_gear[which(gemm_gear[, "sector"] %in% hkl), "fleet"] <- "hook-and-line"
gemm_gear[which(gemm_gear[, "sector"] %in% trawl), "fleet"] <- "trawl"
gemm_gear[which(gemm_gear[, "sector"] %in% pot), "fleet"] <- "pot"
gemm_gear <- gemm_gear |>
  dplyr::select(year, fleet, sector, 
                total_discard_with_mort_rates_applied_mt, 
                total_landings_mt,
                total_discard_with_mort_rates_applied_and_landings_mt) |>
  dplyr::mutate(
    total_discard_with_mort_rates_applied_mt = round(total_discard_with_mort_rates_applied_mt, 1),
    total_landings_mt = round(total_landings_mt, 1),
    total_discard_with_mort_rates_applied_and_landings_mt = round(total_discard_with_mort_rates_applied_and_landings_mt, 1)
  )
gemm_gear_total <- gemm_gear |>
  dplyr::group_by(year, fleet) |>
  dplyr::summarise(
    discard = sum(total_discard_with_mort_rates_applied_mt),
    landings = sum(total_landings_mt),
    catch = sum(total_discard_with_mort_rates_applied_and_landings_mt),
    source = "gemm"
  )
model_by_gear <- discard_fleets |>
  dplyr::filter(year %in% 2002:2023) |>
  dplyr::mutate(
    discard = dplyr::case_when(fleet %in% 4:6 ~ catch_mt, .default = 0),
    landings = dplyr::case_when(fleet %in% 1:3 ~ catch_mt, .default = 0),
    fleet = dplyr::case_when(fleet %in% c(1, 4) ~ "trawl", fleet %in% c(2, 5) ~ "hkl", .default = "pot")
  ) |>
  dplyr::group_by(year, fleet) |>
  dplyr::summarise(
    discard = sum(discard),
    landings = sum(landings),
    catch = sum(discard + landings),
    source = "model"
  )
compare_mortality_by_gear <- dplyr::bind_rows(
  gemm_gear_total,
  model_by_gear
)
ggplot(compare_mortality_by_gear, aes(x = year, y = discard, fill = fleet)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Discard (mt)") +
  facet_grid("source")
ggplot(compare_mortality_by_gear, aes(x = year, y = landings, fill = fleet)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Landings (mt)") +
  facet_grid("source")
ggplot(compare_mortality_by_gear, aes(x = year, y = catch, fill = fleet)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  xlab("Year") + ylab("Landings (mt)") +
  facet_grid("source")

# aggregate(catch~source, compare_mortality_by_gear, sum)
#  source  catch
# 1   gemm 127706.8
# 2  model 126400.8
# 126400.8 / 127706.8 = 99.0%
#  aggregate(catch~source, compare_mortality_by_gear[compare_mortality_by_gear$year > 2004, ], sum)
# source    catch
# 1   gemm 110319.4
# 2  model 109995.9
# 109995.9 / 110319.4
# 99.7%

ggplot(compare_mortality_by_gear |> 
         dplyr::group_by(year, source) |>
         dplyr::summarise(catch = sum(catch)), aes(x = year, y = catch, linetype = source, color = source)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  ylim(c(0, 8000)) +
  xlab("Year") + ylab("Catch (mt)") +
  scale_color_viridis_d(begin = 0, end = 0.5)
