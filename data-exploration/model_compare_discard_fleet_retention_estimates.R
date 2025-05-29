# Compare the model metrics across the model scructures
retention <- SS_output(here::here("model", "_retention_model", "_growth", "21.6_growth_sd_log_dw"))
discard_fleet <- SS_output(here::here("model", "_discard_fleets", "growth", "7.9_triennial_block_width_final_extra_sd_dw"))


total_mortality <- dplyr::bind_rows(
  retention$catch |> dplyr::mutate(Model = "Retention"),
  discard_fleet$catch |> dplyr::mutate(Model = "Discard Fleets")
  ) |>
  dplyr::mutate(
    Fleet_Name = dplyr::case_when(
      Fleet_Name %in% c("AtSea_TWL_Foreign", "TWL_Discards", "TWL") ~ "TWL",
      Fleet_Name %in% c("HKL_Foreign", "HKL_Discards", "HKL") ~ "HKL",
      Fleet_Name %in% c("Pot_Foreign", "Pot_Discard", "Pot") ~ "Pot"
    )
  ) |>
  dplyr::group_by(Yr, Model, Fleet_Name) |>
  dplyr::summarise(
    catch = sum(dead_bio)
  ) 

ggplot(total_mortality, aes(x = Yr, y = catch, linetype = Model, color = Model)) +
  geom_line(size = 1) +
  theme_bw() +
  scale_color_viridis_d() + 
  facet_grid("Fleet_Name", scales = "free_y")
aggregate(catch ~ Fleet_Name + Model, total_mortality, sum)
# Pot Discard Fleets - Retention = -1,654
# HKL Discard Fleets - Retention = 547
# TWL Discard Fleets - Retention = 10,787

#===============================================================================
# Selectivity Comparison
#===============================================================================
plot_fleet_selectivity(model_out = retention, fleet_num = 1)
plot_fleet_selectivity(model_out = discard_fleet, fleet_num = 1)

plot_fleet_selectivity(model_out = retention, fleet_num = 2)
plot_fleet_selectivity(model_out = discard_fleet, fleet_num = 2)

plot_fleet_selectivity(model_out = retention, fleet_num = 3)
plot_fleet_selectivity(model_out = discard_fleet, fleet_num = 3)


plot_fleet_retention(model_out = retention, fleet_num = 1)
plot_fleet_selectivity(model_out = discard_fleet, fleet_num = 4)

plot_fleet_retention(model_out = retention, fleet_num = 2)
plot_fleet_selectivity(model_out = discard_fleet, fleet_num = 5)

plot_fleet_retention(model_out = retention, fleet_num = 3)
plot_fleet_selectivity(model_out = discard_fleet, fleet_num = 6)
