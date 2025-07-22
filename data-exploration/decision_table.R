folder <- "m"
catch_name <- "pstar_45"
base <- r4ss::SS_output(here::here("model", "base_model", "decision_table", folder, paste0("base_", catch_name)))

proj_year <- 2025:2036

base_catch_stream <- data.frame(
  year = proj_year,
  catch = base$derived_quants[base$derived_quants$Label %in% paste0("ForeCatch_", proj_year), "Value"])
sb_base <- round(base$derived_quants[base$derived_quants$Label %in% paste0("SSB_", proj_year), "Value"], 0)
bratio_base <- round(base$derived_quants[base$derived_quants$Label %in% paste0("Bratio_", proj_year), "Value"], 2)
ofl <- base$derived_quants[base$derived_quants$Label %in% paste0("OFLCatch_", proj_year), "Value"]

catch_ave <- base$catch |>
  dplyr::filter(Yr %in% 2020:2024) |>
  dplyr::mutate(total = sum(dead_bio)) |>
  dplyr::group_by(Fleet_Name) |>
  dplyr::summarise(
    proportion = sum(dead_bio) / unique(total)
  ) |>
  dplyr::mutate(
    fleet = dplyr::case_when(
      Fleet_Name == "Trawl" ~ 1,
      Fleet_Name == "Hook_and_Line" ~ 2,
      Fleet_Name == "Pot" ~ 3,
      Fleet_Name == "Trawl_Discard" ~ 4,
      Fleet_Name == "Hook_and_Line_Discard" ~ 5,
      Fleet_Name == "Pot_Discard" ~ 6
    )
  ) |>
  dplyr::arrange(fleet)

#===============================================================================
# Low State of Nature
#===============================================================================
# set the catch in the low state of nature forecast file
# remove the buffer vector from the file (or set all to 1.0)
forecast <- r4ss::SS_readforecast(file = here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name), "forecast.ss"))
forecast$Flimitfraction_m[, "fraction"] <- 1

fore_catch <- NULL
for(a in 2027:2036){
  tmp = data.frame(
    year = a,
    seas = 1,
    fleet = 1:6,
    catch = catch_ave[, "proportion"] * base_catch_stream[base_catch_stream$year == a, "catch"]
  )
  fore_catch <- dplyr::bind_rows(fore_catch, tmp)
}
fore_catch <- fore_catch |> dplyr::rename(catch_or_F = proportion)
forecast$ForeCatch <- dplyr::bind_rows(forecast$ForeCatch, fore_catch)
r4ss::SS_writeforecast(forecast, dir = here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name)), overwrite = TRUE)

starter <- r4ss::SS_readstarter(file = here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name), "starter.ss"))
starter$init_values_src <- 1
starter$last_estimation_phase <- 0
r4ss::SS_writestarter(starter, dir = here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name)), overwrite = TRUE)
# run the model
setwd(here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name)))
shell("ss3 -nohess")

low <- r4ss::SS_output(here::here("model", "base_model", "decision_table", folder, paste0("low_", catch_name)))
ofl_low <- low$derived_quants[low$derived_quants$Label %in% paste0("OFLCatch_", proj_year), "Value"]
abc <- low$derived_quants[low$derived_quants$Label %in% paste0("ForeCatch_", proj_year), "Value"]
sb_low <- round(low$derived_quants[low$derived_quants$Label %in% paste0("SSB_", proj_year), "Value"], 0)
bratio_low <- round(low$derived_quants[low$derived_quants$Label %in% paste0("Bratio_", proj_year), "Value"], 2)

#===============================================================================
# High State of Nature
#===============================================================================
forecast <- r4ss::SS_readforecast(file = here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name), "forecast.ss"))
forecast$Flimitfraction_m[, "fraction"] <- 1

fore_catch <- NULL
for(a in 2027:2036){
  tmp = data.frame(
    year = a,
    seas = 1,
    fleet = 1:6,
    catch = catch_ave[, "proportion"] * base_catch_stream[base_catch_stream$year == a, "catch"]
  )
  fore_catch <- dplyr::bind_rows(fore_catch, tmp)
}
fore_catch <- fore_catch |> dplyr::rename(catch_or_F = proportion)
forecast$ForeCatch <- dplyr::bind_rows(forecast$ForeCatch, fore_catch)
r4ss::SS_writeforecast(forecast, dir = here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name)), overwrite = TRUE)

starter <- r4ss::SS_readstarter(file = here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name), "starter.ss"))
starter$init_values_src <- 1
starter$last_estimation_phase <- 0
r4ss::SS_writestarter(starter, dir = here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name)), overwrite = TRUE)
# run the model
setwd(here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name)))
shell("ss3 -nohess")

high <- r4ss::SS_output(here::here("model", "base_model", "decision_table", folder, paste0("high_", catch_name)))
ofl_high <- high$derived_quants[high$derived_quants$Label %in% paste0("OFLCatch_", proj_year), "Value"]
abc <- high$derived_quants[high$derived_quants$Label %in% paste0("ForeCatch_", proj_year), "Value"]
abc == base_catch_stream[, "catch"]

sb_high <- round(high$derived_quants[high$derived_quants$Label %in% paste0("SSB_", proj_year), "Value"], 0)
bratio_high <- round(high$derived_quants[high$derived_quants$Label %in% paste0("Bratio_", proj_year), "Value"], 2)

#===============================================================================
# Bind Low-Base-High for a decision table row
#===============================================================================
catch_stream <- data.frame(
  Year = proj_year,
  Catch = base_catch_stream[, "catch"],
  Low_SO = sb_low,
  Low_depl = bratio_low, 
  SO = sb_base,
  depl = bratio_base,
  High_SO = sb_high,
  High_depl = bratio_high
)
save(catch_stream, file = here::here("model", "base_model", "decision_table", folder, paste0(catch_name, ".rda")))
write.csv(catch_stream, file = here::here("model", "base_model", "decision_table", folder, paste0(catch_name, ".csv")), row.names = FALSE)

ofl_out <- dplyr::bind_rows(
  data.frame(model = "base", year = proj_year, ofl = ofl),
  data.frame(model = "low", year = proj_year, ofl = ofl_low),
  data.frame(model = "high", year = proj_year, ofl = ofl_high)
) |>
  dplyr::filter(year >= 2027)
write.csv(ofl_out, file = here::here("model", "base_model", "decision_table", folder, paste0(catch_name, "_ofl.csv")), row.names = FALSE)

ggplot2::ggplot(ofl_out, ggplot2::aes(x = year, y = ofl, color = model, shape = model)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_bw() +
  ggplot2::xlab("Projection Year") +
  ggplot2::ylab("OFL (mt)")
ggplot2::ggsave(here::here("model", "base_model", "decision_table", folder, paste0(catch_name, "_", "ofl.png")), height = 7, width = 7)
