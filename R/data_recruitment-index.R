library(r4ss)

model_2023 <- SS_output(
  dir = here::here("model", "_bridging", "0_2023_model"),
  verbose = FALSE,
  printstats = FALSE)

model_2023_wo_index <- SS_output(
  dir = here::here("model", "_bridging", "_2023 sensitivities", "0_2023_model_rm_enviro"),
  verbose = FALSE,
  printstats = FALSE)

data_with_index <- data.frame(
  year = readr::parse_number(model_2023$parameters$Label[find]),
  recdevs = model_2023$parameters[grep("RecrDev", model_2023$parameters$Label), "Value"],
  model = "with_ssh_index",
  assessment = 2023
)

data_wo_index <- data.frame(
  year = readr::parse_number(model_2023_wo_index$parameters$Label[find]),
  recdevs = model_2023_wo_index$parameters[grep("RecrDev", model_2023_wo_index$parameters$Label), "Value"],
  model = "without_ssh_index",
  assessment = 2023
)

data_model_recdevs <- dplyr::bind_rows(
  data_with_index,
  data_wo_index
)


gg <- ggplot2::ggplot(data_model_recdevs, 
  aes(x = year, y = recdevs, color = model, linetype = model, shape = model)) +
  geom_point() + geom_line() +
  theme_bw() +  
  scale_color_viridis_d(begin = 0, end = 0.5) +
  ylab("Estimated Rec. Dev.") + xlab("Year")

saveRDS(data_model_recdevs, file = here::here("data-processed", "estimated_rec_devs_2023.rds"))
