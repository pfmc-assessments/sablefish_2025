

#===============================================================================
# r4ss plots
#===============================================================================

model_output <- r4ss::SS_output(here::here("model", "base_model", "8.36_base_model"))
save(model_output, file = here::here("presentation", "model_and_diag", "tables", "model_output.rda"))
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 4,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 7,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 9,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 11,
  forecastplot = FALSE,
  print = TRUE,
  minyr = 1950,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotSPR(
  replist = model_output,
  subplot = 3:4,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotRecdevs(
  replist = model_output,
  subplots = 1:2,
  print = TRUE,
  minyr = 1950,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotIndices(
  replist = model_output,
  subplots = 2,
  print = TRUE,
  plotdir = here::here("presentation",  "model_and_diag", "plots")
)
r4ss::SSplotComps(
  replist = model_output,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  plotdir = here::here("presentation", "model_and_diag",  "plots"),
  fleets = 1:6,
  maxrows = 2,
  pwidth = 6,
  pheight = 5
)
file.rename(here::here("presentation", "model_and_diag", "plots", "comp_agefit__aggregated_across_time.png"),
            here::here("presentation", "model_and_diag", "plots", "comp_agedat__aggregated_across_time_1-6.png"))
r4ss::SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  plotdir = here::here("presentation", "model_and_diag", "plots"),
  fleets = 7:10,
  maxrows = 2,
  pwidth = 6,
  pheight = 5
)
file.rename(here::here("presentation", "model_and_diag", "plots", "comp_agefit__aggregated_across_time.png"),
            here::here("presentation", "model_and_diag", "plots", "comp_agedat__aggregated_across_time_7-10.png"))
r4ss::SSplotComps(
  replist = model_output,
  subplots = 9,
  kind = "AGE", 
  print = TRUE,
  plotdir = here::here("presentation", "model_and_diag",  "plots"),
  maxrows = 1,
  pwidth = 6,
  pheight = 5
)

#===============================================================================
# Recruitment Index
#===============================================================================

data <- model_output$cpue |>
  dplyr::filter(Fleet == 11) |>
  dplyr::mutate(
    Survey = "Recruitment Index",
    year = Yr,
    est = Obs,
    sd = SE_input,
    lwr = qnorm(0.025, mean = est, sd = sd),
    upr = qnorm(0.975, mean = est, sd = sd)
  )

model_est <- model_output$cpue |>
  dplyr::filter(Fleet == 11) |>
  dplyr::select(Yr, Exp) |>
  dplyr::rename(
    year = Yr, 
    est = Exp) |>
  dplyr::mutate(
    Survey = "Catchability * Rec. Dev."
  )

rec_est <- model_output$cpue |>
  dplyr::filter(Fleet == 11) |>
  dplyr::select(Yr, Vuln_bio) |>
  dplyr::rename(
    year = Yr, 
    dev = Vuln_bio) |>
  dplyr::mutate(
    Survey = "Rec. Dev."
  )

ggplot2::ggplot(
  data = data,
  ggplot2::aes(
    x = year,
    y = est,
    group = Survey,
    colour = Survey,
    fill = Survey
  )
) +
  ggplot2::geom_point(size = 3) +
  #ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = lwr, ymax = upr),
    linewidth = 1
  ) +
  ggplot2::ylim(c(-4, 4)) +
  ggplot2::geom_point(data = model_est, ggplot2::aes(x = year, y = est), color = "#008DA8", size = 4, shape = 17) +
  ggplot2::geom_line(data = model_est, ggplot2::aes(x = year, y = est), color = "#008DA8", linewidth = 1) +
  ggplot2::geom_point(data = rec_est, ggplot2::aes(x = year, y = dev), color = "grey", size = 4, shape = 15) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    #legend.position = c(0.80, 0.15),
    legend.title = ggplot2::element_text(size = 15), 
    legend.text = ggplot2::element_text(size = 15),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  ) +
  nmfspalette::scale_color_nmfs(palette = "oceans", reverse = TRUE) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Recruitment Deviation") +
  ggplot2::expand_limits(y = 0)
ggplot2::ggsave(filename = here::here("presentation", "model_and_diag", "plots", "recruitment_index_fit.png"), width = 10, height = 7)
