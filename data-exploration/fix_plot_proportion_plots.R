data <- data_survey_catch$nwfsc_combo |>
  dplyr::mutate(
    lat = dplyr::case_when(Latitude_dd < 33 ~ 32.1, .default = Latitude_dd),
    new = factor(
      cpue_kg_km2 <= 0,
      levels = c(TRUE, FALSE),
      labels = c("Absent", "Present")
    ),
    Latitude = plyr::round_any(lat, 1, floor)
  )


library(ggplot2)

ggplot(data = data, aes(x = factor(Latitude), fill = factor(new))) +
  geom_bar(position = "fill") +
  xlab("Latitude (N)") +
  ggplot2::ylab("Proportion") +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank()
  ) +
  ggokabeito::scale_fill_okabe_ito() +
  ggplot2::coord_cartesian(expand = FALSE)
ggsave(
  filename = here::here("report", "figures", "wcgbts_present_absense.png"),
  height = 3,
  width = 5
)


data <- data_survey_catch$nwfsc_combo |>
  dplyr::mutate(
    lat = dplyr::case_when(Latitude_dd < 33 ~ 32.1, .default = Latitude_dd),
    new = factor(
      cpue_kg_km2 <= 0,
      levels = c(TRUE, FALSE),
      labels = c("Absent", "Present")
    ),
    Depth = plyr::round_any(Depth_m, 100, floor)
  )

ggplot(data = data, aes(x = factor(Depth), fill = factor(new))) +
  geom_bar(position = "fill") +
  xlab("Depth (m)") +
  ggplot2::ylab("Proportion") +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank()
  ) +
  ggokabeito::scale_fill_okabe_ito() +
  ggplot2::coord_cartesian(expand = FALSE)
ggsave(
  filename = here::here(
    "report",
    "figures",
    "wcgbts_present_absense_depth.png"
  ),
  height = 3,
  width = 5
)

data <- data_survey_bio$nwfsc_combo |>
  dplyr::filter(Sex != "U") |>
  dplyr::mutate(
    Depth = plyr::round_any(Depth_m, 100, floor)
  )

ggplot(data = data, aes(x = factor(Depth), fill = factor(Sex))) +
  geom_bar(position = "fill") +
  xlab("Depth (m)") +
  ggplot2::ylab("Proportion") +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.5),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank()
  ) +
  ggokabeito::scale_fill_okabe_ito() +
  ggplot2::coord_cartesian(expand = FALSE)
ggsave(
  filename = here::here("report", "figures", "wcgbts_sex_ratio_depth.png"),
  height = 3,
  width = 5
)
