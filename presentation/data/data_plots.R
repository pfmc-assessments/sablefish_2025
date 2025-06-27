library(ggplot2)


#===============================================================================
# Growth
#===============================================================================
load(here::here("data", "data_survey_bio.rda"))
cols_to_keep <- c(
  "Project",
  "Year",
  "Sex",
  "Age_years", 
  "Length_cm", 
  "Weight_kg",
  "Depth_m", 
  "Latitude_dd"
)

survey_bio <- dplyr::bind_rows(
  data_survey_bio$nwfsc_combo |> dplyr::select(dplyr::all_of(cols_to_keep)), 
  data_survey_bio$nwfsc_slope |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$afsc_slope$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_early$age_data |> dplyr::select(dplyr::all_of(cols_to_keep)),
  data_survey_bio$triennial_late$age_data |> dplyr::select(dplyr::all_of(cols_to_keep))
) |>
  dplyr::mutate(
    state = dplyr::case_when(Latitude_dd > 46.25 ~ "WA", Latitude_dd < 42 ~ "CA", .default = "OR"),
    area = dplyr::case_when(Latitude_dd > 36 ~ "North", .default = "South")
  ) 

growth_cw <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)

growth_north <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(area == "North", Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)
growth_south <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(area == "South", Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)

growth <- dplyr::bind_rows(
  growth_cw$female_growth,
  growth_cw$male_growth,
  growth_north$female_growth,
  growth_north$male_growth,
  growth_south$female_growth,
  growth_south$male_growth
)
growth$sex <- rep(c("F", "M"), 3)
growth$area <- c(rep("Coastwide", 2), rep("North", 2), rep("South", 2))

data_to_plot <- survey_bio |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::filter(
    sex != "U",
    !is.na(age_years),
    age_years >= 0,
    !is.na(length_cm),
    length_cm > 0
  )

xlims <- c(0, ceiling(max(data_to_plot[, "age_years"])))
ylims <- c(0, max(data_to_plot[, "length_cm"]))
growth$amin <- 0
growth$amax <- xlims[2]

lines_to_plot <- growth |>
  dplyr::group_by(area, sex) |>
  dplyr::reframe(
    k = K,
    Linf = Linf,
    L0 = L0,
    age_years = seq(amin, amax, 1),
    length_cm = Linf + (L0 - Linf) * exp(-k * age_years)
  )
label <- lines_to_plot |>
  dplyr::mutate(
    max_x = quantile(age_years, 0.60),
    multiplier = ifelse(sex == "Female", 0.2, 0.1)
  ) |>
  dplyr::group_by(area, sex) |>
  dplyr::summarize(
    label = paste0(
      "k = ", round(unique(k), 2), "; ",
      paste0("Lmin = ", round(unique(L0), 1)), "; ",
      paste0("Linf = ", round(unique(Linf), 1))
    ),
    x = unique(max_x),
    y = unique(max(length_cm)) * unique(multiplier)
  )

growth <- ggplot(lines_to_plot) + 
  geom_line(aes(x = age_years, y = length_cm, color = area, linetype = sex), linewidth = 1) +
  xlim(xlims[1], xlims[2]) +
  ylim(24, 70) +
  theme_bw() +
  xlab("Age (yrs)") +
  ylab("Length (cm)") +
  scale_color_viridis_d() + 
  theme(
    legend.title=element_text(size = 16), 
    legend.text=element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16))
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "growth.png"), width = 7, height = 4)

#===============================================================================
# Maturity
#===============================================================================

load(here::here("data", "maturity_data.rda"))
north <- ggplot2::ggplot(maturity_data |> dplyr::filter(Area == "North") |>
                           dplyr::mutate(
                             Area = dplyr::case_when(latitude >= 36 ~ "North", .default = "South")), 
                         ggplot2::aes(x = age, y = functional_maturity)) +
  ggplot2::geom_point() +
  ggplot2::ylab("Functional Maturity") +
  ggplot2::geom_vline(xintercept = 7, color = "blue", linetype = 3, size = 1) + 
  ggplot2::xlab("Age years") +
  ggplot2::theme_bw() +
  ggplot2::xlim(c(0, 70) ) + 
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 16),
    strip.background = ggplot2::element_rect(fill = "white")) +
  ggplot2::annotate("text", x = 40, y = 0.75, label = "North Sample Size = 496") 

south <- ggplot2::ggplot(maturity_data |> dplyr::filter(Area == "South") |>
                           dplyr::mutate(
                             Area = dplyr::case_when(latitude >= 36 ~ "North", .default = "South")), 
                         ggplot2::aes(x = age, y = functional_maturity)) +
  ggplot2::geom_point() +
  ggplot2::ylab("Functional Maturity") +
  ggplot2::geom_vline(xintercept = 7, color = "blue", linetype = 3, size = 1) + 
  ggplot2::xlab("Age years") +
  ggplot2::theme_bw() + ggplot2::xlim(c(0, 70)) + 
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 16),
    strip.background = ggplot2::element_rect(fill = "white")) +
  ggplot2::annotate("text", x = 40, y = 0.75, label = "South Sample Size = 199")
fig <- cowplot::plot_grid(north, south, nrow = 1)
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "maturity_samples.png"), width = 7, height = 4)