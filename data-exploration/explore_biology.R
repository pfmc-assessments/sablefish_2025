library(ggplot2)

load(here::here("data", "data_survey_bio.rda"))

data_survey_bio$afsc_slope$length_data$Age_years <- NA
data_survey_bio$triennial$length_data$Age_years <- NA
data_survey_bio$afsc_slope$age_data$Project <- paste0(data_survey_bio$afsc_slope$length_data$Project[1], "-Age")
data_survey_bio$triennial$age_data$Project <- paste0(data_survey_bio$triennial$length_data$Project[1], "-Age")

cols_to_keep <- c("Year", "Project", "Depth_m", "Latitude_dd", "Longitude_dd", "Common_name",
                  "Length_cm", "Age_years", "Weight_kg", "Sex", "Tow", "Vessel", "Date", "Pass")

survey_bio <- dplyr::bind_rows(
  data_survey_bio$nwfsc_combo[, cols_to_keep],
  data_survey_bio$nwfsc_slope[, cols_to_keep],
  data_survey_bio$afsc_slope$length_data[, cols_to_keep],
  data_survey_bio$afsc_slope$age_data[, cols_to_keep],
  data_survey_bio$triennial$length_data[, cols_to_keep],
  data_survey_bio$triennial$age_data[, cols_to_keep]
) |>
  dplyr::mutate(
    state = dplyr::case_when(Latitude_dd >= 46.25 ~ "WA",
                             Latitude_dd < 42.0 ~ "CA",
                             .default = "OR"),
    area = dplyr::case_when(Latitude_dd >= 36.0 ~ "N",
                            .default = "S")
  )

#=====================================================================
# Estimate growth
#=====================================================================
growth_cw <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)
growth_wa <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(state == "WA", Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)
growth_or <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(state == "OR", Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)
growth_nca <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(Latitude_dd >= 36, Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)

growth <- dplyr::bind_rows(
  growth_cw$female_growth,
  growth_cw$male_growth,
  growth_wa$female_growth,
  growth_wa$male_growth,
  growth_or$female_growth,
  growth_or$male_growth,
  growth_nca$female_growth,
  growth_nca$male_growth,
  growth_sca$female_growth,
  growth_sca$male_growth
)
growth$sex <- rep(c("F", "M"), 5)
growth$area <- c(rep("Coastwide", 2), rep("WA", 2), rep("OR", 2), rep("NCA", 2), rep("SCA", 2))

utils::write.csv(
  growth,
  file = here::here("data-raw", "biology", "growth.csv"),
  row.names = FALSE
)

#=====================================================================
# Weight-length estiamtes
#=====================================================================
weight_length_estimates <- nwfscSurvey::estimate_weight_length(
  survey_bio,
  verbose = FALSE
)
weight_length_estimates_north <- nwfscSurvey::estimate_weight_length(
  survey_bio |> dplyr::filter(Latitude_dd > 36.0),
  verbose = FALSE
)
weight_length_estimates_south <- nwfscSurvey::estimate_weight_length(
  survey_bio |> dplyr::filter(Latitude_dd <= 36.0),
  verbose = FALSE
)
weight_length <- dplyr::bind_rows(
  weight_length_estimates[1:2, ],
  weight_length_estimates_north[1:2, ],
  weight_length_estimates_south[1:2, ]
) |> dplyr::mutate(
  Sex = dplyr::case_when(sex == "female" ~ "F", .default = "M")
)
weight_length$area <- c(
  rep("Coastwide", 2),
  rep("North", 2),
  rep("South", 2)
)

utils::write.csv(
  weight_length_estimates,
  file = here::here("data-raw", "biology", "weight_length_area.csv"),
  row.names = FALSE
)

#=====================================================================
# Weight-age estimates
#=====================================================================
ave_weight_age_area <- survey_bio |>
  dplyr::filter(!is.na(Age_years), !is.na(Weight_kg), Sex != "U") |>
  dplyr::group_by(Sex, Age_years, area) |>
  dplyr::summarise(
    n = dplyr::n(),
    ave_weight = mean(Weight_kg)
  )

survey_bio |> 
  dplyr::filter(!is.na(Age_years), !is.na(Weight_kg), Sex != "U") |>
  dplyr::group_by(Project, area, Sex) |>
  dplyr::summarise(n = dplyr::n())

ggplot(ave_weight_age_area, aes(x = Age_years, y = ave_weight, color = area)) +
  geom_point() +
  facet_wrap(Sex ~.)

ave_weight_age <- survey_bio |>
  dplyr::filter(!is.na(Age_years), !is.na(Weight_kg), Sex != "U") |>
  dplyr::group_by(Sex, Age_years) |>
  dplyr::summarise(
    n = dplyr::n(),
    ave_weight = mean(Weight_kg)
  )

ggplot(ave_weight_age, aes(x = Age_years, y = ave_weight, color = Sex)) +
  geom_point()


#==============================================================================
# Plots
#==============================================================================

ggplot(survey_bio |> 
         dplyr::filter(!is.na(Length_cm),
                       !Project %in% 
                         c("Groundfish Triennial Shelf Survey-Age",
                           "AFSC/RACE Slope Survey-Age-Age")), 
       aes(x = Length_cm, color = state)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 2)) + 
  facet_grid(Project~.)

ggplot(survey_bio |> dplyr::filter(!is.na(Age_years)), 
       aes(x = Age_years, color = state)) + 
  geom_density(size = 2) + 
  xlim(c(0, 30)) + 
  scale_color_viridis_d() + 
  xlab("Age") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 2)) + 
  facet_grid(Project~.)

ggplot(survey_bio |> 
         dplyr::filter(!is.na(Age_years), !is.na(Length_cm), Sex != "U"), 
       aes(x = Age_years, y = Length_cm, color = state)) + 
  geom_point() +  
  scale_color_viridis_d() + 
  xlab("Age") + ylab("Length (cm)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 15)) + 
  facet_grid(Sex~.)

data_to_plot <- survey_bio |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::filter(
    sex != "U",
    !is.na(weight_kg),
    weight_kg > 0,
    !is.na(length_cm),
    length_cm > 0
  )

xlims <- c(0, 90)
ylims <- c(0, 8.5)
weight_length_estimates$lmin <- 0
weight_length_estimates$lmax <- xlims[2]

lines_to_plot <- weight_length_estimates |>
  dplyr::filter(sex != "all") |>
  dplyr::group_by(sex) |>
  dplyr::reframe(
    length_cm = seq(lmin, lmax, 1),
    weight_kg = A * length_cm^B,
    a = A,
    b = B
  )

label <- lines_to_plot |>
  dplyr::filter(sex != "all") |>
  dplyr::mutate(
    max_y = quantile(weight_kg, 0.95),
    multiplier = ifelse(sex == "female", 1, 0.9)
  ) |>
  dplyr::group_by(sex) |>
  dplyr::summarize(
    label = paste0("a = ", format(unique(a), digits = 3, scientific = TRUE), "; ", paste0("b = ", round(unique(b), 2))),
    x = quantile(length_cm, 0.30),
    y = unique(max_y) * unique(multiplier)
  )

colors <- c("#414487FF", "#414487FF", "#22A884FF", "#22A884FF")
p1 <- ggplot2::ggplot(data_to_plot) +
  ggplot2::geom_point(aes(x = length_cm, y = weight_kg, color = sex), alpha = 0.15, size = 1) +
  ggplot2::ylab("Weight (kg)") +
  ggplot2::xlab("Length (cm)") +
  ggplot2::geom_line(
    data = lines_to_plot,
    ggplot2::aes(x = length_cm, y = weight_kg, color = sex, linetype = sex), linewidth = 1.5
  ) +
  ggplot2::geom_text(data = label, 
    ggplot2::aes(x = x, y = y, label = label, color = sex), size = 6) +
  ggplot2::xlim(xlims[1], xlims[2]) +
  ggplot2::ylim(ylims[1], ylims[2]) +
  ggplot2::theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "right") +
  ggplot2::scale_color_manual(name = "sex", values = colors) +
  ggplot2::scale_fill_manual(name = "sex", values = colors) +
  ggplot2::guides(color = guide_legend(override.aes = list(alpha = 1))) 
ggplot2::ggsave(
  p1,
  filename = here::here("data-raw", "biology", "weight_length_ests.png"),
  height = 7, width = 7
)

#===============================================================================
# Plot length-at-age
#===============================================================================
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

ggplot(lines_to_plot) +
 geom_line(aes(x = age_years, y = length_cm, color = area, linetype = sex), linewidth = 1) +
 xlim(xlims[1], xlims[2]) +
 ylim(24, 75) +
 theme_bw() +
 xlab("Age (yrs)") +
 ylab("Length (cm)") +
 scale_color_viridis_d() + 
 theme(axis.text = element_text(size = 13),
       axis.title = element_text(size = 13))
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "length_age_area.png"),
  height = 7, width = 7
)

#===============================================================================
# Plot weight-at-length
#===============================================================================
data_to_plot <- survey_bio |>
  dplyr::rename_with(
    tolower
  ) |>
  dplyr::filter(
    sex != "U",
    !is.na(weight_kg),
    !is.na(length_cm),
    length_cm > 0
  )

xlims <- c(0, ceiling(max(data_to_plot[, "length_cm"])))
ylims <- c(0, max(data_to_plot[, "weight_kg"]))
weight_length$lmin <- 0
weight_length$lmax <- xlims[2]

lines_to_plot <- weight_length |>
  dplyr::group_by(area, Sex) |>
  dplyr::reframe(
    length_cm = seq(0, 98, 1),
    weight_kg = A * length_cm^B,
    a = A,
    b = B
  )
label <- lines_to_plot |>
  dplyr::mutate(
    max_y = quantile(weight_kg, 0.95),
    multiplier = ifelse(Sex == "F", 1, 0.9)
  ) |>
  dplyr::group_by(area, Sex) |>
  dplyr::summarize(
    label = paste0("a = ", format(unique(a), digits = 3, scientific = TRUE), "; ", paste0("b = ", round(unique(b), 2))),
    x = quantile(length_cm, 0.30),
    y = unique(max_y) * unique(multiplier)
  )
ggplot2::ggplot(lines_to_plot) +
  #ggplot2::geom_point(aes(x = length_cm, y = weight_kg, color = sex), alpha = 0.15, size = 1) +
  ggplot2::ylab("Weight (kg)") +
  ggplot2::xlab("Length (cm)") +
  ggplot2::ylim(0, 9) + ggplot2::xlim(0, 90) +
  ggplot2::geom_line(
    ggplot2::aes(x = length_cm, y = weight_kg, color = area, linetype = Sex), linewidth = 1
  ) +
  scale_color_viridis_d() + 
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "length_age_area.png"),
  height = 7, width = 7
)

#===============================================================================
# Plot age by latitude and depth
#===============================================================================
survey_bio$latitude <- plyr::round_any(
  survey_bio$Latitude_dd, 1
)
ggplot(survey_bio |> dplyr::filter(!is.na(Age_years)),
       aes(x = Latitude_dd, y = Age_years, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
  theme_bw() +
  ylab("Age (yrs)") +
  xlab("Latitude") +
  scale_color_viridis_d() + 
  facet_wrap("Project")
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "latitude_age.png"),
  height = 7, width = 12
)

ggplot(survey_bio |> dplyr::filter(!is.na(Length_cm),
       !Project %in% c("Groundfish Triennial Shelf Survey-Age", "AFSC/RACE Slope Survey-Age")), 
       aes(x = Latitude_dd, y = Length_cm, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2, se = FALSE) +
  theme_bw() +
  ylab("Length (cm)") +
  xlab("Latitude") +
  scale_color_viridis_d() + 
  facet_wrap("Project")

ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "latitude_length.png"),
  height = 7, width = 12
)

ggplot(survey_bio |> dplyr::filter(!is.na(Length_cm),
       !Project %in% c("Groundfish Triennial Shelf Survey-Age", "AFSC/RACE Slope Survey-Age")), 
       aes(x = Latitude_dd, y = Length_cm, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2, se = FALSE) +
  theme_bw() +
  ylab("Length (cm)") +
  xlab("Latitude") +
  scale_color_viridis_d() + 
  facet_wrap("Project")

ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "latitude_length.png"),
  height = 7, width = 12
)

ggplot(survey_bio |> dplyr::filter(!is.na(Age_years)),
       aes(x = Depth_m, y = Age_years, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
  theme_bw() +
  ylab("Age (yrs)") +
  xlab("Depth (m)") +
  scale_color_viridis_d() + 
  facet_wrap("Project")

ggplot(survey_bio |> dplyr::filter(!is.na(Age_years), Age_years < 6),
       aes(x = Depth_m, y = Age_years, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
  ylim(-1, 6) + 
  theme_bw() +
  ylab("Age (yrs)") +
  xlab("Depth (m)") +
  scale_color_viridis_d() + 
  facet_wrap("Project")
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "depth_age_0-5.png"),
  height = 7, width = 12
)
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "depth_age.png"),
  height = 7, width = 12
)

ggplot(survey_bio |> dplyr::filter(!is.na(Length_cm), 
                                   !Project %in% c("Groundfish Triennial Shelf Survey-Age", "AFSC/RACE Slope Survey-Age")),
       aes(x = Depth_m, y = Length_cm, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2, se = FALSE) +
  theme_bw() +
  ylab("Age (yrs)") +
  xlab("Length (cm)") +
  scale_color_viridis_d() + 
  facet_wrap("Project")
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "depth_length.png"),
  height = 7, width = 12
)


ggplot(survey_bio |> dplyr::filter(!is.na(Age_years)),
       aes(x = Depth_m, y = Age_years, color = Sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
  theme_bw() +
  ylab("Age (yrs)") +
  xlab("Depth (m)") +
  scale_color_viridis_d() + 
  facet_wrap("Project")

nwfscSurvey::plot_bio_patterns(
  dir = here::here("data-raw", "biology"),
  bio = survey_bio |> dplyr::filter(Project == "Groundfish Slope and Shelf Combination Survey")
)

nwfscSurvey::plot_bio_patterns(
  dir = here::here("data-raw", "biology"),
  bio = survey_bio |> dplyr::filter(Project == "Groundfish Slope and Shelf Combination Survey"),
  col_name = "Age_years"
)

nwfscSurvey::plot_proportion(
  data = survey_bio |>
    dplyr::filter(Project == "Groundfish Slope and Shelf Combination Survey") |>
    dplyr::mutate(Sex = nwfscSurvey::codify_sex(Sex)),
  column_factor = Sex,
  column_bin = Depth_m,
  width = 100,
  boundary = 0,
  bar_width = "equal"
)
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "depth_sex.png"),
  height = 7, width = 7
)
nwfscSurvey::plot_proportion(
  data = data_survey_catch$nwfsc_combo |>
    dplyr::mutate(new = factor(
         cpue_kg_km2 <= 0,
         levels = c(FALSE, TRUE),
         labels = c("Present", "Absent")
       )),
  column_factor = new,
  column_bin = Latitude_dd,
  width = 1,
  boundary = 0,
  bar_width = "equal"
)
ggplot2::ggsave(
  filename = here::here("data-raw", "biology", "presence_absence_latitude.png"),
  height = 7, width = 7
)




#===============================================================================
# Commercial Data
#===============================================================================
file_bds <- fs::dir_ls(here::here("data-raw", "bds"), regex = 
                         "PacFIN.SABL.bds.11.Dec.2024")
load(file_bds)
bds_cleaned <- cleanPacFIN(
  Pdata = bds.pacfin,
  CLEAN = TRUE,
  spp = "sablefish"
)

ggplot(bds_cleaned, 
       aes(x = lengthcm, color = state)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 2)) +
  facet_grid(geargroup~.)

ggplot(bds_cleanded, 
       aes(x = Age, color = state)) + 
  geom_density(size = 2) + 
  xlim(c(0, 30)) + 
  scale_color_viridis_d() + 
  xlab("Age)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 2)) +
  facet_grid(geargroup~.)
growth_sca <- nwfscSurvey::est_growth(
  dat = survey_bio |> 
    dplyr::filter(Latitude_dd < 36, Sex != "U") |> dplyr::mutate(Age = Age_years),
  return_df = FALSE
)