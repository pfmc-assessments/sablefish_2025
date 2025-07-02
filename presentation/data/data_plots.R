library(ggplot2)


#===============================================================================
# r4ss plots
#===============================================================================

model_output <- r4ss::SS_output(here::here("model", "base_model", "8.36_base_model"))
r4ss::SSplotData(
  replist = model_output,
  print = TRUE,
  pwidth = 7.0,
  pheight = 7.0,
  plotdir = here::here("presentation", "data", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 4,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation", "data", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 7,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation", "data", "plots")
)
r4ss::SSplotTimeseries(
  replist = model_output,
  subplot = 9,
  forecastplot = FALSE,
  print = TRUE,
  pwidth = 5.2,
  pheight = 4,
  plotdir = here::here("presentation", "data", "plots")
)
r4ss::SSplotRecdevs(
  replist = model_output,
  subplots = 1:2,
  dir = here::here("presentation", "data", "plots")
)
r4ss::SSplotIndices(
  replist = model_output,
  suplots = 1,
  print = TRUE,
  plotdir = here::here("presentation", "data", "plots")
)
r4ss::SS_plots(
  replist = model_output, 
  plot = 1, 
  minyr = 2000, 
  maxyr = 2025,
  ptsize = 12,
  pheight_tall = 3.5, 
  pwidth = 6.5,
  dir = here::here("presentation", "data")
)

all_files <- list.files(here::here("presentation", "data", "plots"))
remove <- c(grep(".html", all_files), grep(".csv", all_files), grep(".txt", all_files))
file.remove(here::here("presentation", "data", "plots", all_files[remove]))
save(model_output, file = here::here("presentation", "data", "tables", "model_output.rda"))

#===============================================================================
# Distribution
#===============================================================================

p <- nwfscSurvey::plot_proportion(
  data = data_survey_catch$nwfsc_combo |> 
    dplyr::mutate(
      new = factor(cpue_kg_km2 <= 0, levels = c(FALSE, TRUE), labels = c("Present", "Absent"))),
  column_factor = new,
  column_bin = Latitude_dd,
  width = 1,
  boundary = 0,
  bar_width = "equal"
) 
p + 
  nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::geom_hline(yintercept = 0.5, color = "white", linetype = 2, size = 1) +
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 20),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20))
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "distribution_latitude.png"), width = 10, height = 8)

p <- nwfscSurvey::plot_proportion(
  data = data_survey_catch$nwfsc_combo |>
    dplyr::mutate(
      new = factor(
        cpue_kg_km2 <= 0,
        levels = c(FALSE, TRUE),
        labels = c("Present", "Absent")
      )
    ),
  column_factor = new,
  column_bin = Depth_m,
  width = 100,
  boundary = 0,
  bar_width = "equal" 
)
p + 
  nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::geom_hline(yintercept = 0.5, color = "white", linetype = 2, size = 1) +
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 20),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20))
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "distribution_depth.png"), width = 10, height = 8)

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


maturity_at_age <- readRDS(
  here::here("quarto_website", "figures", "ogives_for_assessment.rds")
)
ggplot2::ggplot(maturity_at_age, ggplot2::aes(x = age, y = p, color = Model)) +
  ggplot2::geom_line(linewidth = 2) +
  ggplot2::ylab("Probability Mature") +
  ggplot2::xlab("Age years") + ggplot2::xlim(c(0, 50)) + 
  nmfspalette::scale_color_nmfs(palette = "waves", reverse = TRUE) + 
  ggplot2::theme_bw() +
  ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white")) +
  ggplot2::theme(
    legend.title=element_text(size = 20), 
    legend.text = ggplot2::element_text(size = 20),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20))
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "maturity_at_age.png"), width = 7, height = 4)

#===============================================================================
# Removals
#===============================================================================

landings <- model_output$catch |> 
  dplyr::mutate(
    year = Yr,
    catch_mt = dead_bio,
    Fleet = dplyr::case_when(
      Fleet_Name == "TWL_Discards" ~ "TWL Discards",
      Fleet_Name == "HKL_Discards" ~ "HKL Discards",
      Fleet_Name == "Pot_Discard" ~ "Pot Discards",
      .default = Fleet_Name)
  )

ggplot2::ggplot(landings, ggplot2::aes(x = year, y = catch_mt, fill = Fleet)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Removals (mt)") +
  ggplot2::xlim(c(1890, 2025)) + 
  ggplot2::scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  #nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = c(0.20, 0.65),
    legend.title=element_text(size = 16), 
    legend.text = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  )

ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "removals.png"), width = 8, height = 4)

#===============================================================================
# Discard Rates
#===============================================================================

discard_rates <- read.csv(here::here("data-processed", "data_commercial_discard_rates.csv")) |>
  dplyr::filter(year > 2000) |>
  dplyr::mutate(
    fleet = dplyr::case_when(
      fleet %in% c(1, 4) ~ "Trawl",
      fleet %in% c(2, 5) ~ "Hook-and-Line",
      .default = "Pot")
  ) |>
  dplyr::rename(
    Year = year,
    Fleet = fleet
  )


ggplot2::ggplot(discard_rates, ggplot2::aes(x = Year, y = discard_rate, color = Fleet, shape = Fleet)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_line(linewidth = 1) + 
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Discard Rates") +
  ggplot2::xlim(c(2002, 2023)) + 
  ggplot2::ylim(c(0, 0.65)) + 
  nmfspalette::scale_color_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = c(0.70, 0.73),
    legend.title = ggplot2::element_text(size = 12), 
    legend.text = ggplot2::element_text(size = 12),
    axis.text = ggplot2::element_text(size = 16),
    axis.title = ggplot2::element_text(size = 16)
  )
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "wcgop_discard_rates.png"), width = 6, height = 3)

#===============================================================================
# Fishery Ages
#===============================================================================

age_samples <- data_commercial_bds |>
  dplyr::filter(!is.na(age_years)) |>
  dplyr::group_by(year, geargroup) |>
  dplyr::summarise(
    Count = dplyr::n()
  ) |>
  dplyr::rename(Fleet = geargroup)

ggplot2::ggplot(age_samples, ggplot2::aes(x = year, y = Count, fill = Fleet)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Count") +
  ggplot2::xlim(c(1982, 2025)) + 
  nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  #ggplot2::scale_fill_viridis_d() +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = c(0.90, 0.80),
    legend.title=element_text(size = 16), 
    legend.text = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  )
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "fishery_age_samples.png"), width = 8, height = 4)


load("C:/Assessments/wcgop/data_2023/sablefish/raw_discard_lengths/raw_wcgop_discard_comps_w_estimated_ages.Rdata")
a <- ggplot2::ggplot(wcgop_age_est[!is.na(wcgop_age_est$age), ], ggplot2::aes(x = age, y = length, color = gear_groups, shape = gear_groups)) +
  ggplot2::geom_point() +
  ggplot2::ylim(c(0, 85)) +
  nmfspalette::scale_color_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::ylab("Length (cm)") + ggplot2::xlab("Estimated Age") + 
  ggplot2::theme_bw() + 
  ggplot2::theme(
    strip.text.x = ggplot2::element_text(size = 20),
    #strip.background = ggplot2::element_rect(colour="white", fill="white"),
    #legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = "none",
    #legend.title=element_text(size = 16), 
    #legend.text = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  ) +
  facet_wrap("gear_groups", nrow = 1)
ggplot2::ggsave(a,
       file = here::here("presentation", "data", "plots", "compare_data_alk_all_years.png"),
       height = 7, width = 12)

discard_age_samples <- read.csv(here::here("data-processed", "data-discard-sample-sizes-length.csv")) |>
  dplyr::group_by(year, gear_groups) |>
  dplyr::summarise(
    gear_groups = dplyr::case_when(
      gear_groups == "trawl" ~ "Trawl Discard",
      gear_groups == "hook-and-line" ~ "HKL Discard",
      .default = "Pot Discard"
    ),
    Count = sum(fish)
  ) |>
  dplyr::rename(Fleet = gear_groups)

ggplot2::ggplot(discard_age_samples, ggplot2::aes(x = year, y = Count, fill = Fleet)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Count") +
  ggplot2::xlim(c(2002, 2023)) +  ggplot2::ylim(c(0, 8000)) + 
  nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  #ggplot2::scale_fill_viridis_d() +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = c(0.20, 0.80),
    legend.title=element_text(size = 16), 
    legend.text = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  )
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "fishery_discard_age_length_samples.png"), width = 8, height = 4)


r4ss::SSplotComps(
  replist = model_output,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  datonly = TRUE,
  plotdir = here::here("presentation", "data", "plots"),
  fleets = 1:3,
  fleetnames = c("Trawl", "Hook-and-Line", "Pot"),
  maxrows = 1,
  pwidth = 8,
  pheight = 4
)
file.rename(here::here("presentation", "data", "plots", "comp_agedat__aggregated_across_time.png"),
            here::here("presentation", "data", "plots", "comp_agedat__aggregated_across_time_1-3.png"))

r4ss::SSplotComps(
  replist = model_output,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  datonly = TRUE,
  plotdir = here::here("presentation", "data", "plots"),
  fleets = 4:6,
  #fleetnames = c("TWL_Discard", "HKL_Discard", "Pot_Discard"),
  maxrows = 1,
  pwidth = 8,
  pheight = 4
)
file.rename(here::here("presentation", "data", "plots", "comp_agedat__aggregated_across_time.png"),
            here::here("presentation", "data", "plots", "comp_agedat__aggregated_across_time_4-6.png"))

#===============================================================================
# Survey ages
#===============================================================================
model_mod <- model_output
model_mod$agedbase <- model_mod$agedbase |> 
  dplyr::filter(Sexes == 3)
r4ss::SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE", 
  print = TRUE,
  datonly = TRUE,
  plotdir = here::here("presentation", "data", "plots"),
  fleets = 7:10,
  #fleetnames = c("TWL_Discard", "HKL_Discard", "Pot_Discard"),
  maxrows = 2,
  pwidth = 8,
  pheight = 4
)
file.rename(here::here("presentation", "data", "plots", "comp_agedat__aggregated_across_time.png"),
            here::here("presentation", "data", "plots", "comp_agedat__aggregated_across_time_survey.png"))


age_samples <- data_survey_bio$nwfsc_combo |>
  dplyr::filter(!is.na(Age_years)) |>
  dplyr::group_by(Year, Sex) |>
  dplyr::summarise(
    Count = dplyr::n()
  ) |>
  dplyr::mutate(
    Sex = dplyr::case_when(
      Sex == "F" ~ "Female", Sex == "M" ~ "Male", .default = "Unsexed"
    )
  )

ggplot2::ggplot(age_samples, ggplot2::aes(x = Year, y = Count, fill = Sex)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Count") +
  nmfspalette::scale_fill_nmfs(palette = "oceans", reverse = TRUE) +
  ggplot2::xlim(c(2003, 2025)) +
  #ggplot2::scale_fill_viridis_d() +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.01, "cm"),
    legend.position = c(0.30, 0.83),
    legend.title = element_text(size = 14), 
    legend.text = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  )
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "wcgbt_age_samples.png"), width = 8, height = 4)

age_samples <- data_survey_bio$nwfsc_slope |>
  dplyr::filter(!is.na(Age_years)) |>
  dplyr::group_by(Year, Sex) |>
  dplyr::summarise(
    Count = dplyr::n(),
    Survey = "NWFSC Slope Survey"
  ) |>
  dplyr::mutate(
    Sex = dplyr::case_when(
      Sex == "F" ~ "Female", Sex == "M" ~ "Male", .default = "Unsexed"
    )
  ) |>
  dplyr::mutate(
    Count = dplyr::case_when(
      Sex == "Unsexed" ~ 0, .default = Count
    )
  )

ggplot2::ggplot(age_samples, ggplot2::aes(x = Year, y = Count, fill = Sex)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Count") +
  #ggplot2::ylim(c(0, 1600)) + 
  nmfspalette::scale_fill_nmfs(palette = "oceans", reverse = TRUE) +
  ggplot2::theme(
    strip.text.x = element_text(size = 14),
    #strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.01, "cm"),
    legend.position = c(0.20, 0.83),
    legend.title = element_text(size = 14), 
    legend.text = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  ) + ggplot2::facet_wrap(~Survey)
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "nwfsc_slope_age_samples.png"), width = 5, height = 4)

age_samples <- dplyr::bind_rows(
  data_survey_bio$triennial_early$age_data,
  data_survey_bio$triennial_late$age_data) |>
  dplyr::filter(!is.na(Age_years)) |>
  dplyr::group_by(Year, Sex) |>
  dplyr::summarise(
    Count = dplyr::n(),
    Survey = "Triennial Survey"
  ) |>
  dplyr::mutate(
    Sex = dplyr::case_when(
      Sex == "F" ~ "Female", Sex == "M" ~ "Male", .default = "Unsexed"
    ),
    Count = dplyr::case_when(
      Sex == "Unsexed" ~ 0, .default = Count
    )
  ) |>
  dplyr::filter(Year %in% c(1983, 1989, 1992, 1995, 2004))

ggplot2::ggplot(age_samples, ggplot2::aes(x = Year, y = Count, fill = Sex)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Count") +
  ggplot2::ylim(c(0, 1600)) + 
  nmfspalette::scale_fill_nmfs(palette = "oceans", reverse = TRUE) +
  ggplot2::theme(
    strip.text.x = element_text(size = 14),
    #strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.01, "cm"),
    legend.position = c(0.80, 0.83),
    legend.title = element_text(size = 14), 
    legend.text = ggplot2::element_text(size = 14),
    axis.text = ggplot2::element_text(size = 20),
    axis.title = ggplot2::element_text(size = 20)
  ) + ggplot2::facet_wrap(~Survey)
ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "triennial_age_samples.png"), width = 5, height = 4)
