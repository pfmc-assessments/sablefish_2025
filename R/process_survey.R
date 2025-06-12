#' Process survey data
#'
#' Process raw survey data into useable objects and save tables and figures
#' to data-processed, assessment/52tables, and assessment/53figures.
#' 
#' @param  catch_data Survey catch data frame
#' @param  bds_data Survey bds data fram
#' @param  strata Survey specific stratification
#' @param  age_bins Age bins
#' @param  length_bins Length bins
#' @param  save_dir Directory to save processed data for the model. Default is 
#' "data-processed"
#' @param  fig_table_dir Directory to save figures and diagnostic tables.
#' Default is `here::here("data-raw", "survey", "trawl")`
#'
#' @details # Steps
#' 1. Create a table with number and proportion of positive tows, number of
#'    lengths, and number of ages by project and year.
#' 1. Calculate the design-based indices.
#' 1. Calculate marginal length- and age-composition data for sexed and unsexed
#'    fish. For 2021 and previous assessments, the unsexed fish were assigned
#'    to sex using the sex ratio calculated in the second stage of the
#'    expansion. The code now calculates marginals for sexed and then unsexed
#'    fish as two separate data frames. The marginal ages for the WCGBTS are
#'    not actually used in the model.
#' 1. Calculate the conditional-composition data, where these ages are
#'    conditional on a length bin.
#' 
#' @export
#' 
process_survey <- function(
  catch_data,
  bds_data,
  strata,
  age_bins = age_bins,
  length_bins = len_bins,
  save_dir = "data-processed",
  fig_table_dir = here::here("data-raw", "survey", "trawl")
  ) {
  fs::dir_create(save_dir)
  fs::dir_create(fig_table_dir)
  
  survey_name <- recode_project(unique(catch_data$Project), gls = FALSE)
  if (survey_name %in% c("triennial")) {
    survey_name <- ifelse(
      1980 %in% unique(catch_data[, "Year"]),
      "triennial-early",
      "triennial-late")
  }
  #============================================================================
  # Number of positive tows and biological samples by year and project
  #============================================================================
  if (survey_name %in% c("wcgbt", "nwfscslope")) {
    samples <- dplyr::full_join(
      x = catch_data |>
        dplyr::group_by(Project, Year) |>
        dplyr::summarise(
          `N Tows` = dplyr::n(),
          Positive = sum(total_catch_numbers > 0),
          `Proportion Positive` = round(Positive/length(Year), 3)
        ),
      y = bds_data |>
        dplyr::group_by(Project, Year) |>
        dplyr::summarise(
          `Sampled Tows Lengths` = length(unique(Trawl_id[!is.na(Length_cm)])),
          `N Lengthed` = sum(!is.na(Length_cm)),
          `Sampled Tows Ages` = length(unique(Trawl_id[!is.na(Age)])),
          `N Aged` = sum(!is.na(Age))
        ),
      by = c("Project", "Year")
    ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Project = recode_project_doc(Project, gls = FALSE),
      ) |>
      dplyr::rename(
        Survey = "Project",
        `Positive Tows` = "Positive"
      ) |>
      as.data.frame()    
  } else {
    samples <- dplyr::full_join(
      x = catch_data |>
        dplyr::group_by(Year) |>
        dplyr::summarise(
          `N Tows` = dplyr::n(),
          Positive = sum(total_catch_numbers > 0),
          `Proportion Positive` = round(Positive/length(Year), 3)
        ),
      y = bds_data$length_data |>
        dplyr::group_by(Year) |>
        dplyr::summarise(
          `Sampled Tows Lengths` = length(unique(Trawl_id)),
          `N Lengthed` = sum(!is.na(Length_cm)),
          `Sampled Tows Ages` = 0,
          `N Aged` = 0
        ),
      by = c("Year")
    ) |>
      dplyr::ungroup() |>
      dplyr::rename(
        `Positive Tows` = "Positive"
      ) |>
      as.data.frame() 
    
    ages_samples <- bds_data$age_data |>
      dplyr::filter(!is.na(Age)) |>
      dplyr::group_by(Year) |>
      dplyr::summarise(
        `Sampled Tows Ages` = length(unique(Trawl_id)),
        `N Aged` = sum(!is.na(Age))
      ) |>
      as.data.frame()
    find <- which(samples[, "Year"] %in% ages_samples[, "Year"])
    samples[find, "N Aged"] <- ages_samples[, "N Aged"]
    samples[find, "Sampled Tows Ages"] <- ages_samples[, "Sampled Tows Ages"]
  }
  
  utils::write.csv(
    samples,
    file = here::here(save_dir, paste0("data-survey-", survey_name,"-n.csv")),
    row.names = FALSE
  )

  #============================================================================
  # Design-based index of abundance using strata
  #============================================================================
  biomass <- nwfscSurvey::get_design_based(
    data = catch_data,
    strata = strata
  )
  utils::write.csv(
    biomass$biomass,
    file = file.path(fig_table_dir, paste0("design-based-index-", survey_name, ".csv")),
    row.names = FALSE
  )

  # These functions do not allow you to change the name of the saved
  # file, well the first one does but not without also adding a title to the
  # saved figure which is undesirable. We could rename the figure after
  # using the function.
  nwfscSurvey::plot_index(
    data = biomass,
    dir = fig_table_dir,
    add_save_name =  survey_name
  )

  #============================================================================
  # Marginal length- and age-composition data
  #============================================================================
  if (survey_name %in% c("afscslope", "triennial-early", "triennial-late")) {
    bds_length <- bds_data$length_data
    bds_age <- bds_data$age_data |> dplyr::filter(Sex != "U")
  } else {
    bds_length <- bds_age <- bds_data
  }
  compositions <- nwfscSurvey::get_expanded_comps(
    bio_data = bds_length,
    catch_data = catch_data,
    comp_bins = length_bins,
    comp_column_name = "length_cm",
    strata = strata,
    fleet = recode_fleet_cw(x = unique(bds_length$Project)),
    month = 7,
    verbose = FALSE
  )
  if (dim(compositions$unsexed)[1] > 1) {
    comps_out <- bind_compositions(compositions)
  } else {
    comps_out <- compositions$sexed
  }
  utils::write.csv(
    comps_out,
    file = here::here(save_dir, paste0("data-survey-comps-lengths-", survey_name, ".csv")),
    row.names = FALSE
  )
  
  #nwfscSurvey::plot_comps(
  #  data = compositions$sexed,
  #  add_save_name = survey_name,
  #  dir = fig_table_dir
  #)
  #if ("unsexed" %in% names(compositions)) {
  #  nwfscSurvey::plot_comps(
  #    data = compositions$unsexed,
  #    add_save_name = survey_name,
  #    dir = fig_table_dir
  #  )
  #}
  
  # Marginal ages
  compositions <- nwfscSurvey::get_expanded_comps(
    bio_data = bds_age,
    catch_data = catch_data,
    comp_bins = age_bins,
    comp_column_name = "age",
    strata = strata,
    fleet = recode_fleet_cw(x = unique(bds_age$Project)),
    month = 7,
    ageerr = 1,
    verbose = FALSE
  )
  if (dim(compositions$unsexed)[1] > 1) {
    comps_out <- bind_compositions(compositions)
  } else {
    comps_out <- compositions$sexed
  }
  
  utils::write.csv(
    comps_out,
    file = here::here(save_dir, paste0("data-survey-comps-ages-", survey_name, ".csv")),
    row.names = FALSE
  )
  
  #nwfscSurvey::plot_comps(
  #  data = compositions$sexed,
  #  add_save_name = survey_name,
  #  dir = fig_table_dir
  #)
  #if ("unsexed" %in% names(compositions)) {
  #  nwfscSurvey::plot_comps(
  #    data = compositions$unsexed,
  #    add_save_name = survey_name,
  #    dir = fig_table_dir
  #  )
  #}
  
  #=============================================================================
  # CAAL age composition data
  #=============================================================================
  caal <- nwfscSurvey::get_raw_caal(
    data = bds_age,
    length_column_name = "length_cm",
    age_column_name = "age",
    len_bins = length_bins,
    age_bins = age_bins,
    fleet = recode_fleet_cw(x = unique(bds_age$Project)),
    month = 7,
    ageerr = 1,
    partition = 0,
    verbose = FALSE
  )
  
  utils::write.csv(
    caal,
    file = here::here("data-processed", paste0("data-survey-comps-caal-", survey_name, ".csv")),
    row.names = FALSE
  )

  #=============================================================================
  # Data plots
  #=============================================================================
  if (survey_name == "wcgbt") {
    nwfscSurvey::PlotSexRatio.fn(
      dir = fig_table_dir,
      dat = bds_data,
      data.type = "age"
    ) 
  }
  
  gg <- nwfscSurvey::plot_proportion(
    data = catch_data |>
      dplyr::mutate(
        new = factor(
          cpue_kg_km2 <= 0,
          levels = c(FALSE, TRUE),
          labels = c("Present", "Absent")
        )
      ),
    column_factor = new,
    column_bin = Depth_m,
    width = 50,
    boundary = 0,
    bar_width = "equal" 
  )
  ggplot2::ggsave(
    filename = fs::path(
      fig_table_dir, "plots",
      paste0("data_survey_", survey_name, "_proportion-by-depth.png")
    ),
    width = 10,
    height = 10,
    plot = gg
  )

  gg <- nwfscSurvey::plot_proportion(
    data = bds_length |>
      dplyr::mutate(Sex = nwfscSurvey::codify_sex(Sex)),
    column_factor = Sex,
    column_bin = Depth_m,
    width = 50,
    boundary = 0,
    bar_width = "equal"
  )
  ggplot2::ggsave(
    filename = fs::path(
      fig_table_dir, "plots",
      paste0("data_survey_", survey_name, "_sex-by-depth.png")
    ),
    width = 10, 
    height = 10,
    plot = gg
  )

  gg <- nwfscSurvey::plot_proportion(
    data = catch_data |> dplyr::mutate(new = factor(cpue_kg_km2 <= 0, levels = c(FALSE, TRUE), labels = c("Present", "Absent"))),
    column_factor = new,
    column_bin = Latitude_dd,
    width = 1,
    boundary = 0,
    bar_width = "equal"
  )
  ggplot2::ggsave(
    filename = fs::path(
      fig_table_dir, "plots",
      paste0("data_survey_", survey_name, "_presence-by-lat.png")
    ),
    width = 10, 
    height = 10,
    plot = gg
  )
  if (survey_name == "wcgbt"){
    gg <- ggplot2::ggplot(
      data = bds_data |>
        dplyr::filter(
          !is.na(Age),
          Age < 2
        ) |>
        dplyr::mutate(Year = factor(Year, levels = min(Year):max(Year))),
      ggplot2::aes(x = Length_cm, y = Year, fill = as.factor(Pass))) +
      ggridges::geom_density_ridges2(alpha = 0.5,
                                     jittered_points = TRUE,
                                     point_alpha = 0.7,
                                     point_shape = 21,
                                     col = "blue")  +
      ggplot2::scale_fill_viridis_d(begin = 0, end = 0.5, name = "Pass") +
      ggplot2::theme_bw(base_size = 20) +
      ggplot2::scale_y_discrete(drop = FALSE) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 20)) +
      ggplot2::ylab("Year") + ggplot2::xlab("Length (cm)") +
      ggplot2::facet_grid(c("Age"), labeller = ggplot2::label_both)
    ggplot2::ggsave(
      filename = fs::path(
        fig_table_dir, "plots",
        "data_survey_wcgbt_young-length-by-year.png"
      ),
      width = 16, 
      height = 16,
      plot = gg
    ) 
  }
  return(invisible(TRUE))
}
