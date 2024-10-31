#' Process survey data
#'
#' Process raw survey data into useable objects and save tables and figures
#' to data-processed, assessment/52tables, and assessment/53figures.
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
#' @details # To Do
#' The following to do list is ordered by priority with the highest priority
#' items listed first.
#' * Create a sensitivity that changes the arguments passed to
#'   [nwfscSurvey::SurveyLFs.fn()] of `sexRatioUnsexed = 0.50`,
#'   `maxSizeUnsexed = 32`, and `sexRatioStage = 2`. Then use these marginal
#'   length compositions in the data file. Chantel suggested the following
#'   figure as well
#'   ```
#'   nwfscSurvey::PlotFreqData.fn(
#'     dir =  figure_dir,
#'     dat = lf_sex_ratio,
#'     main = "NWFSC Groundfish Bottom Trawl Survey",
#'     ylim = c(0, max(len_bins)),
#'     yaxs = "i",
#'     ylab = "Length (cm)"
#'   )
#'   ```
#' * There is a mix of saving things as wcgbts and NWFSC.Combo. Increase
#'   consistency.
#' * [PlotBio.fn()] and [PlotBioStrata.fn()] do not allow you to change
#'   the name but this function could rename the output.
#' * The current functionality attempts to do many things for each survey
#'   included in `data_survey_*` but this fails because the strata only works
#'   for the WCGBTS. So, in the end many outputs are limited to just this most
#'   recent survey. It would be nice to get the function to work for all
#'   surveys.
process_survey <- function() {
  fs::dir_create(here::here(), "data-processed")
  fs::dir_create(here::here(), table_dir)
  fs::dir_create(here::here(), figure_dir)

  #============================================================================
  # Number of positive tows and biological samples by year and project
  #============================================================================
  utils::write.csv(
    x = dplyr::full_join(
      x = data_survey_catch |>
        dplyr::group_by(Project, Year) |>
        dplyr::summarise(
          Positive = sum(total_catch_numbers > 0),
          `Proportion Positive` = round(Positive/length(Year), 3)
        ),
      y = data_survey_bio |>
        dplyr::group_by(Project, Year) |>
        dplyr::summarise(
          `N Lengthed` = sum(!is.na(Length_cm)),
          `N Aged` = sum(!is.na(Age))
        ),
      by = c("Project", "Year")
    ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Project = recode_Project(Project, gls = FALSE),
      ) |>
      dplyr::rename(
        Survey = "Project",
        `Positive Tows` = "Positive"
      ) |>
      as.data.frame(),
    file = here::here(table_dir, "data-survey-n.csv"),
    row.names = FALSE
  )

  #============================================================================
  # Design-based index of abundance using strata
  #============================================================================
  biomass <- data_survey_catch |>
    dplyr::group_by(Project) |>
    dplyr::group_map(
      .f = ~ nwfscSurvey::Biomass.fn(
        dat = .x,
        strat.df = strata,
        outputMedian = TRUE,
        dir = NULL,
        verbose = FALSE
      )
    )
  Project_names <- dplyr::group_by(data_survey_catch, Project) |>
    dplyr::group_keys() |>
    dplyr::pull(Project)
  names(biomass) <- recode_Project(Project_names)

  # These functions do not allow you to change the name of the saved
  # file, well the first one does but not without also adding a title to the
  # saved figure which is undesirable. We could rename the figure after
  # using the function.
  nwfscSurvey::PlotBio.fn(
    biomass[["wcgbt"]],
    dir = figure_dir,
    main = "WCGBTS"
  )
  nwfscSurvey::PlotBioStrata.fn(
    dir = figure_dir,
    dat = biomass[["wcgbt"]],
    mfrow.in = c(4, 4), gap = 0.01,
    sameylim = TRUE, ylim = c(0, 60)
  )

  #============================================================================
  # Marginal length- and age-composition data
  #============================================================================
  # Only marginal sex-specific lengths are included for WCGBTS
  # nwfscSurvey::GetN.fn calculates input sample sizes based on Hamel & Stewart
  # bootstrap approach. The effN sample size is calculated using the others
  # multiplier of 2.38. This number is multiplied by the number of tows in each
  # year.
  compositions <- data_survey_bio |>
    dplyr::filter(Project == "NWFSC.Combo") |>
    dplyr::mutate(Sexed = Sex %in% c("F", "M")) |>
    tidyr::nest(gg = -Sexed, .by = c("Project", "Sexed")) |>
    tidyr::crossing(type = c("Age", "Length")) |>
    dplyr::mutate(
      filtered = purrr::map2(
        .x = gg,
        .y = type,
        .f = ~ if (.y == "Length") {
          .x
        } else {
          dplyr::filter(.x, !is.na(Age)) |> dplyr::mutate(Length_cm = Age)
        }),
      bins = ifelse(
        test = type == "Length",
        yes = bins <- list(len_bins),
        no = bins <- list(age_bins)
      ),
      "n" = purrr::map2(
        .x = gg,
        .y = tolower(type),
        .f = ~ nwfscSurvey::GetN.fn(
          dat = .x,
          type = .y,
          species = "others",
          verbose = FALSE
        )
      )
    ) |>
    dplyr::mutate(
      composition = purrr::pmap(
        .l = list(
          datL = filtered,
          nSamps = n,
          sex = ifelse(Sexed, 3, 0),
          lgthBins = bins
        ),
        .f = nwfscSurvey::SurveyLFs.fn,
        datTows = dplyr::filter(data_survey_catch, Project == "NWFSC.Combo"),
        strat.df = strata,
        fleet = 7,
        month = 7,
        ageErr = 1,
        partition = 0,
        agelow = -1,
        agehigh = -1,
        printfolder = "",
        verbose = FALSE
      ),
    )

  composition_figures_and_save <- compositions |>
    dplyr::mutate(
      comp_figures = purrr::pmap(
        .l = list(
          add_save_name = recode_Project(Project),
          data = composition,
          add_0_ylim = tolower(type) == "age"
        ),
        .f = nwfscSurvey::plot_comps,
        dir = figure_dir,
      ),
      save_name = fs::path(
        "data-processed",
        paste(
          recode_Project(Project), tolower(type), "composition.csv",
          sep = "_"
        )
      )
    ) |>
    dplyr::group_by(save_name) |>
    dplyr::summarize(
      out = list(bind_compositions(composition))
    ) |>
    dplyr::mutate(
      work = purrr::map2(
        .x = out,
        .y = save_name,
        .f = ~ write.csv(.x, file = .y, row.names = FALSE)
      )
    )

  nwfscSurvey::PlotSexRatio.fn(
    dir = figure_dir,
    dat = data_survey_bio |> dplyr::filter(Project == "NWFSC.Combo"),
    data.type = "length"
  )

  gg <- nwfscSurvey::plot_proportion(
    data = data_survey_catch |>
      dplyr::filter(Project == "NWFSC.Combo") |>
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
      figure_dir,
      "data_survey_wcgbt_proportion-by-depth.png"
    ),
    width = 10,
    height = 10,
    plot = gg
  )

  gg <- nwfscSurvey::plot_proportion(
    data = data_survey_bio |>
      dplyr::filter(Project == "NWFSC.Combo") |>
      dplyr::mutate(Sex = nwfscSurvey::codify_sex(Sex)),
    column_factor = Sex,
    column_bin = Depth_m,
    width = 50,
    boundary = 0,
    bar_width = "equal"
  )
  ggplot2::ggsave(
    filename = fs::path(
      figure_dir,
      "data_survey_wcgbt_sex-by-depth.png"
    ),
    width = 10, 
    height = 10,
    plot = gg
  )

  gg <- nwfscSurvey::plot_proportion(
    data = data_survey_catch |> dplyr::mutate(new = factor(cpue_kg_km2 <= 0, levels = c(FALSE, TRUE), labels = c("Present", "Absent"))),
    column_factor = new,
    column_bin = Latitude_dd,
    width = 1,
    boundary = 0,
    bar_width = "equal"
  )
  ggplot2::ggsave(
    filename = fs::path(
      figure_dir,
      "data_survey_wcgbt_presence-by-lat.png"
    ),
    width = 10, 
    height = 10,
    plot = gg
  )
  
  gg <- ggplot2::ggplot(
    data = data_survey_bio |>
      dplyr::filter(
        Project == "NWFSC.Combo",
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
      figure_dir,
      "data_survey_wcgbt_young-length-by-year.png"
    ),
    width = 16, 
    height = 16,
    plot = gg
  )

  #=============================================================================
  # CAAL age composition data
  #=============================================================================
  caal <- nwfscSurvey::SurveyAgeAtLen.fn(
    datAL = data_survey_bio |>
      dplyr::filter(Project == "NWFSC.Combo") |>
      # Fix data so that small fish are included in the smallest Lbin_lo
      dplyr::mutate(Length_cm = ifelse(
        test = Length_cm < len_bins[1],
        yes = len_bins[1],
        no = Length_cm
      )
    ),
    datTows = data_survey_catch |> dplyr::filter(Project == "NWFSC.Combo"),
    strat.df = strata,
    lgthBins = len_bins,
    ageBins = age_bins,
    fleet = 7,
    month = 7,
    ageErr = 1,
    partition = 0,
    verbose = FALSE
  )
  caal$unsexed <- cbind(caal$unsexed, caal$unsexed[, 10:ncol(caal$unsexed)])

  utils::write.csv(
    bind_compositions(caal),
    file = fs::path("data-processed", "wcgbt_caal.csv"),
    row.names = FALSE
  )
  return(invisible(TRUE))
}
