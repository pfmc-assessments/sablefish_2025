#' Get the weight-at-age data from the survey data. 
#'
#' @param savedir A string specifying the path of interest.
#'
#' @export
#' @author Kelli F. Johnson
#'
process_weight_at_age_survey <- function(savedir = getwd()) {
  
  filter_data <- function(x) {
    x |>
      as.data.frame() |>
      dplyr::rename_with(
        tolower
      ) |>
      dplyr::filter(
        !is.na(age), !is.na(weight_kg)
      )
  }
  # There are no weights in the NWFSC Slope data
  survey_data <- rbind(
    filter_data(data_survey_bio$nwfsc_combo[[1]]),
    filter_data(data_survey_bio$afsc_slope$age_data),
    filter_data(data_survey_bio$triennial$age_data)
  ) |>
    dplyr::mutate(
      state = dplyr::case_when(latitude_dd > 46.25 ~ "WA", latitude_dd < 42.0 ~ "CA", .default = "OR"),
      sex = nwfscSurvey::codify_sex(sex)) |>
    dplyr::filter(sex != "U") |>
    dplyr::select(project, year, state, sex, length_cm, age_years, weight_kg, date)  |>
    dplyr::rename(source = project)
  
  # Save the data after combining with old data
  file_path <- fs::path(savedir, "data-raw", "data_weight_at_age_survey.csv")
  utils::write.csv(
    x = survey_data,
    file = file_path,
    quote = FALSE,
    row.names = FALSE
  )
  return(survey_data)
}

#' Get the weight-at-age data from the fishery data. 
#'
#' @param savedir A string specifying the path of interest.
#' @param bds 
#'
#' @export
#' @author Chantel Wetzel
#'
process_weight_at_age_survey <- function(savedir = getwd()) {
  
  raw_bds <-
    fs::dir_ls(here::here("data-raw", "bds"), regex = "PacFIN\\..+bds") |>
    purrr::map_df(
      .f = function(x) {load(x); return(bds.pacfin)}
    ) |>
    tibble::tibble() 
  cleaned_bds <- PacFIN.Utilities::cleanPacFIN(
    Pdata = raw_bds
  )
  
  bds_data <- cleaned_bds |>
    dplyr::rename_with(
      tolower
    ) |>
    dplyr::filter(
      age_method != "S",
      sex != "U",
      !is.na(age),
      !is.na(weightkg)) |>
    dplyr::mutate(
      source = "PacFIN",
      date = paste0(sample_year, "-", sample_month, "-", sample_day)) |>
    dplyr::select(source, state, sample_year, sex, lengthcm, age, weightkg, date) |>
    dplyr::rename(
      year = sample_year, 
      weight_kg = weightkg,
      age_years = age,
      length_cm = lengthcm)
  
  # Save the data after combining with old data
  file_path <- fs::path(savedir, "data-raw", "data_weight_at_age_fishery.csv")
  utils::write.csv(
    x = bds_data,
    file = file_path,
    quote = FALSE,
    row.names = FALSE
  )
  return(bds_data)
}
#' Create weight-at-age files for hake assessment
#'
#' Create weight-at-age files for the hake stock assessment.
#' Updated csv or rds files must exist prior to running.
#'
#' @param dir The directory where the data is stored.
#' It can either be relative or absolute because the working
#' directory will not be changed. Instead, `dir` is
#' used to import data and export results to.
#' @param max_age The age of the plus group used for the stock assessment.
#' This will correspond to the maximum age group in the data, not in the
#' model because SS can model many ages when there is only information in
#' the data for a few ages.
#' @param years A vector of years to search for recent data. Typically,
#' the vector starts with 2008 and ends with the most recent year
#' of data. This will allow files created from `process_weight_at_age_US()` to
#' be included in the analysis, i.e., recent US data. Typically, you
#' should not have to change this value from the default entry.
#' @param n_avg_years The number of early and late years to average since
#' 1975 and \code{max(yrs)} for the early and late analysis asked for
#' by the Scientific Review Group in 2017. The argument can be a single
#' value or a vector of two values, where in the latter case the second
#' value will be used for the most recent time period.
#' @param n_forecast The number of years to forecast into the future.
#' Typically, this is three for the hake assessment and will lead to
#' this many rows of mean weight-at-age data being copied to the data frame
#' beyond the last year of empirical data.
#' @param maturity A vector of maturity values from the maturity ogive. The
#'   length needs to be the same as the number of ages in the model, not the
#'   number of ages in the data. The default is to use the maturity ogive stored
#'   in the package.
#'
#' @export
#' @author Chantel Wetzel and Ian G. Taylor
#' @return 
#'
process_weight_at_age <- function(
  data_dir = here::here("data-processed"),
  savedir =  here::here("data-raw", "weight_at_age"),
  max_age = 70,
  years = 1995:2024,
  n_avg_years = 5,
  n_forecast = 12,
  maturity = maturity_at_age) {
  fs::dir_create(path = file.path(savedir, "plots"))
  files_weights <- fs::path(
    ext = "csv",
    data_dir,
    c("data_weight_at_age_survey", "data_weight_at_age_fishery")
  )
  data <- purrr::map_dfr(
    files_weights,
    .f = read.csv) |>
    weight_at_age_outlier(filter = FALSE, drop = FALSE)
  
  late <- (max(years) - n_avg_years + 1):(max(years))
  
  gg <- plot_weight_at_age(
    data = dplyr::filter(data, age_years <= max_age, outlier == FALSE),
    max_age = max_age
  )
  ggplot2::ggsave(
    gg,
    width = 7, height = 7, units = "in",
    filename = file.path(savedir, "plots", "meanweightatage_source.png")
  )
  gg <- plot_weight_at_age(
    data = dplyr::filter(data, age_years <= max_age, outlier == FALSE),
    max_age = max_age
  ) +
    ggplot2::facet_grid(source ~ .)
  ggplot2::ggsave(gg,
                  width = 7, height = 7, units = "in",
                  filename = file.path(savedir, "plots", "meanweightatage_all.png")
  )
  gg <- plot_weight_age(
    data = data
  ) +
    ggplot2::facet_wrap("source")
  ggplot2::ggsave(gg,
                  width = 7, height = 7, units = "in",
                  filename = file.path(savedir, "plots", "weightagedatasource.png")
  )
  gg <- plot_weight_age(
    data = data |> dplyr::filter(source != "PacFIN")
  ) 
  ggplot2::ggsave(gg,
                  width = 7, height = 7, units = "in",
                  filename = file.path(savedir, "plots", "weightagedatastate.png")
  )
  #### making input files for SS with the holes still present
  filter_data <- dplyr::filter(data, !outlier)
  wtage_all <- weight_at_age_wide(dat = filter_data)
  wtage_all_mean <- dplyr::bind_rows(
    weight_at_age_wide(filter_data |> dplyr::mutate(year = -1892)),
    weight_at_age_wide(filter_data)
  )
  
  #### making alternative data.frame with mean lengths
  lenage_all_mean <- dplyr::bind_rows(
    weight_at_age_wide(
      filter_data |>
        dplyr::mutate(year = -1892) |>
        dplyr::filter(!is.na(length_cm)),
      value = "length"
    ),
    weight_at_age_wide(
      filter_data |>
        dplyr::filter(!is.na(length_cm)),
      value = "length"
    )
  )
  # repeat but return sample sizes instead of mean weights
  counts_all_mean <- dplyr::bind_rows(
    weight_at_age_wide(
      filter_data |> dplyr::mutate(year = -1892),
      value = "count"
    ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))),
    weight_at_age_wide(
      filter_data,
      value = "count"
    ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0)))
  )
  utils::write.csv(
    setNames(counts_all_mean, gsub("#", "", colnames(counts_all_mean))),
    file.path(normalizePath(data_dir), "wtatage-all-samplesize.csv"),
    row.names = FALSE
  )
  # new method does only linear interpolation within each age (only works with all data)
  wtageInterp1_All <- dointerpSimple(wtage_all)
  
  #### do 2nd interpolation (actually an extrapolation at the edges)
  wtageInterp2_All <- fill_wtage_matrix(wtageInterp1_All)
  wtageInterp2_All$Note <- fill_wtage_matrix(wtage_all)$Note
  
  # write output combining all fleets closer to format used by SS3
  wtage_all_mean$Note <- c(paste("# Mean from ", min(filter_data$year), "-", max(filter_data$year), sep = ""), wtageInterp2_All$Note)
  wtageInterp2_All <- rbind(wtage_all_mean[1, ], wtageInterp2_All)
  
  # matrices for plotting
  make_wtatage_plots(
    plots = c(1:3, 6),
    data = wtage_all_mean,
    counts = counts_all_mean,
    lengths = lenage_all_mean,
    dir = file.path(savedir, "plots"),
    year =  format(Sys.Date(), "%Y"),
    max_age = max_age
  )
  
  # adding ages 16-20 as repeats of age 15
  wtage_extended <- wtageInterp2_All[, -grep("Note", colnames(wtageInterp2_All))]
  wtage_extended <- wtage_extended[, c(
    1:ncol(wtage_extended),
    rep(ncol(wtage_extended), times = sum(!(1:length(maturity) - 1) %in% 0:max_age))
  )]
  wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))] <-
    round(wtage_extended[, -grep("^[^a]|Note", colnames(wtage_extended))], 4)
  colnames(wtage_extended)[grep("^a", colnames(wtage_extended))] <-
    paste0("a", seq_along(maturity) - 1)
  
  ## Add forecast average
  withforecast <- dplyr::bind_rows(
    wtage_extended,
    wtage_extended |>
      dplyr::filter(year %in% late) %>%
      dplyr::mutate(
        dplyr::across(.cols = dplyr::starts_with("a"), mean),
        year = max(year) + 1 #:NROW(.)
      ) #%>%
      #dplyr::filter(
      #  rep(c(TRUE, FALSE), times = c(n_forecast, NROW(.) - n_forecast))
      #)
  )
  write_wtatage_file(
    file = fs::path(data_dir, "wtatage.ss"),
    data = as.data.frame(withforecast),
    maturity = maturity
  )
  save(
    filter_data, wtage_all, wtage_all_mean, withforecast,
    file = fs::path(savedir, "LWAdata.Rdata")
  )
  
  return(withforecast)
}
