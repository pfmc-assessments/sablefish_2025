# TODO: ----

# TODO: Use data-table objects rather than files
# TODO: Make sure that predictions should be the average of males, females,
#       and unknown sex

# Functions ----------------

#' Estimate time-varying weight at age
#'
#' @details Predict weight-at-age for fishery and survey data without spatial
#' information, where the model includes age, year, cohort, and sex. There is a
#' smoother on age, year is modeled as a random effect, cohort is also a random
#' effect, and sex is a linear predictor to estimate weight, i.e.,
#' (weight ~ 1 + s(age, by = sex) + (1|fcohort) + (1|fyear) + sex + area).
#' @param max_age An integer specifying the maximum age of the modeled data
#'   in the Stock Synthesis model. All age data beyond this will be assigned
#'   to the maximum age. This is typically the age beyond which data are sparse
#'   and weight and length are essentially the same across ages. 
#' @param first_year A four-digit integer specifying the first year of data in
#'   the assessment model. Sometimes weight-at-age data might begin before this
#'   year but the data are so sparse for other data that the model should not
#'   include this year as a separate year. For example, weight-at-age data for
#'   sablefish start in the early 80s but are sparse until the late 90s, 
#'   which means that the cohorts are predicted for years prior to this,
#'   i.e., year - age, so we want to remove these early years with little data. 
#' @return
#' A data frame in long format with time-varying weight-at-age data.
#' @author Kelli F. Johnson and Chantel Wetzel
estimate_tv_weight_at_age <- function(
  max_age = 30, 
  first_year = 1975) {
  
  weight_at_age_files <- fs::dir_ls(
    regexp = "data_weight_at_age_survey",
    here::here("data-processed")
  )
  
  # Load in data
  weight_at_age_data <- purrr::map_df(
    weight_at_age_files,
    utils::read.csv,
    .id = "path"
  ) |>
    dplyr::mutate(
      age = ifelse(age_years > max_age, max_age, age_years),
      cohort = year - age,
      sex = as.factor(sex),
      fyear = as.factor(year),
      fcohort = as.factor(cohort)    
    ) |>
    dplyr::rename(
      weight = weight_kg
    ) |>
    dplyr::rename_with(.fn = tolower) |>
    dplyr::select(-path) |>
    dplyr::filter(weight > 0, outlier == FALSE)

  remove <- which(weight_at_age_data$sex == "U" & weight_at_age_data$age != 0)
  weight_at_age_data <- weight_at_age_data[-remove, ]
  # Randomly assign unsexed age-0 fish to a M or F 50/50
  set.seed(98105)
  weight_at_age_data <- weight_at_age_data |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      sex = dplyr::case_when(
        sex == "U" ~sample(rep(c("F", "M"), length = dplyr::n())), .default = sex),
      sex = as.factor(sex)
    )
  
  # TODO: 
  # 1. add area to the model
  fit <- sdmTMB::sdmTMB(
    data = weight_at_age_data,
    formula = weight ~ 1 + s(age, by = sex, k = 20) + (1|fcohort) + (1|fyear) + sex, # + area,
    family = sdmTMB::lognormal(link = "log"),
    spatial = "off",
    spatiotemporal = "off",
    time = "year",
    control = sdmTMB::sdmTMBcontrol(newton_loops = 1)
  )
  saveRDS(
    fit,
    file = here::here("data-raw", "weight_at_age", "tv_watage_fit.rds")
  )
  
  # extract random effects for plotting
  ran_vals <- sdmTMB::tidy(fit, "ran_vals")
  cohort_effects <- ran_vals[grep("fcohort", ran_vals$term),]
  year_effects <- ran_vals[grep("fyear", ran_vals$term),]
  
  cohort_effects$year <- as.numeric(substr(cohort_effects$term, 9, 12))
  p1 <- ggplot(cohort_effects, aes(year,estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line() + xlab("Year") + ylab("Estimate") +
    theme_bw() +
    ggtitle("Cohort effects (+/- 2SE)")
  
  year_effects$year <- as.numeric(substr(year_effects$term, 7, 10))
  p2 <- ggplot(year_effects, aes(year,estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5) +
    geom_line() + xlab("Year") + ylab("Estimate") +
    theme_bw() +
    ggtitle("Year effects (+/- 2SE)")
  ggsave(
    gridExtra::grid.arrange(p1, p2, ncol = 1),
    width = 7, height = 7, units = "in",
    filename = here::here("data-raw", "weight_at_age",  "plots", "cohort_and_year_effects.png")
  )
  
  # create prediction grid
  # TODO: 
  # 1. Modify prediction grid to have a weighting by area
  pred_grid <- expand.grid(
    year = unique(weight_at_age_data[["year"]]),
    #area = unique(weight_at_age_data[["area"]]),
    sex = unique(weight_at_age_data[["sex"]]),
    age = 0:max_age
  ) |>
    dplyr::mutate(
      cohort = year - age
    ) |>
    dplyr::filter(
      cohort %in% intersect(
        cohort,
        weight_at_age_data[["cohort"]]
      )
    ) |>
    dplyr::mutate(
      fyear = as.factor(year),
      fcohort = as.factor(cohort)
    )
  
  # Get estimates
  preds <- predict(fit, newdata = pred_grid) |>
    dplyr::mutate(est_weight = exp(est)) |>
    dplyr::filter(year >= first_year)
  
  # Make estimated weight-at-age where it is the average of area and sex.
  # TODO: 
  # 1. Does area need to be added here or is taken care if it is available in the 
  # weighted in the prediction grid?
  ewaa <- dplyr::group_by(preds, year, sex, age) |>
    dplyr::summarise(
      n = dplyr::n(),
      pred_weight = mean(exp(est))
    ) |>
    as.data.frame()
  
  ewaa_long <- ewaa |>
    dplyr::select(year, sex, age, pred_weight)
  
  ewaa_wide <- tidyr::pivot_wider(
    ewaa_long,
    names_from = age,
    values_from = pred_weight
  ) |>
    dplyr::relocate(`0`, .before = `1`) |>
    as.matrix()
  
  utils::write.csv(
    ewaa_long,
    fs::path(here::here("data-processed"), "weight-at-age-ogives.csv"),
    row.names = FALSE
  )
  
  lines_of_weight_at_age_per_cohort <- ggplot2::ggplot(
    preds |> dplyr::filter(sex != "U"),
    ggplot2::aes(x = age, y = est_weight)
  ) +
    ggplot2::geom_line(ggplot2::aes(col = fcohort)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_grid(sex~.) +
    ggplot2::ylab("Estimated Weight by Cohort") +
    ggplot2::xlab("Age") +
    ggplot2::facet_grid(sex~.)
  ggplot2::ggsave(
    lines_of_weight_at_age_per_cohort,
    width = 10, height = 7, units = "in",
    filename = here::here("data-raw", "weight_at_age", "plots", "cohort_wt_at_age_estimate.png")
  )
  
  return(ewaa_long)
}

# estimate_tv_maturity_at_age <- function() {
#   cli::cli_inform(c(
#     "i" = "Time-varying maturity is estimated by Eric Ward.",
#     "!" = "Ensure the rds file is downloaded and used to update maturity.",
#     "i" = "Find the raw .rds on ericward-noaa/hake-maturity-assessment-2025.",
#     "i" = "Add model = 'Spatial + temperature' as a column to the rds data.",
#     "i" = "Save the information from the .rds in maturity-ogives.csv"
#   ))
# }
