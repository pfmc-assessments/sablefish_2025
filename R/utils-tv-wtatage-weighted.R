#' Estimate time-varying weight at age that is biomass weighted
#'
#' @details Predict weight-at-age for fishery and survey data with an area covariate
#' where the model includes age, year, cohort, sex, and area. There is a
#' smoother on age, year is modeled as a random effect, cohort is also a random
#' effect, and sex is a linear predictor to estimate weight, i.e.,
#' (weight ~ 1 + s(age, by = sex) + (1|fcohort) + (1|fyear) + sex + area). The
#' model estimated weight-at-age parameters by sex, cohort, year, and area are
#' then biomass weighted via a prediction grid based on a sdmTMB index.
#' @param max_age An integer specifying the maximum age of the modeled data
#'   in the Stock Synthesis model. All age data beyond this will be assigned
#'   to the maximum age. This is typically the age beyond which data are sparse
#'   and weight and length are essentially the same across ages. 
#' @return
#' A data frame in long format with time-varying weight-at-age data.
#' @author Kelli F. Johnson, Chantel Wetzel, and Eric Ward
#' 
estimate_tv_wtatage_weighted <- function(
  max_age = 30) {

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
      sex = tidyr::replace_na(sex, "U"),
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
  
  m1 <- sdmTMB::sdmTMB(
    data = weight_at_age_data,
    formula = weight ~ 1 + s(age, by = sex, k = 20) + (1|fcohort) + (1|fyear) + sex + area,
    family = sdmTMB::lognormal(link = "log"),
    spatial = "off",
    spatiotemporal = "off",
    time = "year",
    control = sdmTMB::sdmTMBcontrol(newton_loops = 1)
  )
  saveRDS(
    m1,
    file = here::here("data-raw", "weight_at_age", "tv_watage_fit.rds")
  )

  # extract random effects for plotting
  ran_vals <- sdmTMB::tidy(m1, "ran_vals")
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
  pred_grid <- readRDS(here::here("data-raw", "survey", "trawl", "indices", "wcgbt_pred_biomass.rds")) |>
    dplyr::rename(biomass_weight = est)
  # add both sexes
  pred_grid <- sdmTMB::replicate_df(pred_grid, "sex", unique(weight_at_age_data$sex))
  # add all ages
  pred_grid <- sdmTMB::replicate_df(pred_grid, "age", seq(0, max_age))
  # add cohort
  pred_grid$cohort <- pred_grid$year - pred_grid$age
  pred_grid$fyear <- as.factor(pred_grid$year)
  pred_grid$fcohort <- as.factor(pred_grid$cohort)

  # add state based on lat boundaries
  # pred_grid$state <- "CA"
  # pred_grid$state[which(pred_grid$Y > 4650)] <- "OR" # 42 deg lat
  # pred_grid$state[which(pred_grid$Y > 5094)] <- "WA" # 46 deg lat
  pred_grid$area <- "south"
  pred_grid$area[which(pred_grid$Y > 3983)] <- "north" # 36 deg lat
  
  # Make predictions of weight at age to the grid
  preds <- predict(m1, newdata = pred_grid)

  # Approach # 1 to make EWAA manually:
  # Where it is the average of all sexes.
  ewaa <- preds |>
    dplyr::group_by(year, sex, age) |>
    dplyr::summarise(
      # this is taking the spatial average
      pred_weight = weighted.mean(x = exp(est), w = biomass_weight),
      pred_unweight = mean(exp(est))
    ) |>
    as.data.frame()

  gg <- ewaa |>
    ggplot(aes(age, pred_weight, group = year, col = year)) +
    geom_line() +
    theme_bw() + 
    ggplot2::ylab("Estimated Weight by Year") +
    ggplot2::xlab("Age") +
    ggplot2::facet_grid(sex~.)
  ggplot2::ggsave(
    gg,
    width = 10, height = 7, units = "in",
    filename = here::here("data-raw", "weight_at_age", "plots", "year_wt_at_age_estimate.png")
  )
  
  breaks <- round(seq(0, max(ewaa[["pred_weight"]]), length = (length(unique(ewaa$age)))), 3)
  col_vec <- rainbow(50)
  gg2 <- ggplot(ewaa, aes(x = age, y = year, z = pred_weight), color = col_vec) +
    stat_summary_2d(geom = "tile", fun = function(x) cut(mean(x), breaks = breaks, right = FALSE))  +
    facet_grid(sex~.) +
    xlab("Age") + ylab("Year") +
    theme(legend.position = "none") 
  ggplot2::ggsave(
    gg2,
    width = 10, height = 7, units = "in",
    filename = here::here("data-raw", "weight_at_age", "plots", "weighted_wtatage_heatmap.png")
  )
  
  ewaa_long <- ewaa |>
    dplyr::select(year, sex, age, pred_weight)
  
  utils::write.csv(
    ewaa_long,
    fs::path(here::here("data-processed"), "weight-at-age-ogives.csv"),
    row.names = FALSE
  )
  return(ewaa_long)  
}

