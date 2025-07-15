library(indexwc)

savedir <- file.path(here::here(), "data-raw", "survey", "trawl", "indices", "triennial")
sp <- "sablefish"
  
configuration_sub <- configuration |>
  dplyr::filter(source == "Triennial", species == sp)

configuration_sub <- configuration_sub[2, ]
configuration_sub$max_depth <- -366

pulled_data <- nwfscSurvey::pull_catch(
  common_name = c("sablefish"),
  survey = "Triennial",
  years = c(1995, 2004)
)

utm_zone_10 <- 32610
data_filtered <- format_data.nwfscSurvey(pulled_data) |>
  dplyr::filter(depth >= -366)
configuration <- configuration_sub

fit <- run_sdmtmb(
  dir_main = savedir,
  data = data_filtered,
  family = sdmTMB::delta_gamma(),
  formula = configuration$formula[1],
  n_knots = configuration$knots[1],
  share_range = configuration$share_range[1],
  anisotropy = configuration$anisotropy[1],
  spatial = "iid",
  spatiotemporal = "iid"
)

#+================================================  

data <- configuration |>
# Row by row ... do stuff then ungroup
dplyr::rowwise() |>
  # Pull the data based on the function found in fxn column
  dplyr::mutate(
    data_raw = list(format_data(eval(parse(text = fxn)))),
    data_filtered = list(data_raw |>
                           dplyr::filter(
                             depth <= min_depth, depth >= max_depth,
                             latitude >= min_latitude, latitude <= max_latitude,
                             year >= min_year, year <= max_year
                           ) |>
                           dplyr::mutate(split_mendocino = ifelse(latitude > 40.1666667, "N", "S")))
  ) |>
  dplyr::ungroup()

best <- data |>
  dplyr::mutate(
    # Evaluate the call in family
    family = purrr::map(family, .f = ~ eval(parse(text = .x))),
    # Run the model on each row in data
    results = purrr::pmap(
      .l = list(
        dir_main = savedir,
        data = data_filtered,
        formula = formula,
        family = family,
        anisotropy = anisotropy,
        n_knots = knots,
        share_range = share_range,
        spatiotemporal = purrr::map2(spatiotemporal1, spatiotemporal2, list)
      ),
      .f = indexwc::run_sdmtmb
    )
  )


#===============================================================================
# Model Output
#===============================================================================

base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
single_index <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_2", "8.36_single_triennial"))
r4ss::tune_comps(
  replist = single_index, 
  dir = here::here("model", "_star_panel_requests", "request_2", "8.36_single_triennial"),
  option = "Francis")
r4ss::SS_plots(single_index)
single_index_split <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_2", "8.36_single_index_split"))
r4ss::tune_comps(
  replist = single_index_split, 
  dir = here::here("model", "_star_panel_requests", "request_2", "8.36_single_index_split"),
  option = "Francis")
r4ss::SS_plots(single_index_split)

modelnames <- c(
  "Pre-STAR Base Model",
  "Triennial Single Index",
  "Triennial Single Index - Model Split")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  single_index,
  single_index_split))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "request_2_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_star_panel_requests", "request_2"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)
