library(r4ss)
run_r4ss_output <- FALSE
run_comparison <- FALSE

copy_files <- function(x, from_name, to_name){
  file.copy(
    from = here::here("model", "_bridging", from_name,  x),
    to = here::here("model", "_bridging", to_name,  x)
  )
}
data_file <- "data.ss"
ctl_file <- "control.ss"
forcast_file <- "forecast.ss"

model_2023 <- SS_output(here::here("model", "_bridging", "0_2023_model"))
model_2021 <- SS_output(here::here("model", "_bridging", "0_2021_model"))
model_2019 <- SS_output(here::here("model", "_bridging", "0_2019_base_model"))

if (run_r4ss_output){
  # The fleet numbering model also includes updating the SS3 version
  fleet_numbering <- SS_output(here::here("model", "_bridging", "1_fleet_numbering"))
  rm_enviro <- SS_output(here::here("model", "_bridging", "2_rm_enviro"))
  add_landings <- SS_output(here::here("model", "_bridging", "3_landings"))
  add_fishery_ages_all <- SS_output(here::here("model", "_bridging", "4.3_fishery_ages_all_years"))
  add_discard_len <- SS_output(here::here("model", "_bridging", "5.3_discard_lengths"))
  split_fleets <- SS_output(here::here("model", "_bridging", "6_split_fixed_gears"))
  blocks <- SS_output(here::here("model", "_bridging", "7_fix_blocks"))
  triennial <- SS_output(here::here("model", "_bridging", "8_triennial"))
  akfsc_slope <- SS_output(here::here("model", "_bridging", "9_akfsc_slope"))
  nwfsc_slope <- SS_output(here::here("model", "_bridging", "10_nwfsc_slope"))
  wcgbt <- SS_output(here::here("model", "_bridging", "11_wcgbt"))
  maturity <- SS_output(here::here("model", "_bridging", "12_maturity"))
  m_prior <- SS_output(here::here("model", "_bridging", "13_m_prior"))
  ageing_error <- SS_output(here::here("model", "_bridging", "14_ageing_error"))
  age_based <- SS_output(here::here("model", "_bridging", "14m_fix_survey_selex_params"))

  
  if (run_comparison) {
    modelnames <- c(
      "2023 Base", 
      #"SS3 Version",
      "- Enviro.",
      "+ Fishery Data",
      #"+ Simplify Ret./Selex. Blocks",
      #"+ Historical Surveys", 
      "+ Surveys",
      "+ Maturity",
      "+ M Prior",
      "+ Age-Based Sel. and Ret",
      "+ Static Wt-at-Age (M equal)")
    mysummary <- SSsummarize(list(
      model_2023,
      #fleet_numbering,
      rm_enviro,
      add_discard_len, 
      #blocks,
      #nwfsc_slope,
      wcgbt,
      maturity,
      m_prior,
      age_based,
      wtatage))
    SSplotComparisons(mysummary,
                      filenameprefix = "all_",
                      legendlabels = modelnames, 	
                      plotdir = here::here("model", "_bridging", "_plots"),
                      ylimAdj = 1.5,
                      btarg = 0.40,
                      minbthresh = 0.25,
                      pdf = TRUE)
  }
}


#===============================================================================
# 1. Revise the fleet numbering and SS3 versions
#===============================================================================
# Fleet 1 (FIX) -> 2
# Fleet 2 (TWL) -> 1
# Fleet 3 (ENV) leave as 3 (to be replaced with Pot)
# Fleet 4 (Triennial)
# Fleet 5 (AKFS Slope)
# Fleet 6 (NWFSC Slope)
# Fleet 7 (WCGBT)

model_2023 <- SS_output(here::here("model", "_bridging", "0_2023_model"))

old_dir <- "0_2023_model"
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "1_fleet_numbering"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(file = here::here("model", "_bridging", new_dir, data_file))
dat$fleetinfo$fleetname[1:2] <- c("TWL", "HKL") 
dat$catch <- dat$catch |> dplyr::mutate(fleet = dplyr::case_when(fleet == 1 ~ 2, fleet == 2 ~ 1, .default = fleet))
dat$lencomp <- dat$lencomp |> 
  dplyr::mutate(fleet = dplyr::case_when(fleet == 1 ~ 2, fleet == 2 ~ 1, .default = fleet))
dat$agecomp <- dat$agecomp |> 
  dplyr::mutate(fleet = dplyr::case_when(fleet == 1 ~ 2, fleet == 2 ~ 1, .default = fleet))
dat$discard_data <- dat$discard_data |> 
  dplyr::mutate(fleet = dplyr::case_when(fleet == 1 ~ 2, fleet == 2 ~ 1, .default = fleet))
dat$meanbodywt <- dat$meanbodywt |> 
  dplyr::mutate(fleet = dplyr::case_when(fleet == 1 ~ 2, fleet == 2 ~ 1, .default = fleet))
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$size_selex_parms <- dplyr::bind_rows(
  ctl$size_selex_parms[grep("TWL", rownames(ctl$size_selex_parms)), ],
  ctl$size_selex_parms[grep("FIX", rownames(ctl$size_selex_parms)), ])
ctl$age_selex_parms <- dplyr::bind_rows(
  ctl$age_selex_parms[grep("TWL", rownames(ctl$age_selex_parms)), ],
  ctl$age_selex_parms[-grep("TWL", rownames(ctl$age_selex_parms)), ])
ctl$size_selex_parms_tv <- dplyr::bind_rows(
  ctl$size_selex_parms_tv[grep("TWL", rownames(ctl$size_selex_parms_tv)), ],
  ctl$size_selex_parms_tv[grep("FIX", rownames(ctl$size_selex_parms_tv)), ])
ctl$age_selex_parms_tv <-  dplyr::bind_rows(
  ctl$age_selex_parms_tv[grep("TWL", rownames(ctl$age_selex_parms_tv)), ],
  ctl$age_selex_parms_tv[-grep("TWL", rownames(ctl$age_selex_parms_tv)), ])
ctl$Variance_adjustment_list <- ctl$Variance_adjustment_list|>
  dplyr::mutate(Fleet = dplyr::case_when(Fleet == 1 ~ 2, Fleet == 2 ~ 1, .default = Fleet))
ctl$age_selex_types <- dplyr::bind_rows(
  ctl$age_selex_types[grep("TWL", rownames(ctl$age_selex_types)), ],
  ctl$age_selex_types[-grep("TWL", rownames(ctl$age_selex_types)), ])
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

# move the new exe into the folder
file.copy(
  from = here::here("model", "_bridging", "ss3.exe"),
  to = here::here("model", "_bridging", new_dir),
  overwrite = TRUE)
setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

fleet_numbering <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "SS3 Version")
mysummary <- SSsummarize(list(model_2023, fleet_numbering))
SSplotComparisons(mysummary,
                  filenameprefix = "0_1_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  pdf = TRUE)

#===============================================================================
# 2. Remove environmental index
#===============================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "2_rm_enviro"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$Q_parms[2, "PHASE"] <- -99
ctl$lambdas$value <- 0
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

rm_enviro <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro))
SSplotComparisons(mysummary,
                  filenameprefix = "0_2_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 3. Update and extend landings data
#===============================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "3_landings"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
catch <- 
  read.csv(here::here("data-processed", "data_commercial_catch_cw.csv")) |>
  dplyr::mutate(
    fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)
  ) |> 
  dplyr::group_by(year, seas, fleet, catch_se) |>
  dplyr::summarise(
    catch = sum(catch_mt)
  ) |>
  dplyr::arrange(fleet) |>
  dplyr::relocate(
    catch, .after = fleet
  ) |> as.data.frame()
dat$endyr <- 2024
dat$catch <- catch
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_landings <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index",
  "+ Landings")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro, add_landings))
SSplotComparisons(mysummary,
                  filenameprefix = "0_3_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 4. Update and extend fishery age data - trawl
#===============================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "4.1_fishery_ages_trawl_2020"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
fishery_ages <- read.csv(here::here("data-processed", "data-commercial-comps_age-0-50.csv")) |>
  dplyr::filter(fleet == 1, year < 2021) |>
  dplyr::mutate(ageerr = 1)
colnames(fishery_ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  fishery_ages,
  dat$agecomp |> dplyr::filter(fleet != 1)) 

SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

# Retune the model
add_fishery_ages <- SS_output(here::here("model", "_bridging", new_dir))
dw <- tune_comps(replist = add_fishery_ages, fleet = 1, option = "Francis")
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$Variance_adjustment_list[
  which(ctl$Variance_adjustment_list$Data_type == 5 & ctl$Variance_adjustment_list$Fleet %in% 1), "Value"] <-
  rev(dw[which(dw$`#Data_type` == 5), "New_Francis"])
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

# Read the model
add_fishery_ages <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Trawl Ages - 2020")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro, add_landings, add_fishery_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "0_4_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(add_fishery_ages)

#===============================================================================
# 4. Update and extend fishery age data - trawl + fixed gear
#===============================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "4.2_fishery_ages_fixed_2020"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
fishery_ages <- read.csv(here::here("data-processed", "data-commercial-comps_age-0-50.csv")) |>
  dplyr::filter(fleet != 1, year < 2021) |>
  dplyr::mutate(fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)) |>
  dplyr::mutate(ageerr = 1)
colnames(fishery_ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  fishery_ages,
  dat$agecomp |> dplyr::filter(fleet != 2)) 

SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_fishery_ages_fg <- SS_output(here::here("model", "_bridging", new_dir))
dw <- tune_comps(replist = add_fishery_ages_fg, fleet = 1:2, option = "Francis")
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$Variance_adjustment_list[
  which(ctl$Variance_adjustment_list$Data_type == 5 & ctl$Variance_adjustment_list$Fleet %in% 1:2), "Value"] <-
  rev(dw[which(dw$`#Data_type` == 5), "New_Francis"])
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

add_fishery_ages_fg <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Trawl Ages - 2020",
  "+ Fixed Gear Ages - 2020")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro, add_landings, add_fishery_ages, add_fishery_ages_fg))
SSplotComparisons(mysummary,
                  filenameprefix = "0_4_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  pdf = TRUE)
SS_plots(add_fishery_ages_fg)

#===============================================================================
# 4.3 Update and extend fishery age data - trawl + fixed gear through 2024
#===============================================================================
new_dir <- "4.2_fishery_ages_fixed_2020"
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "4.3_fishery_ages_all_years"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
fishery_ages <- read.csv(here::here("data-processed", "data-commercial-comps_age-0-50.csv")) |>
  dplyr::mutate(fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)) |>
  dplyr::mutate(ageerr = 1)
colnames(fishery_ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  fishery_ages,
  dat$agecomp |> dplyr::filter(!fleet %in% 1:2)
  ) 

SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_fishery_ages_all <- SS_output(here::here("model", "_bridging", new_dir))
dw <- tune_comps(replist = add_fishery_ages_all, fleet = 1:2, option = "Francis")
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$Variance_adjustment_list[
  which(ctl$Variance_adjustment_list$Data_type == 5 & ctl$Variance_adjustment_list$Fleet %in% 1:2), "Value"] <-
  rev(dw[which(dw$`#Data_type` == 5), "New_Francis"])
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

add_fishery_ages_all <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Trawl Ages - 2020",
  "+ Fixed Gear Ages - 2020",
  "+ Trawl & Fixed Gear Ages - 2024")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro, add_landings, add_fishery_ages, add_fishery_ages_fg, add_fishery_ages_all))
SSplotComparisons(mysummary,
                  filenameprefix = "0_4_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(add_fishery_ages_all)

#=============================================================================================
# 5. Discard data - discard rates
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "5.1_discard_rate_data"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
discard_rates <- read.csv(here::here("data-processed", "data_commercial_discard_rates.csv")) |>
  dplyr::mutate(
    fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)
  ) |>
  dplyr::rename(
    obs = discard_rate,
    stderr = sd
  ) 
discard_rates_comb <- dplyr::bind_rows(
  discard_rates |> dplyr::filter(fleet == 1),
  discard_rates |>
  dplyr::filter(fleet == 2) |>
  dplyr::group_by(year, month, fleet) |>
  dplyr::summarise(
    obs = mean(obs),
    stderr = mean(stderr)
  ))

dat$discard_data <- discard_rates_comb

SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_discard_rates <- SS_output(here::here("model", "_bridging", new_dir))
add_var <- data.frame(
  Data_type = 2,
  Fleet = add_discard_rates$discard_tuning_info$fleet,
  Value = add_discard_rates$discard_tuning_info$added
) |> 
  dplyr::filter(Value > 0)

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$Block_Design[[2]][ctl$Block_Design[[2]] == 2022] <- 2024
ctl$Block_Design[[3]][ctl$Block_Design[[3]] == 2022] <- 2024
ctl$Block_Design[[4]][ctl$Block_Design[[4]] == 2022] <- 2024
ctl$Block_Design[[5]][ctl$Block_Design[[5]] == 2022] <- 2024
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  add_var,
  ctl$Variance_adjustment_list[ctl$Variance_adjustment_list$Data_type != 2, ]
 )

SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_discard_rates <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Rates")
mysummary <- SSsummarize(list(
  model_2023, rm_enviro, add_landings, add_fishery_ages_all, add_discard_rates))
SSplotComparisons(mysummary,
                  filenameprefix = "0_5_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(add_discard_rates)

#=============================================================================================
# 5. Discard data - discard weights
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "5.2_discard_rate_weight"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
discard_wght <- read.csv(here::here("data-processed", "data_commercial_discard_weight.csv")) |>
  dplyr::mutate(
    fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)
  ) |>
  dplyr::rename(
    year = X.year,
    part = partition,
    stderr = cv)
discard_wght_comb <- dplyr::bind_rows(
  discard_wght |> dplyr::filter(fleet == 1),
  discard_wght |>
    dplyr::filter(fleet == 2) |>
    dplyr::group_by(year, month, fleet, part, type) |>
    dplyr::summarise(
      obs = mean(obs),
      stderr = mean(stderr)
    )
  )
dat$meanbodywt <- discard_wght_comb

SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_discard_weights <- SS_output(here::here("model", "_bridging", new_dir))
add_var <- data.frame(
  Data_type = 3,
  Fleet = add_discard_weights$mnwgt_tuning_info$fleet,
  Value = add_discard_weights$mnwgt_tuning_info$added
) |>
  dplyr::filter(Value > 0)
# Negative suggested added var

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$MG_parms[3, "LO"] <- 55
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  add_var,
  ctl$Variance_adjustment_list[ctl$Variance_adjustment_list$Data_type != 3, ]
)

SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

add_discard_weights <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Rates",
  "+ Discard Mean Weights")
mysummary <- SSsummarize(list(
  model_2023, rm_enviro, add_landings, add_fishery_ages_all, add_discard_rates,
  add_discard_weights))
SSplotComparisons(mysummary,
                  filenameprefix = "0_5_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(add_discard_weights)

#=============================================================================================
# 5. Discard data - discard lengths
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "5.3_discard_lengths"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))

discard_lens <- read.csv(here::here("data-processed", "data_commercial_discard_length_composition.csv")) |>
  dplyr::mutate(
    fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)
  ) 

colnames(discard_lens) <- colnames(dat$lencomp)
dat$lencomp <- dplyr::bind_rows(
  discard_lens,
  dat$lencomp[!dat$lencomp$fleet %in% 1:2, ]
)
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_discard_len <- SS_output(here::here("model", "_bridging", new_dir))
dw <- tune_comps(replist = add_discard_len, fleet = 1:2, option = "Francis")
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
add_var <- dw[, 1:3]
colnames(add_var) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  add_var, 
  ctl$Variance_adjustment_list[c(-3, -4, -6, -7), ]
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

add_discard_len <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Rates",
  "+ Discard Mean Weights",
  "+ Discard Lengths")
mysummary <- SSsummarize(list(
  model_2023, rm_enviro, add_landings, add_fishery_ages_all, add_discard_rates,
  add_discard_weights, add_discard_len))
SSplotComparisons(mysummary,
                  filenameprefix = "0_5_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(add_discard_len)

#=============================================================================================
# 6. Split out HKL and Pot
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "6_split_fixed_gears"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
dat$fleetinfo$type[3] <- 1
dat$fleetinfo$fleetname[3] <- "Pot"
dat$fleetnames[3] <- "Pot"
dat$fleetinfo$units <- 1
dat$catch <- read.csv(here::here("data-processed", "data_commercial_catch_cw.csv")) |>
  dplyr::rename(catch = catch_mt) |>
  dplyr::filter(catch > 0)
dat$CPUEinfo[3, 2:3] <- c(1, 0) 
dat$CPUE <- dat$CPUE |> dplyr::filter(index != 3)

discard_rates <- read.csv(here::here("data-processed", "data_commercial_discard_rates.csv")) |>
  dplyr::rename(
    obs = discard_rate,
    stderr = sd
  ) 
discard_wght <- read.csv(here::here("data-processed", "data_commercial_discard_weight.csv")) |>
  dplyr::rename(
    year = X.year,
    part = partition,
    stderr = cv)

discard_lens <- read.csv(here::here("data-processed", "data_commercial_discard_length_composition.csv")) 
colnames(discard_lens) <- colnames(dat$lencomp)
dat$N_discard_fleets <- 3
dat$discard_data <- discard_rates
dat$meanbodywt <- discard_wght
dat$lencomp <- dplyr::bind_rows(
  discard_lens,
  dat$lencomp[dat$lencomp$fleet == 7, ]
)
add_fleet <- data.frame(
  fleet = 3, units = 2, errtype = -1
)
row.names(add_fleet) <- "Pot"
dat$discard_fleet_info <- dplyr::bind_rows(
  dat$discard_fleet_info, add_fleet
)

fishery_ages <- read.csv(here::here("data-processed", "data-commercial-comps_age-0-50.csv")) |>
  dplyr::mutate(ageerr = 1)
colnames(fishery_ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  fishery_ages,
  dat$agecomp |> dplyr::filter(!fleet %in% 1:2)) 
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

# Modify the control file
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
# remove the enviro survey
ctl$Q_options <- ctl$Q_options |> dplyr::filter(fleet != 3)
ctl$Q_parms <- ctl$Q_parms[c(-1, -2), ]
# add selectivity specification
ctl$size_selex_types[rownames(ctl$size_selex_types) %in% c("Pot", "ENV"), "Discard"] <- 2
ctl$age_selex_types[rownames(ctl$age_selex_types) %in% c("Pot", "ENV"), c("Pattern", "Male")] <- c(20, 1)
# add selectivity parameters
ctl$size_selex_parms <- dplyr::bind_rows(
  ctl$size_selex_parms,
  ctl$size_selex_parms[9:16, ]
)
# add tv selex/retention parameters
ctl$age_selex_parms <- dplyr::bind_rows(
  ctl$age_selex_parms[1:16, ],
  ctl$age_selex_parms[7:16, ],
  ctl$age_selex_parms[17:nrow(ctl$age_selex_parms), ]
)
ctl$size_selex_parms_tv <- dplyr::bind_rows(
  ctl$size_selex_parms_tv,
  ctl$size_selex_parms_tv[11:nrow(ctl$size_selex_parms_tv), ]
)
ctl$age_selex_parms_tv <- dplyr::bind_rows(
  ctl$age_selex_parms_tv[-nrow(ctl$age_selex_parms_tv), ],
  ctl$age_selex_parms_tv[4:(nrow(ctl$age_selex_parms_tv) - 1), ],
  ctl$age_selex_parms_tv[nrow(ctl$age_selex_parms_tv), ]
)
# add variance adjustments
fleet_3 <- ctl$Variance_adjustment_list[ctl$Variance_adjustment_list$Fleet == 2, ] |>
  dplyr::mutate(Fleet = 3)

ctl$Variance_adjustment_list <- dplyr::bind_rows(
  fleet_3,
  ctl$Variance_adjustment_list[ctl$Variance_adjustment_list$Data_type != 2, ]
) |>
  dplyr::arrange(Data_type, Fleet)
ctl$lambdas[, c("like_comp", "value")] <- c(4, 1)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

starter <- SS_readstarter(file = here::here("model", "_bridging", new_dir, "starter.ss"))
starter$init_values_src <- 0
SS_writestarter(
  mylist = starter,
  dir = here::here("model", "_bridging", new_dir),
  overwrite = TRUE
)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
split_fleets <- SS_output(here::here("model", "_bridging", new_dir))

dw <- tune_comps(replist = split_fleets, fleet = 1:3, option = "Francis")[, 1:3]
add_rate <- data.frame(
  Data_type = 2, 
  Fleet = split_fleets$discard_tuning_info$fleet,
  Value = split_fleets$discard_tuning_info$added
) |>
  dplyr::filter(Value > 0)
add_weight <- data.frame(
  Data_type = 3,
  Fleet = split_fleets$mnwgt_tuning_info$fleet,
  Value = split_fleets$mnwgt_tuning_info$added
) |>
  dplyr::filter(Value > 0)
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  add_rate,
  add_weight,
  dw,
  ctl$Variance_adjustment_list |>
    dplyr::filter(!Fleet %in% 1:3)
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

split_fleets <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "- SSH Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Data",
  "+ Split Fixed Gears")
mysummary <- SSsummarize(list(
  model_2023, rm_enviro, add_landings, add_fishery_ages_all, add_discard_len, split_fleets))
SSplotComparisons(mysummary,
                  filenameprefix = "0_6_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(split_fleets)

#=============================================================================================
# 7. Revise retention and selectivity blocks
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "7_fix_blocks"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
# Get rid of the extra triennial block and hkl/pot selectivity blocks
ctl$N_Block_Designs <- 4
ctl$blocks_per_pattern <- ctl$blocks_per_pattern[c(1:3, 5)]
ctl$Block_Design <- list(ctl$Block_Design[[1]], ctl$Block_Design[[2]], 
                         ctl$Block_Design[[3]], ctl$Block_Design[[4]])

ctl$Block_Design[[1]] <- c(1995, 2004)

# Revised hkl/pot retention blocks
ctl$blocks_per_pattern[2] <- 3
# WWII, trip limits, IFQ, survival credits
ctl$Block_Design[[2]] <- c(1942, 1946, 2011, 2018, 2019, 2024)

# Revise trawl retention blocks
ctl$blocks_per_pattern[3] <- 3
# WWII, IFQ, survival credits
ctl$Block_Design[[3]] <- c(1942, 1946, 2011, 2018, 2019, 2024)

# Revise trawl selectivity blocks
ctl$blocks_per_pattern[4] <- 1
# RCAs
ctl$Block_Design[[4]] <- c(2002, 2024)

# Fix retention parameters
# trawl length base retention parameters cover 1890-1941 and 1947-2010
trawl_retention <- ctl$size_selex_parms[1:8, ]
trawl_retention[1:3, "PHASE"] <- 5
# hkl length base retention parameters cover 1890-1941 and 1947-2010
hkl_retention <- ctl$size_selex_parms[9:16, ]
hkl_retention[1:3, "PHASE"] <- 5
# pot length base retention parameters cover 1890-1941 and 1947-2010
pot_retention <- ctl$size_selex_parms[17:24, ]
pot_retention[1:3, "PHASE"] <- 5

ctl$size_selex_parms <- dplyr::bind_rows(
  trawl_retention,
  hkl_retention,
  pot_retention
)

# Fix base selectivity parameters
trawl_selex <- ctl$age_selex_parms[1:6, ]
trawl_selex[c(1, 4), "PHASE"] <- 4
trawl_selex[4, "Block"] <- 4

hkl_selex <- ctl$age_selex_parms[7:16, ]
hkl_selex[1, "PHASE"] <- 4
hkl_selex[c(1, 3), "Block"] <- 0
hkl_selex[c(1, 3), "Block_Fxn"] <- 0

pot_selex <- ctl$age_selex_parms[17:26, ]
pot_selex[1, "PHASE"] <- 4
pot_selex[c(1, 3), "Block"] <- 0
pot_selex[c(1, 3), "Block_Fxn"] <- 0

triennial_selex <- ctl$age_selex_parms[27:36, ]
triennial_selex[4, "Block"] <- 1
survey_selex <- ctl$age_selex_parms[37:nrow(ctl$age_selex_parms), ] 

ctl$age_selex_parms <- dplyr::bind_rows(
  trawl_selex,
  hkl_selex,
  pot_selex,
  triennial_selex,
  survey_selex
)

# Time varying parameters
trawl_tv_retention <- ctl$size_selex_parms_tv[1:10, ]
trawl_tv_retention <- trawl_tv_retention[c(1, 4, 5, 6, 9, 10), ]
trawl_tv_retention[6, "PHASE"] <- -5
trawl_tv_retention[6, "INIT"] <- 10
hkl_tv_retention <- ctl$size_selex_parms_tv[11:20, ]
hkl_tv_retention <- hkl_tv_retention[c(1, 4, 5, 6, 9, 10), ]
hkl_tv_retention[c(3), "PHASE"] <- 5
hkl_tv_retention[5, "INIT"] <- 10
pot_tv_retention <- ctl$size_selex_parms_tv[21:30, ]
pot_tv_retention <- pot_tv_retention[c(1, 4, 5, 6, 9, 10), ]
pot_tv_retention[c(3), "PHASE"] <- 5
pot_tv_retention[5, "INIT"] <- 10

ctl$size_selex_parms_tv <- dplyr::bind_rows(
  trawl_tv_retention,
  hkl_tv_retention,
  pot_tv_retention
)
trawl_tv_selex <- ctl$age_selex_parms_tv[1:3, ]
trawl_tv_selex <- trawl_tv_selex[2, ]
survey_tv_selex <- ctl$age_selex_parms_tv[nrow(ctl$age_selex_parms_tv), ]

ctl$age_selex_parms_tv <- dplyr::bind_rows(
  trawl_tv_selex,
  survey_tv_selex
)

SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
blocks <- SS_output(here::here("model", "_bridging", new_dir))

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))

dw <- tune_comps(replist = blocks, fleet = 1:3, option = "Francis")[, 1:3]
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  ctl$Variance_adjustment_list |> dplyr::filter(Data_type %in% 2:3),
  dw,
  ctl$Variance_adjustment_list |> dplyr::filter(Fleet %in% 5:7)
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")

blocks <- SS_output(here::here("model", "_bridging", new_dir))
modelnames <- c(
  "2023 Base", 
  "- SSH Enviro. Index",
  "+ Landings",
  "+ Fishery Ages",
  "+ Discard Data",
  "+ Split Fixed Gears",
  "Simplify Ret./Selex. Blocks")
mysummary <- SSsummarize(list(
  model_2023, rm_enviro, add_landings, add_fishery_ages_all, add_discard_len, split_fleets,
  blocks))
SSplotComparisons(mysummary,
                  filenameprefix = "0_7_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(blocks)


#=============================================================================================
# 8. Triennial
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "8_triennial"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
indices <- read.csv(here::here("data-processed", "data-survey-indices.csv")) |>
  dplyr::filter(fleet == 4)
colnames(indices) <- colnames(dat$CPUE)
dat$CPUE <- dplyr::bind_rows(
  indices, 
  dat$CPUE |> dplyr::filter(index != 4)
)
ages <- dplyr::bind_rows(
  read.csv(here::here("data-processed", "data-survey-comps-ages-triennial-early.csv")),
  read.csv(here::here("data-processed", "data-survey-comps-ages-triennial-late.csv"))
  )
colnames(ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  ages,
  dat$agecomp |> dplyr::filter(fleet != 4)
)
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
triennial <- SS_output(here::here("model", "_bridging", new_dir))

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
dw <- tune_comps(replist = triennial, fleet = 4, option = "Francis")[1:3]
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  dw,
  ctl$Variance_adjustment_list |> dplyr::filter(Fleet != 4)
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")
triennial <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial))
SSplotComparisons(mysummary,
                  filenameprefix = "0_8_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(triennial)


#=============================================================================================
# 9. AFSC Slope
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "9_akfsc_slope"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
indices <- read.csv(here::here("data-processed", "data-survey-indices.csv")) |>
  dplyr::filter(fleet == 5)
colnames(indices) <- colnames(dat$CPUE)
dat$CPUE <- dplyr::bind_rows(
  indices, 
  dat$CPUE |> dplyr::filter(index != 5)
)
ages <- read.csv(here::here("data-processed", "data-survey-comps-ages-afscslope.csv"))
colnames(ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  ages,
  dat$agecomp |> dplyr::filter(fleet != 5)
)
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, "control.ss_new"))
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
akfsc_slope <- SS_output(here::here("model", "_bridging", new_dir))

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
dw <- tune_comps(replist = akfsc_slope, fleet = 5, option = "Francis")[1:3]
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  dw,
  ctl$Variance_adjustment_list |> dplyr::filter(Fleet != 5)
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
starter <- SS_readstarter(file = here::here("model", "_bridging", new_dir, "starter.ss"))
starter$init_values_src <- 1
SS_writestarter(
  mylist = starter,
  dir = here::here("model", "_bridging", new_dir),
  overwrite = TRUE
)

shell("ss3 -nohess")
akfsc_slope <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope))
SSplotComparisons(mysummary,
                  filenameprefix = "0_9_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(akfsc_slope)

#=============================================================================================
# 10. NWFSC Slope
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "10_nwfsc_slope"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
indices <- read.csv(here::here("data-processed", "data-survey-indices.csv")) |>
  dplyr::filter(fleet == 6)
colnames(indices) <- colnames(dat$CPUE)
dat$CPUE <- dplyr::bind_rows(
  indices, 
  dat$CPUE |> dplyr::filter(index != 6)
)
ages <- read.csv(here::here("data-processed", "data-survey-comps-ages-nwfscslope.csv"))
colnames(ages) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  ages,
  dat$agecomp |> dplyr::filter(fleet != 6)
)
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
nwfsc_slope <- SS_output(here::here("model", "_bridging", new_dir))

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
dw <- tune_comps(replist = nwfsc_slope, fleet = 6, option = "Francis")[1:3]
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  dw,
  ctl$Variance_adjustment_list |> dplyr::filter(Fleet != 6)
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")
nwfsc_slope <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope",
  "+ NWFSC Slope")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope,
  nwfsc_slope))
SSplotComparisons(mysummary,
                  filenameprefix = "0_10_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(nwfsc_slope)

#=============================================================================================
# 11. NWFSC WCGBT
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "11_wcgbt"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dat <- SS_readdat(
  file = here::here("model", "_bridging", new_dir, data_file))
indices <- read.csv(here::here("data-processed", "data-survey-indices.csv")) |>
  dplyr::filter(fleet == 7) 
colnames(indices) <- colnames(dat$CPUE)
dat$CPUE <- dplyr::bind_rows(
  indices, 
  dat$CPUE |> dplyr::filter(index != 7)
)
lengths <- read.csv(here::here("data-processed", "data-survey-comps-lengths-wcgbt.csv"))
colnames(lengths) <- colnames(dat$lencomp)
dat$lencomp <-  dplyr::bind_rows(
  lengths,
  dat$lencomp |> dplyr::filter(fleet != 7)
)
ages <- read.csv(here::here("data-processed", "data-survey-comps-ages-wcgbt.csv")) |>
  dplyr::mutate(fleet = -7)
caal <- read.csv(here::here("data-processed", "data-survey-comps-caal-wcgbt.csv")) 
colnames(ages) <- colnames(dat$agecomp)
colnames(caal) <- colnames(dat$agecomp)
dat$agecomp <- dplyr::bind_rows(
  ages,
  caal,
  dat$agecomp |> dplyr::filter(!fleet %in% c(-7, 7))
)
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
wcgbt <- SS_output(here::here("model", "_bridging", new_dir))

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
dw <- tune_comps(replist = wcgbt, fleet = 7, option = "Francis")[1:3]
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  dw,
  ctl$Variance_adjustment_list |> dplyr::filter(Fleet != 7)
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

shell("ss3")
wcgbt <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope",
  "+ NWFSC Slope", 
  "+ WCGBT")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope,
  nwfsc_slope,
  wcgbt))
SSplotComparisons(mysummary,
                  filenameprefix = "0_11_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(wcgbt)

#=============================================================================================
# 12. Maturity
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "12_maturity"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$maturity_option <- 3
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

cli::cli_abort("The maturity vector needs to be added by hand.")

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
maturity <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope",
  "+ NWFSC Slope", 
  "+ WCGBT",
  "+ Maturity")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope,
  nwfsc_slope,
  wcgbt,
  maturity))
SSplotComparisons(mysummary,
                  filenameprefix = "0_12_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(maturity)

#===============================================================================
# 13. M prior
#================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "13_m_prior"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
prior <- round(log(5.4 / 75), 3)
sd_prior <- 0.31
mgparam <- ctl$MG_parms
mgparam[c(1, 13), "PRIOR"] <- prior
mgparam[c(1, 13), "PR_SD"] <- sd_prior
ctl$MG_parms <- mgparam
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

starter <- SS_readstarter(file = here::here("model", "_bridging", new_dir, "starter.ss"))
starter$init_values_src <- 1
SS_writestarter(
  mylist = starter,
  dir = here::here("model", "_bridging", new_dir),
  overwrite = TRUE
)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
m_prior <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope",
  "+ NWFSC Slope", 
  "+ WCGBT",
  "+ Maturity",
  "+ M Prior")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope,
  nwfsc_slope,
  wcgbt,
  maturity,
  m_prior))
SSplotComparisons(mysummary,
                  filenameprefix = "0_13_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(m_prior)


#===============================================================================
# 15-16 Rec Devs
#================================================================================

main_rec_devs <- SS_output(here::here("model", "_bridging", "15_rec_devs_main"))
r4ss::tune_comps(
  replist = main_rec_devs, 
  dir = here::here("model", "_bridging", "15_rec_devs_main"),
  option = "Francis")


early_rec_devs <- SS_output(here::here("model", "_bridging", "16_no_early_devs"))
r4ss::tune_comps(
  replist = early_rec_devs, 
  dir = here::here("model", "_bridging", "16_no_early_devs"),
  option = "Francis")

modelnames <- c(
  "Ageing Error",
  "Main Rec. Devs. 1975",
  "No Early Rec. Devs.")
mysummary <- SSsummarize(list(
  ageing_error,
  main_rec_devs,
  early_rec_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "14-16_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 17 Foreign Fleets
#================================================================================
foreign_fleets <- SS_output(here::here("model", "_bridging", "17_foreign_fleets_2023_rec_devs"))
r4ss::tune_comps(
  replist = foreign_fleets, 
  dir = here::here("model", "_bridging", "17_foreign_fleets"),
  option = "Francis")
SS_plots(foreign_fleets)

#===============================================================================
# 18 Age Based Retention
#================================================================================
age_based_retention <- SS_output(here::here("model", "_bridging", "18_age_based_retention"))
r4ss::tune_comps(
  replist = age_based_retention, 
  dir = here::here("model", "_bridging", "18_age_based_retention"),
  option = "Francis")
SS_plots(age_based_retention)


#===============================================================================
# 19 Selectivity Blocks and Parameters
#===============================================================================
ret_blocks_parameters <- SS_output(here::here("model", "_bridging", "19_blocks_parameters"))
r4ss::tune_comps(
  replist = ret_blocks_parameters, 
  dir = here::here("model", "_bridging", "19_blocks_parameters"),
  option = "Francis")
SS_plots(ret_blocks_parameters)


#===============================================================================
# Coming at the bridging from the final model and working down
#===============================================================================
base <- SS_output(here::here("model", "_bridging", "30_8.33_enviro_index_mle"))
r4ss::tune_comps(
  replist = base, 
  dir = here::here("model", "_bridging", "30_8.33_enviro_index_mle"),
  option = "Francis")

watage <- SS_output(here::here("model", "_bridging", "29_add_watage"))
r4ss::tune_comps(
  replist = watage, 
  dir = here::here("model", "_bridging", "29_add_watage"),
  option = "Francis")

sex_specific_m <- SS_output(here::here("model", "_bridging", "28_single_m"))
steepness <- SS_output(here::here("model", "_bridging", "27_steepness"))
r4ss::tune_comps(
  replist = steepness, 
  dir = here::here("model", "_bridging", "27_steepness"),
  option = "Francis")

discard_fleets <- SS_output(here::here("model", "_bridging", "26_discard_fleets_age_based_selectivity"))
r4ss::tune_comps(
  replist = discard_fleets, 
  dir = here::here("model", "_bridging", "26_discard_fleets_age_based_selectivity"),
  option = "Francis")

rec_devs_1975_no_early <- SS_output(here::here("model", "_bridging", "25_no_early_devs_1975_main_devs"))
rec_devs_1975 <- SS_output(here::here("model", "_bridging", "25_1975_main_devs"))
rec_devs_2023 <- SS_output(here::here("model", "_bridging", "25_2023_rec_devs"))
r4ss::tune_comps(
  replist = rec_devs_2023, 
  dir = here::here("model", "_bridging", "25_rec_devs"),
  option = "Francis")

triennial <- SS_output(here::here("model", "_bridging", "24_triennial_dw"))
r4ss::tune_comps(
  replist = triennial, 
  dir = here::here("model", "_bridging", "24_triennial_dw"),
  option = "Francis")

afsc_slope <- SS_output(here::here("model", "_bridging", "23_afsc_slope"))
r4ss::tune_comps(
  replist = afsc_slope, 
  dir = here::here("model", "_bridging", "23_afsc_slope"),
  option = "Francis")

#retention <- SS_output(here::here("model", "_bridging", "25_foreign_fleets_age_based_retention"))
#r4ss::tune_comps(
#  replist = retention, 
#  dir = here::here("model", "_bridging", "25_foreign_fleets_age_based_retention"),
#  option = "Francis")
#
#retention_w_devs <- SS_output(here::here("model", "_bridging", "24_foreign_fleets_age_based_retention"))
#r4ss::tune_comps(
#  replist = retention_w_devs, 
#  dir = here::here("model", "_bridging", "24_foreign_fleets_age_based_retention"),
#  option = "Francis")

modelnames <- c(
  "2023 Base Model",
  "Ageing Error",
  "Include AFSC Slope Survey")
mysummary <- SSsummarize(list(
  model_2023,
  ageing_error,
  afsc_slope))
SSplotComparisons(mysummary,
                  filenameprefix = "23_comparison",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

modelnames <- c(
  "+ Include AFSC Slope Survey",
  "+ 2023 Triennial Set-up",
  "+ 2023 Rec. Dev. Set-up",
  "+ Main Period = 1975 & No Early Devs.",
  "+ Discard Fleets",
  "+ Steepness",
  "+ Single M",
  "+ Weight-at-Age",
  "+ Environmental Index")
mysummary <- SSsummarize(list(
  afsc_slope,
  triennial,
  rec_devs_2023,
  rec_devs_1975_no_early,
  discard_fleets,
  steepness,
  sex_specific_m,
  watage, 
  base))
SSplotComparisons(mysummary,
                  filenameprefix = "24-30_base_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

modelnames <- c(
  #"+ Foreign Fleets Age-Based Retention",
  "+ 2023 Rec Dev Set-up",
  "+ Main Period = 1975",
  "+ Main Period = 1975 & No Early Devs.",
  "+ Discard Fleets",
  "+ Steepness",
  "+ Single M",
  "+ Weight-at-Age",
  "+ Environmental Index")
mysummary <- SSsummarize(list(
  #retention,
  rec_devs_2023,
  rec_devs_1975,
  rec_devs_1975_no_early,
  discard_fleets,
  steepness,
  sex_specific_m,
  watage, 
  base))
SSplotComparisons(mysummary,
                  filenameprefix = "25-30_base_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


# Discard fleet model with 2023 rec. dev. set-up, AFSC slope survey and 2023 Triennial survey
discard_fleets <- SS_output(here::here("model", "_bridging", "23_discard_fleets_age_based_selectivity_2023_rec_devs_2023_afsc_triennial"))
# Discard fleet model with 2023 rec. dev. set-up and 2023 Triennial survey
remove_afsc_slope <- SS_output(here::here("model", "_bridging", "24_discard_fleets_age_based_selectivity_2023_rec_devs_2023_triennial"))
# Discard fleet model with 2023 rec. dev. set-up with split Triennial survey
split_triennial <- SS_output(here::here("model", "_bridging", "25_discard_fleets_age_based_selectivity_2023_rec_devs_2023_split_triennial"))
# Discard fleet model with main rec. devs. starting in 1975 with split Triennial survey
main_devs <- SS_output(here::here("model", "_bridging", "25_discard_fleets_age_based_selectivity_main_devs_1975"))
# Discard fleet model with main rec. devs. starting in 1975 and no early devs with split Triennial survey
no_early_devs <- SS_output(here::here("model", "_bridging", "26_discard_fleets_age_based_selectivity_main_devs_1975_no_early"))

steepness <- SS_output(here::here("model", "_bridging", "27_steepness"))
sex_specific_m <- SS_output(here::here("model", "_bridging", "28_single_m"))
watage <- SS_output(here::here("model", "_bridging", "29_add_watage"))
base <- SS_output(here::here("model", "_bridging", "30_8.33_enviro_index_mle"))


modelnames <- c(
  "Ageing Error & Length-Based Retention & Age-Based Retention",
  "Age-Based Retention & Selectivity",
  "Discard Fleets",
  "Remove AFSC Slope Survey",
  "Split Triennial Survey",
  "Main Rec. Devs = 1975",
  "No Early Rec. Devs. & Main Rec. Devs = 1975",
  "Steepness = 0.72",
  "Single M",
  "Weight-at-Age",
  "2025 Base Model")
mysummary <- SSsummarize(list(
  ageing_error,
  retention_ages,
  discard_fleets,
  remove_afsc_slope,
  split_triennial,
  main_devs,
  no_early_devs,
  steepness,
  sex_specific_m,
  watage, 
  base))
SSplotComparisons(mysummary,
                  filenameprefix = "23-30_base_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

# OLD RUNS =====================================================================
#===============================================================================
# 14. Biology
#================================================================================

biology <- SS_output(here::here("model", "_bridging", "14_biology"))
modelnames <- c(
  "2023 Base", 
  "+ WCGBT",
  "+ Maturity",
  "+ M Prior",
  "+ Update wat parameters")
mysummary <- SSsummarize(list(
  model_2023,
  wcgbt,
  maturity,
  m_prior,
  biology))
SSplotComparisons(mysummary,
                  filenameprefix = "0_14_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 14. Age-based selectivity
#================================================================================

cli::cli_abort("The age-based model needs to be converted by hand.")
new_dir <- "14l_retention_selex_discard"
#age_based <- r4ss::SS_output(here::here("model", "_bridging", "14k_retention_HL_PT_TW_addParm"))
age_based <- r4ss::SS_output(here::here("model", "_bridging", new_dir))

# r4ss is not able to read the age-based model
# Error in 1:ctllist[["N_tag_groups"]] : argument of length 0
#ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
dw <- tune_comps(replist = age_based, fleet = 1:3, option = "Francis")
#colnames(dw) <- colnames(ctl$Variance_adjustment_list)
#ctl$Variance_adjustment_list <- dplyr::bind_rows(
#  dw,
#  ctl$Variance_adjustment_list |> dplyr::filter(Fleet != 7)
#)
#SS_writectl(
#  ctllist = ctl, 
#  outfile = here::here("model", "_bridging", new_dir, ctl_file),
#  overwrite = TRUE)

# Hand adjust the data weights
shell("ss3 -nohess")
age_based  <- SS_output(here::here("model", "_bridging", new_dir))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope",
  "+ NWFSC Slope", 
  "+ WCGBT",
  "+ Maturity",
  "+ M Prior", 
  "+ Age-based selex, ret, discard")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope,
  nwfsc_slope,
  wcgbt,
  maturity,
  m_prior,
  age_based))
SSplotComparisons(mysummary,
                  filenameprefix = "0_14_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

plot_year_selex(replist = age_based, fleets = 4:7, year = 1890)
plot_year_selex(replist = age_based, fleets = 1:3, year = 1942)
plot_year_selex(replist = age_based, fleets = 1:3, year = 2010)
plot_year_selex(replist = age_based, fleets = 1:3, year = 2011)
plot_year_selex(replist = age_based, fleets = 1:3, year = 2019)
r4ss::SS_plots(age_based)

#===============================================================================
# 14. Remove pot blocks on retention
#================================================================================

# new_dir <- "14m_retention_selex_discard_pot_blocks"
# pot <- r4ss::SS_output(here::here("model", "_bridging", new_dir))
# plot_year_selgroex(replist = pot, fleets = 1:3, year = 1890)
# plot_year_selex(replist = pot, fleets = 1:3, year = 1942)
# plot_year_selex(replist = pot, fleets = 1:3, year = 2010)
# plot_year_selex(replist = pot, fleets = 1:3, year = 2011)
# plot_year_selex(replist = pot, fleets = 1:3, year = 2019)

# modelnames <- c(
#   "2023 Base", 
#   "Simplify Ret./Selex. Blocks",
#   "+ Triennial",
#   "+ AFSC Slope",
#   "+ NWFSC Slope", 
#   "+ WCGBT",
#   "+ Maturity",
#   "+ M Prior", 
#   "+ Age-based selex, ret, discard", 
#   "- Remove pot ret. blocks")
# mysummary <- SSsummarize(list(
#   model_2023,
#   blocks,
#   triennial,
#   akfsc_slope,
#   nwfsc_slope,
#   wcgbt,
#   maturity,
#   m_prior,
#   age_based,
#   pot))
#  SSplotComparisons(mysummary,
#                    filenameprefix = "0_14_1_",
#                    legendlabels = modelnames, 	
#                    plotdir = here::here("model", "_bridging", "_plots"),
#                    ylimAdj = 1.5,
#                    pdf = TRUE)

#===============================================================================
# 15 Remove discard weights
#================================================================================

remove_discard_weights <- SS_output(here::here("model", "_bridging", "15_remove_discard_weights"))

modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Triennial",
  "+ AFSC Slope",
  "+ NWFSC Slope", 
  "+ WCGBT",
  "+ Maturity",
  "+ M Prior", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  triennial,
  akfsc_slope,
  nwfsc_slope,
  wcgbt,
  maturity,
  m_prior,
  age_based,
  remove_discard_weights))
SSplotComparisons(mysummary,
                  filenameprefix = "0_15_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(remove_discard_weights)

#===============================================================================
# 16 Add catch fleets
#================================================================================

add_catch_fleets <- SS_output(here::here("model", "_bridging", "16_catch_fleets"))
modelnames <- c(
  "2023 Base", 
  "+ M Prior", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets")
mysummary <- SSsummarize(list(
  model_2023,
  m_prior,
  age_based,
  remove_discard_weights,
  add_catch_fleets))
SSplotComparisons(mysummary,
                  filenameprefix = "0_16_",
                  legendlabels = modelnames, 
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(add_catch_fleets)
# The model unexpectedly resulted in the estimate of female Lmin decreasing to
# 57.5 cm from 59.5 cm and male Lmin decreasing to 53.7 cm from 54.9 cm.

plot_year_selex(
  replist = male_selex,
  fleets = 9,
  year = 2024)

#===============================================================================
# 16 Add catch fleets w/ wcgbt length data
#================================================================================

add_catch_fleets_alt <- SS_output(here::here("model", "_bridging", "16_catch_fleets_w_wcgbt_len"))

modelnames <- c(
  "+ Foreign Catch Fleets w/o WCGBT Lengths",
  "+ Foreign Catch Fleets w/ WCGBT Lengths")
mysummary <- SSsummarize(list(
  add_catch_fleets,
  add_catch_fleets_alt))
SSplotComparisons(mysummary,
                  filenameprefix = "0_16_only_",
                  legendlabels = modelnames, 
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 17 Fix selectivity bounds
#================================================================================
selex_bounds <- SS_output(here::here("model", "_bridging", "17_fix_selex_bounds_fix_cv"))
modelnames <- c(
  "2023 Base", 
  "+ M Prior", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets",
  "+ Fix Selectivity Bounds")
mysummary <- SSsummarize(list(
  model_2023,
  m_prior,
  age_based,
  remove_discard_weights,
  add_catch_fleets,
  selex_bounds))
SSplotComparisons(mysummary,
                  filenameprefix = "0_17_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(selex_bounds)

#===============================================================================
# 18 Fix male selectivity 
#================================================================================

male_selex <- SS_output(here::here("model", "_bridging", "18_fix_male_selex"))
male_selex <- SS_output(here::here("model", "_bridging", "18_fix_male_selex_w_wcgbt_len"))

r4ss::tune_comps(
  replist = male_selex, 
  dir = here::here("model", "_bridging", "18_fix_male_selex"),
  option = "Francis")


modelnames <- c(
  "2023 Base", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets",
  "+ Fix Selectivity Bounds",
  "+ Fix Male Selectivity")
mysummary <- SSsummarize(list(
  model_2023,
  age_based,
  remove_discard_weights,
  add_catch_fleets,
  selex_bounds, 
  male_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "0_18_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
plot_ghostfleets(replist = male_selex)
plot_age_fits_sexed_only(replist = male_selex)
SS_plots(male_selex)

plot_year_selex(
  replist = male_selex,
  fleets = 3,
  year = 2024)
plot_fleet_selectivity(
  model_out = male_selex, 
  fleet_num = 1)

model_mod <- male_selex
model_mod$agedbase <- model_mod$agedbase |> 
  dplyr::filter(Sexes == 3, Fleet == 3) 
SSplotComps(
  replist = model_mod,
  subplots = 21,
  kind = "AGE"
)

#===============================================================================
# 19.0 Rec Devs
#===============================================================================

# The previous model was not inverting the hessian due to a rec dev parameter, 
# so trying to fix that.
rec_devs <- SS_output(here::here("model", "_bridging", "19.0_rec_devs"))

modelnames <- c(
  "2023 Base", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets",
  "+ Fix Selectivity Bounds",
  "+ Fix Male Selectivity",
  "+ Rec. Devs.")
mysummary <- SSsummarize(list(
  model_2023,
  age_based,
  remove_discard_weights,
  add_catch_fleets,
  selex_bounds, 
  male_selex,
  rec_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "0_19_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
plot_ghostfleets(replist = rec_devs)
plot_age_fits_sexed_only(replist = rec_devs)
SS_plots(rec_devs)

# The recommended data weights have changed substantially
# Params with very high uncertainty
# Age_DblN_ascend_se_HKL(2)
# Age_DblN_descend_se_NWFSC_Slope(9) 

# parameters_with_highest_gradients
#                                Value Gradient
# SR_LN(R0)                  9.8142600  7.38112 !!!!!!!!
# Age_DblN_peak_Pot(3)       2.1213900 -4.67087
# Age_DblN_peak_TWL(1)       0.9864610 -3.49623
# NatM_uniform_Fem_GP_1      0.0574724 -1.52421
# Age_DblN_end_logit_TWL(1) -1.0265100  1.49920

#===============================================================================
# 19.1 Rec Devs
#===============================================================================

# Turn off early devs, extend main devs, and fix select params with high sd
rec_devs_2 <- SS_output(here::here("model", "_bridging", "19.1_rec_devs_early_main"))
SS_plots(rec_devs_2)


#===============================================================================
# 19.1 Rec Devs w/ WCGBT lengths
#===============================================================================

# Turn off early devs, extend main devs, and fix select params with high sd
rec_devs <- SS_output(here::here("model", "_bridging", "19.1_rec_devs_early_main_wcgbt_len"))
SS_plots(rec_devs)
# Final gradient: 2.66004 is larger than final_conv: 0.001
# M ~ 0.063 and 0.062
# Lmax ~ 59.7 and 54.8

# Selex WCGBT peak near lower bound

r4ss::tune_comps(
  replist = rec_devs, 
  dir = here::here("model", "_bridging", "19.1_rec_devs_early_main_wcgbt_len"),
  option = "Francis")

#===============================================================================
# 20.0 Data - update ageing error and at-sea catch for 2024
#================================================================================

ageing_error <- SS_output(here::here("model", "_bridging", "20.0_data_ageing_error_atsea_catch"))
SS_plots(ageing_error)

modelnames <- c(
  "2023 Base", 
  "+ Age-based selex, ret, discard", 
  "+ Foreign Catch Fleets",
  "+ Fix Male Selectivity",
  "+ Rec. Devs.",
  "+ Ageing Error & At-Sea 2024")
mysummary <- SSsummarize(list(
  model_2023,
  age_based,
  add_catch_fleets,
  male_selex,
  rec_devs,
  ageing_error))
SSplotComparisons(mysummary,
                  filenameprefix = "0_20_data_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 20.1 Data - remove unsexed fish from the slope survey
#===============================================================================

unsexed_fish <- SS_output(here::here("model", "_bridging", "20.1_data_remove_unsexed_slope"))
SS_plots(unsexed_fish)
plot_ghostfleets(replist = unsexed_fish)

#===============================================================================
# 20.2 Data - combine sexed below 28 cm and age-1
#================================================================================

combine_sexes <- SS_output(here::here("model", "_bridging", "20.2_data_combine_bins"))
SS_plots(combine_sexes)
plot_ghostfleets(replist = combine_sexes)

#===============================================================================
# 20.3 Param - use a single M value
#================================================================================

single_m <- SS_output(here::here("model", "_bridging", "20.3_param_single_m"))
SS_plots(single_m)

r4ss::tune_comps(
  replist = single_m, 
  dir = here::here("model", "_bridging", "20.3_param_single_m"),
  option = "Francis")


#===============================================================================
# Summary
#================================================================================

# Fix male selex
# gradient = 0.49
# NLL = 2476.57
# R0 = 9.27684

# Rec devs
# gradient = 2.66004
# NLL = 2486.5 (Age = 2566.99)
# R0 = 10.0099

# Ageing Error
# gradient = 0.138698
# NLL = 2301.96 (Age = 2384.57)
# R0 = 10.2508

# Remove unsexed in slope survey
# gradient = 0.540945
# NLL = 2296.37 
# R0 = 10.2498

# Combine below bins
# gradient = .624544
# NLL = 2288.23
# R0 = 10.2643
# growth estimates are nearly identical

# Single M
# gradient = 0.275216
# NLL =  2288.41
# R0 = 10.225 

#===============================================================================
# 20.5 Rec Dev Option
#================================================================================

add_ages <- SS_output(here::here("model", "_bridging", "20.5_data_add_ages"))
add_ages_dev2 <- SS_output(here::here("model", "_bridging", "20.5_data_add_ages_rec_dev_2"))

modelnames <- c(
  "2023 Base", 
  "+ Age-based selex, ret, discard", 
  "+ Foreign Catch Fleets",
  "+ Fix Male Selectivity",
  "+ Rec. Devs.",
  "+ Ageing Error & At-Sea 2024",
  "+ Add Recent Ages",
  "+ Rec. Dev. Option 2")
mysummary <- SSsummarize(list(
  model_2023,
  age_based,
  add_catch_fleets,
  male_selex,
  rec_devs,
  ageing_error,
  add_ages, 
  add_ages_dev2))
SSplotComparisons(mysummary,
                  filenameprefix = "0_20.5_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


modelnames <- c(
  "+ Rec. Dev. Option 1",
  "+ Rec. Dev. Option 2")
mysummary <- SSsummarize(list(
  add_ages, 
  add_ages_dev2))
SSplotComparisons(mysummary,
                  filenameprefix = "0_20.5_devs_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
#===============================================================================
# 20.4 Data Weight
#================================================================================

data_weight <- SS_output(here::here("model", "_bridging", "20.4_data_weight"))
data_weight <- SS_output(here::here("model", "_bridging", "20.4_data_weight_hessian"))

#===============================================================================
# Compare models
#===============================================================================

modelnames <- c(
  "2023 Base", 
  "+ Fix Male Selectivity",
  "+ Rec. Devs.",
  "+ Ageing Error",
  "+ Remove Slope Survey Unsexed Fish",
  "+ Combine Below Bins",
  "+ Single M",
  "+ Data Weight")
mysummary <- SSsummarize(list(
  model_2023,
  male_selex,
  rec_devs,
  ageing_error,
  unsexed_fish,
  combine_sexes, 
  single_m,
  data_weight))
SSplotComparisons(mysummary,
                  filenameprefix = "0_20_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 20.6 Reduce discard input sample size
#===============================================================================

discard_input_n <- SS_output(here::here("model", "_bridging", "20.6_reduce_discard_input_n"))
SS_plots(discard_input_n)

#===============================================================================
# 20.5 Remove early surveys
#================================================================================

early_surveys <- SS_output(here::here("model", "_bridging", "20.5_remove_early_surveys"))

modelnames <- c(
  "20.4 Data Weight",
  "20.5 Remove Early Surveys")
mysummary <- SSsummarize(list(
  data_weight,
  early_surveys))
SSplotComparisons(mysummary,
                  filenameprefix = "0_20.5_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#===============================================================================
# 21.0 Fix growth
#===============================================================================

fix_growth <- SS_output(here::here("model", "_bridging", "21.0_fix_growth_mle"))
SS_plots(fix_growth)
plot_age_fits_sexed_only(
  replist = fix_growth)
# NLL = 2006.7
# R0 = 9.924
# gradient = 0.000916592
# wants to upweight survey age data (expcept Triennial) and downweight fishery age data

#===============================================================================
# 21.1_hkl_selex
#===============================================================================

hkl_blocks <- SS_output(here::here("model", "_bridging", "21.1_hkl_selex"))
# apply block to peak, top, and ascending 2011 2018 2019 2024
# 6 extra parameters

# NLL = 1994.23
#      1890-2010   2011-2018 2019+
# peak 4.1019700, 3.3811100, 4.1711700
# top -3.99597,  -9.9999100, 5.8584
# asc 1.34044,     0.345367, 1.0822700

plot_year_selex(
 replist = hkl_blocks,
 fleets = 2,
 year = 1890)
plot_year_selex(
  replist = hkl_blocks,
  fleets = 2,
  year = 2011)
plot_year_selex(
  replist = hkl_blocks,
  fleets = 2,
  year = 2019)
plot_age_fits_sexed_only(
  replist = hkl_blocks,
  years = NULL)

# Does not improve the visual fits to the aggregated composition data

#===============================================================================
# 21.2_plot_selex
#===============================================================================

pot_blocks <- SS_output(here::here("model", "_bridging", "21.2_pot_selex"))
# apply block to peak, top, and ascending 2011 2018 2019 2024
# 6 extra parameters

# NLL = 1981.57 vs. 2006.7
#      1890-2010   2011-2018 2019+
# peak 2.6625500, 3.2089800, 3.6907500
# top -3.5680400, -5.980750, 1.0472200
# asc -0.4918930, 0.0863965, 0.2591460

plot_year_selex(
  replist = pot_blocks,
  fleets = 3,
  year = 1890)
plot_year_selex(
  replist = pot_blocks,
  fleets = 3,
  year = 2011)
plot_year_selex(
  replist = pot_blocks,
  fleets = 3,
  year = 2019)
plot_age_fits_sexed_only(
  replist = pot_blocks,
  years = NULL)

# Does not improve the visual fits to the aggregated composition data

#===============================================================================
# Only block the peak parameter for the hkl and pot fleets
#===============================================================================
# NLL = 2006.7 no blocks (21.0_fix_growth_mle)
# NLL = 1995.79 hkl peak blocked 2011-2018 vs. NLL = 1994.23 for peak, top, asc
# NLL = 1987.66 pot peak blocked 2011-2018 and 2019+ vs. NLL = 1981.57 for peak, top, asc

#===============================================================================
# 21.5_hkl_pot_male_offset w/ blocks on peak for hkl (1) and pot (2) for males and females
#===============================================================================
male_offset <- SS_output(here::here("model", "_bridging", "21.5_hkl_pot_male_offset"))
# NLL = 1989.79
plot_age_fits_sexed_only(
  replist = male_offset)
SS_plots(male_offset, plot = c(2:5, 17))
plot_year_selex(
  replist = male_offset,
  fleets = 2:3,
  year = 1890)
# Does not improve the visual fits to the aggregated composition data

#===============================================================================
# 21.6_hkl_pot_splines
#===============================================================================
splines <- SS_output(here::here("model", "_bridging", "21.6_hkl_pot_splines"))
# NLL = 2068.94
# HKL LL =  226.5 (no blocks)
plot_age_fits_sexed_only(
  replist = splines)
plot_age_fits_sexed_only(
  replist = splines,
  years = 2011:2024)
plot_age_fits_sexed_only(
  replist = splines,
  years = 1890:2010)
