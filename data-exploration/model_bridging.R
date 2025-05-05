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
  "Simplify Ret./Selex. Blocks",
  "+ Survey Data",
  "+ Maturity",
  "+ M Prior", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  wcgbt,
  maturity,
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
# 17 Fix selectivity bounds
#================================================================================
selex_bounds <- SS_output(here::here("model", "_bridging", "17_fix_selex_bounds"))
modelnames <- c(
  "2023 Base", 
  "Simplify Ret./Selex. Blocks",
  "+ Survey Data",
  "+ Maturity",
  "+ M Prior", 
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets",
  "+ Fix Selectivity Bounds")
mysummary <- SSsummarize(list(
  model_2023,
  blocks,
  wcgbt,
  maturity,
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

r4ss::tune_comps(
  replist = male_selex, 
  dir = here::here("model", "_bridging", "18_fix_male_selex"),
  option = "Francis")

male_selex <- SS_output(here::here("model", "_bridging", "18_fix_male_selex"))
modelnames <- c(
  "2023 Base", 
  "+ Survey Data",
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets",
  "+ Fix Selectivity Bounds",
  "+ Fix Male Selectivity")
mysummary <- SSsummarize(list(
  model_2023,
  wcgbt,
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

#===============================================================================
# 19 Weight-at-Age 
#================================================================================
watage <- SS_output(here::here("model", "_bridging", "19_weight_at_age"))

r4ss::tune_comps(
  replist = watage, 
  dir = here::here("model", "_bridging", "19_weight_at_age"),
  option = "Francis")

watage <- SS_output(here::here("model", "_bridging", "19_weight_at_age"))
modelnames <- c(
  "2023 Base", 
  "+ Survey Data",
  "+ Age-based selex, ret, discard", 
  "- Remove discard weights",
  "+ Foreign Catch Fleets",
  "+ Fix Selectivity Bounds",
  "+ Fix Male Selectivity",
  "+ Weight-at-Age")
mysummary <- SSsummarize(list(
  model_2023,
  wcgbt,
  age_based,
  remove_discard_weights,
  add_catch_fleets,
  selex_bounds, 
  male_selex,
  watage))
SSplotComparisons(mysummary,
                  filenameprefix = "0_19_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
plot_ghostfleets(replist = watage)
plot_age_fits_sexed_only(replist = watage)
SS_plots(watage)
