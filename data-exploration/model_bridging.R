library(r4ss)

copy_files <- function(x, from_name, to_name){
  file.copy(
    from = here::here("model", "_bridging", from_name,  x),
    to = here::here("model", "_bridging", to_name,  x)
  )
}
data_file <- "data.ss"
ctl_file <- "control.ss"
forcast_file <- "forecast.ss"

# 1. Revise the fleet numbering
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

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
fleet_numbering <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch")
mysummary <- SSsummarize(list(model_2023, fleet_numbering))
SSplotComparisons(mysummary,
                  filenameprefix = "0_1_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  pdf = TRUE)

# 2. Remove environmental index
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
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
# Need to add lambda line by hand

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
rm_enviro <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro))
SSplotComparisons(mysummary,
                  filenameprefix = "0_2_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
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
add_landings <- SS_output(getwd())
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

# 4. Update and extend fishery age data data
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "4_fishery_ages"
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
  dat$agecomp |> dplyr::filter(!fleet %in% 1:2)) 

SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")

add_fishery_ages <- SS_output(getwd())
dw <- tune_comps(replist = add_fishery_ages, fleet = 1:2, option = "Francis")
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
ctl$Variance_adjustment_list[
  which(ctl$Variance_adjustment_list$Data_type == 5 & ctl$Variance_adjustment_list$Fleet %in% 1:2), "Value"] <-
  rev(dw[which(dw$`#Data_type` == 5), "New_Francis"])
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)
shell("ss3 -nohess")
add_fishery_ages <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Fishery Ages (DW)")
mysummary <- SSsummarize(list(model_2023, fleet_numbering, rm_enviro, add_landings, add_fishery_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "0_4_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  pdf = TRUE)

#=============================================================================================
# 5. Discard data
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "5_discard_data"
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

discard_lens <- read.csv(here::here("data-processed", "data_commercial_discard_composition.csv")) |>
  dplyr::mutate(
    fleet = dplyr::case_when(fleet == 3 ~ 2, .default = fleet)
  ) 
dat$discard_data <- discard_rates_comb
dat$meanbodywt <- discard_wght_comb
colnames(discard_lens) <- colnames(dat$lencomp)
dat$lencomp <- dplyr::bind_rows(
  discard_lens,
  dat$lencomp[dat$lencomp$fleet == 7, ]
)
SS_writedat(
  datlist = dat, 
  outfile = here::here("model", "_bridging", new_dir, data_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
add_discard <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "Fleet Number Switch",
  "Rm. Enviro. Index",
  "+ Landings",
  "+ Fishery Ages (DW)",
  "+ Discard Data")
mysummary <- SSsummarize(list(
  model_2023, fleet_numbering, rm_enviro, add_landings, add_fishery_ages, add_discard))
SSplotComparisons(mysummary,
                  filenameprefix = "0_5_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  pdf = TRUE)
SS_plots(add_discard)

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
dat$fleetinfo$fleetname[3] <- "Pot"
dat$catch <- read.csv(here::here("data-processed", "data_commercial_catch_cw.csv")) |>
  dplyr::rename(catch = catch_mt)
  
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

discard_lens <- read.csv(here::here("data-processed", "data_commercial_discard_composition.csv")) 
colnames(discard_lens) <- colnames(dat$lencomp)
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

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
split_fleets <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "+ Landings",
  "+ Fishery Ages (DW)",
  "+ Discard Data",
  "+ Split Fixed Gears")
mysummary <- SSsummarize(list(
  model_2023, add_landings, add_fishery_ages, add_discard, split_fleets))
SSplotComparisons(mysummary,
                  filenameprefix = "0_6_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  pdf = TRUE)
SS_plots(split_fleets)
tune_comps(replist = split_fleets, option = "Francis")

split_fleets_dw <- SS_output(here::here("model", "_bridging", "7_split_fixed_gears_dw"))
modelnames <- c(
  "2023 Base", 
  "+ Landings",
  "+ Fishery Ages (DW)",
  "+ Discard Data",
  "+ Split Fixed Gears (DW)")
mysummary <- SSsummarize(list(
  model_2023, add_landings, add_fishery_ages, add_discard, split_fleets_dw))
SSplotComparisons(mysummary,
                  filenameprefix = "0_7_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


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
triennial <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "+ Landings",
  "+ Fishery Ages (DW)",
  "+ Discard Data",
  "+ Split Fixed Gears (DW)",
  "+ Triennial")
mysummary <- SSsummarize(list(
  model_2023, add_landings, add_fishery_ages, add_discard, split_fleets, triennial))
SSplotComparisons(mysummary,
                  filenameprefix = "0_8_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)


#=============================================================================================
# 9. AFSC Slope
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "8_akfsc_slope"
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

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
akfsc_slope <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "+ Landings",
  "+ Fishery Ages (DW)",
  "+ Discard Data",
  "+ Split Fixed Gears (DW)",
  "+ Triennial",
  "+ AKFSC Slope")
mysummary <- SSsummarize(list(
  model_2023, add_landings, add_fishery_ages, add_discard, split_fleets, triennial,
  akfsc_slope))
SSplotComparisons(mysummary,
                  filenameprefix = "0_9_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

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
nwfsc_slope <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "+ Landings",
  "+ Fishery Ages (DW)",
  "+ Discard Data",
  "+ Split Fixed Gears (DW)",
  "+ Triennial",
  "+ AKFSC Slope",
  "+ NWFSC Slope")
mysummary <- SSsummarize(list(
  model_2023, add_landings, add_fishery_ages, add_discard, split_fleets, triennial,
  akfsc_slope, nwfsc_slope))
SSplotComparisons(mysummary,
                  filenameprefix = "0_10_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

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
dat$lencomp <- dplyr::bind_rows(
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
wcgbt <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "+ Triennial",
  "+ AKFSC Slope",
  "+ NWFSC Slope",
  "+ WCGBT")
mysummary <- SSsummarize(list(model_2023, triennial, akfsc_slope, nwfsc_slope, wcgbt))
SSplotComparisons(mysummary,
                  filenameprefix = "0_11_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

#=============================================================================================
# 12. Data Weight
#=============================================================================================
old_dir <- new_dir
files <- list.files(here::here("model", "_bridging", old_dir))
new_dir <- "12_data_weight"
dir.create(here::here("model", "_bridging", new_dir))

copy_files(
  x = files, 
  from_name = old_dir, 
  to_name = new_dir)

dw <- tune_comps(replist = wcgbt, option = "Francis")[,1:3] 
ctl <- SS_readctl(file = here::here("model", "_bridging", new_dir, ctl_file))
colnames(dw) <- colnames(ctl$Variance_adjustment_list)
ctl$Variance_adjustment_list <- dplyr::bind_rows(
  ctl$Variance_adjustment_list |> dplyr::filter(Data_type == 3),
  dw
)
SS_writectl(
  ctllist = ctl, 
  outfile = here::here("model", "_bridging", new_dir, ctl_file),
  overwrite = TRUE)

setwd(here::here("model", "_bridging", new_dir))
shell("ss3 -nohess")
dw <- SS_output(getwd())
modelnames <- c(
  "2023 Base", 
  "+ Triennial",
  "+ AKFSC Slope",
  "+ NWFSC Slope",
  "+ WCGBT",
  "+ Update DW")
mysummary <- SSsummarize(list(model_2023, triennial, akfsc_slope, nwfsc_slope, wcgbt, dw))
SSplotComparisons(mysummary,
                  filenameprefix = "0_12_",
                  legendlabels = modelnames, 	
                  plotdir = here::here("model", "_bridging", "_plots"),
                  ylimAdj = 1.5,
                  pdf = TRUE)
SS_plots(dw)
