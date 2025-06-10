# Create sensitivity directories (Sablefish 2025) and running models
#
# Files are written to the disk inside the following directories:
# 01-remove-WCGBT
# 02-remove-Triennial-all
# 03-remove-Triennial-early
# 04-remove-Triennial-late
# 05-remove-NWFSC-slope
# 06-remove-enviro-index
# 07-MacIan-weight
# 08-2019-age-error
# 09-2019-maturity (not including anymore)
# 10-growth-model
# 11-M-Diffsex
# 12-M-AK
# 13-M-adjust-migration
# 14-M-Fix-2019Assess
# 15-NWsurveys-Asymptotic
# 16-Est-steepness
# 17-RecDev-sum-zero
# 18-Est-RecDev-early-sum-zero
# 19-2yr-enviro-index

library(r4ss)
library(here)
library(dplyr)

model_directory <- 'model'
base_folder <- 'base_model'
base_model_name <- '8.36_base_model'
exe_loc <- here('model/ss3.exe')
base_loc <- file.path(here(),model_directory, base_folder,base_model_name)
base_model <- SS_read(base_loc, ss_new = TRUE)
base_out <- SS_output(base_loc,
  SpawnOutputLabel = "Spawning output (trillions of eggs)",
  printstats = FALSE,
  verbose = FALSE
)

#############Set up model files/folders##################
sens_loc <- file.path(here(),model_directory, base_folder, '_sensitivities')

# 01-remove-WCGBT (index and ages)
sensi_mod <- base_model
sensi_mod$ctl$lambdas <- sensi_mod$ctl$lambdas |>
  bind_rows(data.frame(
    like_comp = 1, fleet = 10, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
    like_comp = 5, fleet = 10, phase = 1, value = 0, sizefreq_method = 0))
sensi_mod$ctl$N_lambdas <- nrow(sensi_mod$ctl$lambdas)

SS_write(sensi_mod, file.path(sens_loc,'01-remove-WCGBT'),
         overwrite = TRUE)
r4ss::run(dir = file.path(sens_loc,'01-remove-WCGBT'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 02-remove-Triennial-all (index and ages for late AND early periods)
sensi_mod <- base_model
sensi_mod$ctl$lambdas <- sensi_mod$ctl$lambdas |>
  bind_rows(data.frame(
    like_comp = 1, fleet = 7, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
      like_comp = 5, fleet = 7, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
      like_comp = 1, fleet = 8, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
      like_comp = 5, fleet = 8, phase = 1, value = 0, sizefreq_method = 0))
sensi_mod$ctl$N_lambdas <- nrow(sensi_mod$ctl$lambdas)

SS_write(sensi_mod, file.path(sens_loc,'02-remove-Triennial-all'),
         overwrite = TRUE)
r4ss::run(dir = file.path(sens_loc,'02-remove-Triennial-all'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 03-remove-Triennial-early (index and ages)
sensi_mod <- base_model
sensi_mod$ctl$lambdas <- sensi_mod$ctl$lambdas |>
  bind_rows(data.frame(
    like_comp = 1, fleet = 7, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
      like_comp = 5, fleet = 7, phase = 1, value = 0, sizefreq_method = 0))
sensi_mod$ctl$N_lambdas <- nrow(sensi_mod$ctl$lambdas)

SS_write(sensi_mod, file.path(sens_loc,'03-remove-Triennial-early'),
         overwrite = TRUE)
r4ss::run(dir = file.path(sens_loc,'03-remove-Triennial-early'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 04-remove-Triennial-late (index and ages)
sensi_mod <- base_model
sensi_mod$ctl$lambdas <- sensi_mod$ctl$lambdas |>
  bind_rows(data.frame(
    like_comp = 1, fleet = 8, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
      like_comp = 5, fleet = 8, phase = 1, value = 0, sizefreq_method = 0))
sensi_mod$ctl$N_lambdas <- nrow(sensi_mod$ctl$lambdas)

SS_write(sensi_mod, file.path(sens_loc,'04-remove-Triennial-late'),
         overwrite = TRUE)
r4ss::run(dir = file.path(sens_loc,'04-remove-Triennial-late'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 05-remove-NWFSC-slope (index and ages)
sensi_mod <- base_model
sensi_mod$ctl$lambdas <- sensi_mod$ctl$lambdas |>
  bind_rows(data.frame(
    like_comp = 1, fleet = 9, phase = 1, value = 0, sizefreq_method = 0),
    data.frame(
      like_comp = 5, fleet = 9, phase = 1, value = 0, sizefreq_method = 0))
sensi_mod$ctl$N_lambdas <- nrow(sensi_mod$ctl$lambdas)

SS_write(sensi_mod, file.path(sens_loc,'05-remove-NWFSC-slope'),
         overwrite = TRUE)
r4ss::run(dir = file.path(sens_loc,'05-remove-NWFSC-slope'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 06-remove-enviro-index
sensi_mod <- base_model
sensi_mod$ctl$lambdas <- sensi_mod$ctl$lambdas |>
  bind_rows(data.frame(
    like_comp = 1, fleet = 11, phase = 1, value = 0, sizefreq_method = 0))
sensi_mod$ctl$N_lambdas <- nrow(sensi_mod$ctl$lambdas)

SS_write(sensi_mod, file.path(sens_loc,'06-remove-enviro-index'),
         overwrite = TRUE)
r4ss::run(dir = file.path(sens_loc,'06-remove-enviro-index'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 07-MacIan-weight
copy_SS_inputs(dir.old = base_loc, 
               dir.new = file.path(sens_loc,'07-MacIan-weight'))
file.copy(from = file.path(base_loc,
                           c('Report.sso', 'CompReport.sso', 'warning.sso')), 
          to = file.path(sens_loc,'07-MacIan-weight', 
                         c('Report.sso', 'CompReport.sso', 'warning.sso')), 
          overwrite = TRUE)
tune_comps(option = 'MI', niters_tuning = 3, 
           dir = file.path(sens_loc,'07-MacIan-weight'),
           #extras = '-nohess',
           exe = exe_loc)

# 08-2019-age-error
sensi_mod <- base_model
sensi_mod$dat$agecomp$ageerr <- replace(sensi_mod$dat$agecomp$ageerr,sensi_mod$dat$agecomp$ageerr==1,2)

SS_write(sensi_mod, file.path(sens_loc,'08-2019-age-error'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'08-2019-age-error'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 09-2019-maturity (decided not to do b/c using wt-at-age)
#sensi_mod <- base_model
#sensi_mod$dat$agecomp$ageerr <- replace(sensi_mod$dat$agecomp$ageerr,sensi_mod$dat$agecomp$ageerr==1,2)

#SS_write(sensi_mod, file.path(sens_loc,'09-2019-maturity'),
#         overwrite = TRUE)

#r4ss::run(dir = file.path(sens_loc,'09-2019-maturity'),
#          exe = exe_loc,
#          extras = "-no hess",
#          verbose = TRUE,
#          )

# 10-growth-model (this is not a coded model run given the extensive and complex format changes)
#
#
# 11-M-Diffsex
sensi_mod <- base_model
sensi_mod$ctl$MG_parms['NatM_p_1_Mal_GP_1', names(sensi_mod$ctl$MG_parms)] <-
  sensi_mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', names(sensi_mod$ctl$MG_parms)]
sensi_mod$start$init_values_src <- 0

SS_write(sensi_mod, file.path(sens_loc,'11-M-Diffsex'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'11-M-Diffsex'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 12-M-AK
sensi_mod <- base_model
sensi_mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('INIT', 'PHASE')] <- c(0.114, -99)
sensi_mod$ctl$MG_parms['NatM_p_1_Mal_GP_1', c('INIT', 'PHASE')] <- c(0.114, -99)
sensi_mod$ctl$SR_parms['SR_LN(R0)', c('INIT')] <- 11
sensi_mod$start$init_values_src <- 0

SS_write(sensi_mod, file.path(sens_loc,'12-M-AK'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'12-M-AK'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 13-M-adjust-migration (fix M at estimate plus add 0.0438 to account for emigration from Kapur et al. 2025 paper)
sensi_mod <- base_model
M_emigration <- 0.0438
M_base <- base_out$parameters['NatM_uniform_Fem_GP_1','Value'] 
sensi_mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('HI', 'INIT', 'PHASE')] <- c(0.15, M_base+M_emigration, -99)
sensi_mod$start$init_values_src <- 0

SS_write(sensi_mod, file.path(sens_loc,'13-M-adjust-migration'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'13-M-adjust-migration'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 14-M-Fix-2019Assess (fix M at 2019 assessment; female = 0.0759; male = 0.0675)
sensi_mod <- base_model
sensi_mod$ctl$MG_parms['NatM_p_1_Fem_GP_1', c('INIT', 'PHASE')] <- c(0.0759, -99)
sensi_mod$ctl$MG_parms['NatM_p_1_Mal_GP_1', c('INIT', 'PHASE')] <- c(0.0675, -99)
sensi_mod$ctl$SR_parms['SR_LN(R0)', c('INIT')] <- 11
sensi_mod$start$init_values_src <- 0


SS_write(sensi_mod, file.path(sens_loc,'14-M-Fix-2019Assess'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'14-M-Fix-2019Assess'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 15-NWsurveys-Asymptotic
sensi_mod <- base_model

sensi_mod$ctl$age_selex_parms['AgeSel_P_1_NWFSC_Slope(9)',c('PHASE')] <- 4
sensi_mod$ctl$age_selex_parms['AgeSel_P_2_NWFSC_Slope(9)',c('INIT', 'PHASE')] <- c(10, -99)
sensi_mod$ctl$age_selex_parms['AgeSel_P_3_NWFSC_Slope(9)',c('PHASE')] <- 4
sensi_mod$ctl$age_selex_parms['AgeSel_P_4_NWFSC_Slope(9)',c('INIT', 'PHASE')] <- c(-10, -99)
sensi_mod$ctl$age_selex_parms['AgeSel_P_5_NWFSC_Slope(9)',c('INIT', 'PHASE')] <- c(-10, -99)
sensi_mod$ctl$age_selex_parms['AgeSel_P_6_NWFSC_Slope(9)',c('INIT', 'PHASE')] <- c(10, -99)

sensi_mod$ctl$age_selex_parms['AgeSel_P_1_WCGBT(10)',c('PHASE')] <- 4
sensi_mod$ctl$age_selex_parms['AgeSel_P_2_WCGBT(10)',c('INIT', 'PHASE')] <- c(10, -99)
sensi_mod$ctl$age_selex_parms['AgeSel_P_3_WCGBT(10)',c('PHASE')] <- 4
sensi_mod$ctl$age_selex_parms['AgeSel_P_4_WCGBT(10)',c('INIT', 'PHASE')] <- c(-10, -99)
sensi_mod$ctl$age_selex_parms['AgeSel_P_5_WCGBT(10)',c('INIT', 'PHASE')] <- c(-10, -99)
sensi_mod$ctl$age_selex_parms['AgeSel_P_6_WCGBT(10)',c('INIT', 'PHASE')] <- c(10, -99)

SS_write(sensi_mod, file.path(sens_loc,'15-NWsurveys-Asymptotic'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'15-NWsurveys-Asymptotic'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 16-Est-steepness
sensi_mod <- base_model
sensi_mod$ctl$SR_parms['SR_BH_steep', c('PHASE')] <- 2

SS_write(sensi_mod, file.path(sens_loc,'16-Est-steepness'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'16-Est-steepness'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 17-RecDev-sum-zero
sensi_mod <- base_model
sensi_mod$ctl$do_recdev <- 1

SS_write(sensi_mod, file.path(sens_loc,'17-RecDev-sum-zero'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'17-RecDev-sum-zero'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 18-Est-RecDev-early-sum-zero
sensi_mod <- base_model
sensi_mod$ctl$do_recdev <- 1
sensi_mod$ctl$recdev_early_phase <- 3

SS_write(sensi_mod, file.path(sens_loc,'18-Est-RecDev-early-sum-zero'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'18-Est-RecDev-early-sum-zero'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )

# 19-2yr-enviro-index (adjust input data for fleet 11)
sensi_mod <- base_model
tmp <- read.csv(here::here("data-processed", "data_enviro_index_2yr.csv"))
sensi_mod$dat$CPUE <- sensi_mod$dat$CPUE[sensi_mod$dat$CPUE$index != 11,]
sensi_mod$dat$CPUE <- rbind(sensi_mod$dat$CPUE,tmp[,-1])

SS_write(sensi_mod, file.path(sens_loc,'19-2yr-enviro-index'),
         overwrite = TRUE)

r4ss::run(dir = file.path(sens_loc,'19-2yr-enviro-index'),
          exe = exe_loc,
          #extras = "-no hess",
          verbose = TRUE,
          )