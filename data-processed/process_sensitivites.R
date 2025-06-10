# Put sensitivity models into groupings and produce key figures for each grouping
#
library(r4ss)
library(here)
library(dplyr)

model_directory <- 'model'
base_folder <- 'base_model'
base_model_name <- '8.36_base_model'
base_loc <- file.path(here(),model_directory, base_folder,base_model_name)
sens_loc <- file.path(here(),model_directory, base_folder, '_sensitivities')
figs_loc <- file.path(here(),'report', 'figures')
tabs_loc <- file.path(here(),'report', 'rda')
source(file.path(here(),'R', 'write_comparison_table.R'))

#Group 1 (Data inputs)
modname <- c("Base Model","Remove WCGBT","Remove Tri","Remove NWFSC_Slope","Remove Env_Index")
mod_sum <- SSsummarize(
  list(
  SS_output(dir=file.path(base_loc)),
  SS_output(dir=file.path(sens_loc,'01-remove-WCGBT')),
  SS_output(dir=file.path(sens_loc,'02-remove-Triennial-all')),
  #SS_output(dir=file.path(sens_loc,'03-remove-Triennial-early')),
  #SS_output(dir=file.path(sens_loc,'04-remove-Triennial-late')),
  SS_output(dir=file.path(sens_loc,'05-remove-NWFSC-slope')),
  SS_output(dir=file.path(sens_loc,'06-remove-enviro-index'))
  #SS_output(dir=file.path(sens_loc,'19-2yr-enviro-index'))
  )
)
cat("Model convergence:")
mod_sum$maxgrad
SSplotComparisons(mod_sum,
                  subplots = 4,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  plotdir=figs_loc,
                  filenameprefix='sens_1_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 18,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  plotdir=figs_loc,
                  filenameprefix='sens_1_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 12,
                  xlim=c(1960,2025),
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "topleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_1_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 13,
                  indexfleets= 10,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "topleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_1_',
                  endyr=2025)

create_comparison_table(dir = tabs_loc,
                        model_summary = mod_sum,
                        model_names = modname,
                        add_name = "sens_1")

#Group 2 (Age comps)
modname <- c("Base Model","Mac-Ianelli Weighting","NW_Surveys Asymtotic Selectivity","Ageing Error 2019 Assessment")
mod_sum <- SSsummarize(
  list(
    SS_output(dir=file.path(base_loc)),
    SS_output(dir=file.path(sens_loc,'07-MacIan-weight')),
    SS_output(dir=file.path(sens_loc,'15-NWsurveys-Asymptotic')),
    SS_output(dir=file.path(sens_loc,'08-2019-age-error'))
  )
)
cat("Model convergence:")
mod_sum$maxgrad
SSplotComparisons(mod_sum,
                  subplots = 4,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "bottomleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_2_',
                  btarg = 0.4,
                  minbthresh = 0.25,
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 18,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "bottomleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_2_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 12,
                  xlim=c(1960,2025),
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "topleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_2_',
                  endyr=2025)

create_comparison_table(dir = tabs_loc,
                        model_summary = mod_sum,
                        model_names = modname,
                        add_name = "sens_2")

#Group 3 (Growth and M)
modname <- c("Base Model","Estimate Growth","M Sex-specific","M Alaska Assessment","M Adjust For Migration","M Fix 2019 Assessment")
mod_sum <- SSsummarize(
  list(
    SS_output(dir=file.path(base_loc)),
    SS_output(dir=file.path(sens_loc,'8.36_growth')),
    SS_output(dir=file.path(sens_loc,'11-M-Diffsex')),
    SS_output(dir=file.path(sens_loc,'12-M-AK')),
    SS_output(dir=file.path(sens_loc,'13-M-adjust-migration')),
    SS_output(dir=file.path(sens_loc,'14-M-Fix-2019Assess'))
  )
)
cat("Model convergence:")
mod_sum$maxgrad
SSplotComparisons(mod_sum,
                  subplots = 4,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  plotdir=figs_loc,
                  filenameprefix='sens_3_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 18,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  plotdir=figs_loc,
                  filenameprefix='sens_3_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 12,
                  xlim=c(1960,2025),
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "topleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_3_',
                  endyr=2025)

create_comparison_table(dir = tabs_loc,
                        model_summary = mod_sum,
                        model_names = modname,
                        add_name = "sens_3")

#Group 4 (Recruitment)
modname <- c("Base Model","Estimate Steepness","RecDevs Sum To Zero","Estimate Early RecDevs (sum 0)")
mod_sum <- SSsummarize(
  list(
    SS_output(dir=file.path(base_loc)),
    SS_output(dir=file.path(sens_loc,'16-Est-steepness')),
    SS_output(dir=file.path(sens_loc,'17-RecDev-sum-zero')),
    SS_output(dir=file.path(sens_loc,'18-Est-RecDev-early-sum-zero'))
  )
)
cat("Model convergence:")
mod_sum$maxgrad
SSplotComparisons(mod_sum,
                  subplots = 4,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  plotdir=figs_loc,
                  filenameprefix='sens_4_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 18,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  plotdir=figs_loc,
                  filenameprefix='sens_4_',
                  endyr=2025)
SSplotComparisons(mod_sum,
                  subplots = 12,
                  plot=FALSE,
                  png=TRUE,
                  legendlabels=modname,
                  legendloc = "topleft",
                  plotdir=figs_loc,
                  filenameprefix='sens_4_',
                  endyr=2025)

create_comparison_table(dir = tabs_loc,
                        model_summary = mod_sum,
                        model_names = modname,
                        add_name = "sens_4")
