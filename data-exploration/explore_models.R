library(r4ss)
model_dir <- here::here("model", "_bridging", "_2023 sensitivities")
model_2023 <- SS_output(file.path(model_dir, "0_2023_model"))
mat_age <- SS_output(file.path(model_dir, "0_2023_model_mat_age"))
SS_plots(mat_age)

modelnames <- c(
  "2023 Base", 
  "Maturity-at-Age")
mysummary <- SSsummarize(list(model_2023, mat_age))

SSplotComparisons(mysummary,
                  filenameprefix = "0_2023_model_mat_",
                  legendlabels = modelnames, 	
                  plotdir = here::here(model_dir, "_plots"),
                  pdf = TRUE)

wtatage <- SS_output(file.path(model_dir, "1_wtatage"))
SS_plots(wtatage)                    
modelnames <- c(
  "2023 Base", 
  "Maturity-at-Age",
  "Weight-at-Age (Simplified)")
mysummary <- SSsummarize(list(model_2023, mat_age, wtatage))

SSplotComparisons(mysummary,
                  filenameprefix = "0_2023_model_mat_wtatage_",
                  legendlabels = modelnames, 	
                  plotdir = here::here(model_dir, "_plots"),
                  pdf = TRUE)
                     

