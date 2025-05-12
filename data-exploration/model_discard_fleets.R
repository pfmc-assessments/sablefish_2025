#===============================================================================
# Discard  Models
#===============================================================================
discard_growth <- SS_output(here::here("model", "_discard_fleets", "0.1_discard_fleet_growth"))
discard_watage <- SS_output(here::here("model", "_discard_fleets", "0.1_discard_fleet_watage"))
SS_plots(discard_growth)
SS_plots(discard_watage)

# Discard fleet growth:
# Length-based discard fleet selectivity
# The model with estimated growth has a double digit gradient and the hessian can not be 
# inverted.

modelnames <- c(
  "+ 0.1 Discard Fleets - Growth",
  "+ 0.1 Discard Fleets - W-at-Age")
mysummary <- SSsummarize(list(
  discard_growth, 
  discard_watage))
SSplotComparisons(mysummary,
                  filenameprefix = "0.1_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets"),
                  ylimAdj = 1.5,
                  pdf = TRUE)

r4ss::tune_comps(
  replist = discard_growth, 
  dir = here::here("model", "_discard_fleets", "0.1_discard_fleet_growth"),
  option = "Francis")

r4ss::tune_comps(
  replist = discard_watage, 
  dir = here::here("model", "_discard_fleets", "0.1_discard_fleet_watage"),
  option = "Francis")


#===============================================================================
# Discard  Models & Rec. Dev. Option 2
#===============================================================================
discard_growth_dev <- SS_output(here::here("model", "_discard_fleets", "0.2_discard_fleet_growth_dev"))
discard_watage_dev <- SS_output(here::here("model", "_discard_fleets", "0.2_discard_fleet_watage_dev"))

# Growth model gradient  = 36.3549
modelnames <- c(
  "+ 0.1 Discard Fleets - Growth (bad gradient)",
  "+ 0.1 Discard Fleets - W-at-Age",
  "+ 0.2 Discard Fleets - Growth, Dev. Option 2 (bad gradient)",
  "+ 0.2 Discard Fleets - W-at-Age, Dev. Option 2"
  )
mysummary <- SSsummarize(list(
  discard_growth, 
  discard_watage,
  discard_growth_dev, 
  discard_watage_dev))
SSplotComparisons(mysummary,
                  filenameprefix = "0.2_",
                  legendlabels = modelnames, 	
                  btarg = 0.40,
                  minbthresh = 0.25,
                  plotdir = here::here("model", "_discard_fleets"),
                  ylimAdj = 1.5,
                  pdf = TRUE)