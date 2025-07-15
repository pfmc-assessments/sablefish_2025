base_model <- r4ss::SS_output(dir = here::here("model", "base_model", "8.36_base_model"))
no_blocks <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_3", "8.36_no_selex_blocks"))
r4ss::SS_plots(no_blocks)

consistent_blocks <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_3", "8.36_consistent_blocks"))
r4ss::tune_comps(
  replist = consistent_blocks, 
  dir = here::here("model", "_star_panel_requests", "request_3", "8.36_consistent_blocks"),
  option = "Francis")
consistent_blocks <- r4ss::SS_output(dir = here::here("model", "_star_panel_requests", "request_3", "8.36_consistent_blocks"))
r4ss::SS_plots(consistent_blocks)

modelnames <- c(
  "Pre-STAR Base Model",
  "No Blocks",
  "Consistent Blocks")
mysummary <- r4ss::SSsummarize(list(
  base_model,
  no_blocks,
  consistent_blocks))
r4ss::SSplotComparisons(
  mysummary,
  filenameprefix = "request_3_",
  legendlabels = modelnames, 	
  btarg = 0.40,
  minbthresh = 0.25,
  plotdir = here::here("model", "_star_panel_requests", "request_3"),
  ylimAdj = 1.25,
  pdf = FALSE,
  print = TRUE)

#===============================================================================
# Plot Selectivity
#===============================================================================
library(r4ss)
model <- consistent_blocks
colors <- viridis::viridis(10)

info1 <- SSplotSelex(model, fleets = 1, subplots = 2, year = c(2001, 2010, 2018, 2024))
info1$infotable$longname <- c("1890-2001", "2002-2010", "2011-2018", "2019-2024")
info1$infotable$col <- c(colors[1], colors[2], colors[3], colors[4])

info2 <- SSplotSelex(model, fleets = 2, subplots = 2, year = c(2001, 2010, 2024))
info2$infotable$longname <- c("1890-2001", "1890-2001", "2002-2010", "2002-2010", "2011-2024", "2011-2024")
info2$infotable$col <- c(colors[1], colors[2], colors[3])

info3 <- SSplotSelex(model, fleets = 3, subplots = 2, year = c(2001, 2010, 2024))
info3$infotable$longname =  c("1890-2001", "1890-2001", "2002-2010", "2002-2010", "2011-2024", "2011-2024")
info3$infotable$col <- c(colors[1], colors[2], colors[3])

info4 <- SSplotSelex(model, fleets = 4, subplots = 2, year = c(2010, 2018, 2024))
info4$infotable$longname = c("1890-2010", "2011-2018", "2019-2024")
info4$infotable$col <-  c(colors[1], colors[2], colors[3])

info5 <- SSplotSelex(model, fleets = 5,  subplots = 2,  year = c(2010, 2024))
info5$infotable$longname =  c("1890-2010", "2011-2024")
info5$infotable$col <- c(colors[1], colors[2])

info6 <- SSplotSelex(model, fleets = 6,  subplots = 2,  year = c(2010, 2024))
info6$infotable$longname =  c("1890-2010", "2011-2024")
info6$infotable$col <- c(colors[1], colors[2])

pngfun <- function(wd, file, w = 7, h = 7, pt = 12) {
  file <- file.path(wd, file)
  cat("writing PNG to", file, "\n")
  grDevices::png(
    filename = file,
    width = w,
    height = h,
    units = "in",
    res = 300,
    pointsize = pt
  )
}

pngfun(wd = here::here("model", "_star_panel_requests", "request_3"), file = 'selectivity.png', w = 14, h = 10)
par(mfrow=c(2,3),mar=c(2,4,3,1))
SSplotSelex(model, fleets=1,  infotable=info1$infotable, agefactors = c("Asel"),
            subplots = 2, legendloc='bottomright', year = c(2001, 2010, 2018, 2024))
grid()
SSplotSelex(model, fleets=2, infotable=info2$infotable, agefactors = c("Asel"),
            subplots=2, legendloc='topright',year = c(2001, 2010, 2024))
grid()
SSplotSelex(model, fleets=3, infotable=info3$infotable,subplots=2, agefactors = c("Asel"),
            legendloc='topright', year = c(2001, 2010, 2024))
grid()
SSplotSelex(model, fleets=4, infotable=info4$infotable,subplots=2, agefactors = c("Asel"),
            legendloc='topright', year = c(2010, 2018, 2024))
grid()
SSplotSelex(model, fleets=5, infotable=info5$infotable,subplots=2, agefactors = c("Asel"),
            legendloc='topright', year = c(2010, 2024))
grid()
SSplotSelex(model, fleets=6, infotable=info6$infotable, subplots=2, agefactors = c("Asel"), 
            legendloc='bottomright', year = c(2010, 2024))
grid()
dev.off()
