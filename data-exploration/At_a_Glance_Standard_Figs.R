library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(r4ss)

############################################
############################################
#Plot sablefish removals figure (front page middle panel)
model_output <- r4ss::SS_output(here::here("model", "base_model", "8.36_base_model"))

landings <- model_output$catch |> 
  dplyr::mutate(
    year = Yr,
    catch_mt = dead_bio,
    Fleet = dplyr::case_when(
      Fleet_Name == "Trawl" ~ "Trawl",
      Fleet_Name == "Hook_and_Line" ~ "Hook and Line",
      Fleet_Name == "Pot" ~ "Pot",
      Fleet_Name == "Trawl_Discard" ~ "Trawl Discard",
      Fleet_Name == "Hook_and_Line_Discard" ~ "Hook and Line Discard",
      Fleet_Name == "Pot_Discard" ~ "Pot Discard",
      .default = Fleet_Name)
  )

ggplot2::ggplot(landings, ggplot2::aes(x = year, y = catch_mt, fill = Fleet)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Removals (mt)") +
  ggplot2::xlim(c(1890, 2025)) + 
  ggplot2::scale_y_continuous(
    labels = function(x) format(x,scientific = FALSE)) +
  #nmfspalette::scale_fill_nmfs(palette = "waves", reverse = TRUE) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = c(0.20, 0.65),
    legend.title=element_text(size = 16), 
    legend.text = ggplot2::element_text(size = 16),
    axis.text.y = ggplot2::element_text(angle = 90,vjust=0.5,hjust=0.5),
    axis.text = ggplot2::element_text(size = 19),
    axis.title = ggplot2::element_text(size = 21)
  )

ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "removals.png"), width = 8, height = 4)


############################################
############################################
#Plot sablefish stock status with error and states of nature (front page bottom panel)
model_output <- r4ss::SS_output(here::here("model", "base_model","decision_table", "m", "base_pstar_45"))
model_output_high <- r4ss::SS_output(here::here("model", "base_model","decision_table", "m", "high_pstar_45"))
model_output_low <- r4ss::SS_output(here::here("model", "base_model", "decision_table","m", "low_pstar_45"))
mymodels <- list(model_output_high,model_output,model_output_low)
mysummary <- SSsummarize(mymodels)
#modelnames <- c("Low State of Nature","Base Model","High State of Nature")
#pdf("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\model\\Base\\Sensitivity_tests\\TruncateMaxAge_35\\Compare_models.pdf")
#SSplotComparisons(mysummary,
#                  legendlabels=modelnames,
#                  subplot = 4,
#                  plotdir="C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary",
#                  print=TRUE,
#                  endyr=2037,
#                  #xlim=c(2000,2037),
#                  new=F
#                  )
#make above in ggplot2
bratio_pt <- mysummary[["Bratio"]] %>%
  pivot_longer(
    cols = starts_with("model"),
    names_to = "Model",
    values_to = "Bratio"
  )
bratio_lo <- mysummary[["BratioLower"]] %>%
  pivot_longer(
    cols = starts_with("model"),
    names_to = "Model",
    values_to = "Bratio"
  )
bratio_hi <- mysummary[["BratioUpper"]] %>%
  pivot_longer(
    cols = starts_with("model"),
    names_to = "Model",
    values_to = "Bratio"
  )
p1 <- ggplot() +
  geom_line(data = bratio_pt, aes(x=Yr,y=Bratio, color = Model),size=1.2) +
  ggplot2::theme_bw() +
  ggplot2::xlab("Year") + ggplot2::ylab("Stock Status \n (fraction of unfished biomass)") +
  ggplot2::xlim(c(2000, 2037)) +
  ggplot2::scale_color_manual(labels = c("High State of Nature", "Base Model","Low State of Nature"), values = c("darkolivegreen4","blue", "brown4")) +
  ggplot2::annotate('rect', xmin=2025, xmax=2037, ymin=0, ymax=0.83, alpha=0.3, fill='gray35') +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = c(0.80, 0.9),
    legend.title=element_blank(), 
    legend.text = ggplot2::element_text(size = 16),
    axis.text.y = ggplot2::element_text(angle = 90,vjust=0.5,hjust=0.5),
    axis.text = ggplot2::element_text(size = 19),
    axis.title = ggplot2::element_text(size = 21)
  ) +
  geom_hline(yintercept=0.25, linetype="dashed", color = "red", size=0.7) +
  geom_hline(yintercept=0.40, linetype="dashed", color = "red", size=0.7) +
  annotate(geom="text", x=2000, y=0.43, label="Management Target", color="red", hjust = 0, size=5) +
  annotate(geom="text", x=2000, y=0.28, label="Minimum Stock Size Threshold", color="red", hjust = 0, size=5) +
  annotate(geom="text", x=2025.2, y=0.05, label="Forecast Period", color="gray35", hjust = 0, size =5) +
  annotate(geom="text", x=2000, y=0.02, label="Blue shading represents 95% uncertainty range for the base model", color="gray40", hjust = 0, size = 4) +
  geom_polygon(data = data.frame(x = c(filter(bratio_hi,Yr>1999 & Model=="model3")$Yr, rev(filter(bratio_hi,Yr>1999 & Model=="model3")$Yr)),y = c(filter(bratio_hi,Yr>1999 & Model=="model3")$Bratio, rev(filter(bratio_lo,Yr>1999 & Model=="model3")$Bratio))),
               aes(x = x, y = y), fill = "brown4",alpha=0.2) +
  geom_polygon(data = data.frame(x = c(filter(bratio_hi,Yr>1999 & Model=="model2")$Yr, rev(filter(bratio_hi,Yr>1999 & Model=="model2")$Yr)),y = c(filter(bratio_hi,Yr>1999 & Model=="model2")$Bratio, rev(filter(bratio_lo,Yr>1999 & Model=="model2")$Bratio))),
               aes(x = x, y = y), fill = "blue",alpha=0.2) +
  geom_polygon(data = data.frame(x = c(filter(bratio_hi,Yr>1999 & Model=="model1")$Yr, rev(filter(bratio_hi,Yr>1999 & Model=="model1")$Yr)),y = c(filter(bratio_hi,Yr>1999 & Model=="model1")$Bratio, rev(filter(bratio_lo,Yr>1999 & Model=="model1")$Bratio))),
               aes(x = x, y = y), fill = "darkolivegreen4",alpha=0.2) 
  #+
  #geom_line(data = data.frame(x = filter(bratio_hi,Yr>1999 & Model=="model2")$Yr,y = filter(bratio_hi,Yr>1999 & Model=="model2")$Bratio),
  #             aes(x = x, y = y), linetype = "dashed", color = "blue",size = 0.5, alpha=0.5) +
  #geom_line(data = data.frame(x = filter(bratio_hi,Yr>1999 & Model=="model2")$Yr,y = filter(bratio_lo,Yr>1999 & Model=="model2")$Bratio),
  #          aes(x = x, y = y), linetype = "dashed", color = "blue",size = 0.5, alpha=0.5) +
  #geom_line(data = data.frame(x = filter(bratio_hi,Yr>1999 & Model=="model3")$Yr,y = filter(bratio_hi,Yr>1999 & Model=="model3")$Bratio),
  #          aes(x = x, y = y), linetype = "dashed", color = "brown4",size = 0.5, alpha=0.5) +
  #geom_line(data = data.frame(x = filter(bratio_hi,Yr>1999 & Model=="model3")$Yr,y = filter(bratio_lo,Yr>1999 & Model=="model3")$Bratio),
  #          aes(x = x, y = y), linetype = "dashed", color = "brown4",size = 0.5, alpha=0.5) +
  #geom_line(data = data.frame(x = filter(bratio_hi,Yr>1999 & Model=="model1")$Yr,y = filter(bratio_hi,Yr>1999 & Model=="model1")$Bratio),
  #          aes(x = x, y = y), linetype = "dashed", color = "darkolivegreen4",size = 0.5, alpha=0.5) +
  #geom_line(data = data.frame(x = filter(bratio_hi,Yr>1999 & Model=="model1")$Yr,y = filter(bratio_lo,Yr>1999 & Model=="model1")$Bratio),
  #          aes(x = x, y = y), linetype = "dashed", color = "darkolivegreen4",size = 0.5, alpha=0.5)

p2 <- ggplot() +
  geom_line(data = bratio_pt, aes(x=Yr,y=Bratio, color = Model),size=0.8) +
  ggplot2::theme_bw() +
  ggplot2::xlab("") + ggplot2::ylab("") +
  ggplot2::xlim(c(1890, 2037)) +
  ggplot2::scale_color_manual(labels = c("High State of Nature", "Base Model","Low State of Nature"), values = c("darkolivegreen4","blue", "brown4")) +
  ggplot2::annotate('rect', xmin=2025, xmax=2037, ymin=0, ymax=1.0, alpha=0.3, fill='gray35') +
  ggplot2::theme(
    strip.text.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(colour="white", fill="white"),
    legend.key.height = ggplot2::unit(0.05, "cm"),
    legend.position = "none",
    legend.title=element_blank(), 
    legend.text = ggplot2::element_text(size = 8),
    axis.text.y = ggplot2::element_text(angle = 90,vjust=0.5,hjust=0.5),
    axis.text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 14)
  ) +
  geom_hline(yintercept=0.25, linetype="dashed", color = "red", size=0.5) +
  geom_hline(yintercept=0.40, linetype="dashed", color = "red", size=0.5) +
  geom_polygon(data = data.frame(x = c(filter(bratio_hi,Model=="model3")$Yr, rev(filter(bratio_hi,Model=="model3")$Yr)),y = c(filter(bratio_hi,Model=="model3")$Bratio, rev(filter(bratio_lo,Model=="model3")$Bratio))),
               aes(x = x, y = y), fill = "brown4",alpha=0.2) +
  geom_polygon(data = data.frame(x = c(filter(bratio_hi,Model=="model2")$Yr, rev(filter(bratio_hi,Model=="model2")$Yr)),y = c(filter(bratio_hi,Model=="model2")$Bratio, rev(filter(bratio_lo,Model=="model2")$Bratio))),
               aes(x = x, y = y), fill = "blue",alpha=0.2) +
  geom_polygon(data = data.frame(x = c(filter(bratio_hi,Model=="model1")$Yr, rev(filter(bratio_hi,Model=="model1")$Yr)),y = c(filter(bratio_hi,Model=="model1")$Bratio, rev(filter(bratio_lo,Model=="model1")$Bratio))),
               aes(x = x, y = y), fill = "darkolivegreen4",alpha=0.2)
  
p1 + annotation_custom(ggplotGrob(p2), xmin = 2000, xmax = 2020, ymin = 0.45, ymax = 1.0)

ggplot2::ggsave(filename = here::here("presentation", "data", "plots", "removals.png"), width = 8, height = 4)


############################################
############################################
#Plot select time series of recruitment strength in a simple bar graph (second page lone figure)
#change directory/file location as needed
setwd("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary")
radar <- read.csv("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary\\RecruitRadar.csv")
Equil_rec <- 25981  #note: taken from stock assessment mean estimate
radar$Year <- as.factor(radar$Year)

plt <- ggplot(radar) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 25000),
    color = "lightgrey"
  ) + 
  geom_col(
    aes(
      x = Year,
      y = Recruit,
      fill = Mature
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  ylab("Recruitment (millions of fish)") +
  geom_hline(
    yintercept = Equil_rec,
    linetype = "dashed",
    color = "maroon",
    size = 0.8
  ) 

plt <- plt +
  annotate(
    x = 3, 
    y = Equil_rec*1.1,
    label = "Equilibrium Recruitment",
    geom = "text",
    #angle = -20,
    color = "maroon",
    size = 5,
    family = "sans"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(color = "gray12", angle = 45, vjust = 0.5, size = 19),
    axis.text.y = element_text(color = "gray12", size = 19),
    # Move the legend to the bottom
    legend.position = c(0.25,0.85),
    axis.title = ggplot2::element_text(size = 21),
    legend.text=element_text(size=16),
    legend.title=element_text(size=16)
  ) +
  guides(fill=guide_legend(title="Mature (> 50%) by 2025"))


plt <- plt + 
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "sans"),
    axis.line.y = element_line(colour = 'black', linewidth=0.5, linetype='solid'),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("Rec_bar.png", plt,width=8, height=8)
ggsave("Rec_bar.pdf", plt,width=8, height=8)


############################################
############################################
#Plot fit to abundance index (front page top panel)
#Need to run the SSplotIndices_play function below, which is a version of SSplotIndices but with cex for labels.
model_output <- r4ss::SS_output(here::here("model", "base_model", "8.36_base_model"))
SSplotIndices_play(model_output,subplots=2,fleets=10)


#note that this is a copy of the r4ss SSplotIndices with some adjustments to change size of axis labels, etc.
SSplotIndices_play <-
  function(
    replist,
    subplots = c(1:10, 12), # IGT 2021/4/15: not sure why 11 is skipped
    plot = TRUE,
    print = FALSE,
    fleets = "all",
    fleetnames = "default",
    smooth = TRUE,
    add = FALSE,
    datplot = TRUE,
    labels = c(
      "Year", # 1
      "Index", # 2
      "Observed index", # 3
      "Expected index", # 4
      "Log index", # 5
      "Log observed index", # 6
      "Log expected index", # 7
      "Standardized index", # 8
      "Catchability (Q)", # 9
      "Time-varying catchability", # 10
      "Vulnerable biomass", # 11
      "Catchability vs. vulnerable biomass", # 12
      "Residual", # 13
      "Deviation"
    ), # 14
    fleetcols = NULL,
    col1 = "default",
    col2 = "default",
    col3 = "blue",
    col4 = "red",
    pch1 = 21,
    pch2 = 16,
    cex = 1,
    bg = "white",
    legend = TRUE,
    legendloc = "topright",
    seasnames = NULL,
    pwidth = 6.5,
    pheight = 5.0,
    punits = "in",
    res = 300,
    ptsize = 10,
    cex.main = 1,
    mainTitle = FALSE,
    plotdir = "default",
    minyr = NULL,
    maxyr = NULL,
    maximum_ymax_ratio = Inf,
    show_input_uncertainty = TRUE,
    verbose = TRUE,
    ...
  ) {
    # get some quantities from replist
    cpue <- replist[["cpue"]]
    SS_versionNumeric <- replist[["SS_versionNumeric"]]
    
    # confirm that some CPUE values are present
    if (is.null(dim(cpue))) {
      message("skipping index plots: no index data in this model")
      return()
    }
    
    # table to store information on each plot
    plotinfo <- NULL
    
    # define a bunch of internal functions
    
    index.fn <- function(addexpected = TRUE, log = FALSE, ...) {
      # plot of time series of observed values with fit (if requested)
      
      # don't do anything for log-scale plot if normal error structure is used
      if (error == -1 & log == TRUE) {
        return()
      }
      
      # function to get uncertainty intervals around points
      # (with or without extra standard error included)
      get_intervals <- function(total = TRUE) {
        if (total) {
          colname <- "SE"
        } else {
          colname <- "SE_input"
        }
        if (error == 0) {
          if (!log) {
            lower <- qlnorm(
              .025,
              meanlog = log(y[include]),
              sdlog = cpueuse[[colname]][include]
            )
            upper <- qlnorm(
              .975,
              meanlog = log(y[include]),
              sdlog = cpueuse[[colname]][include]
            )
          } else {
            lower <- qnorm(
              .025,
              mean = log(y[include]),
              sd = cpueuse[[colname]][include]
            )
            upper <- qnorm(
              .975,
              mean = log(y[include]),
              sd = cpueuse[[colname]][include]
            )
          }
        }
        # normal error interval
        if (error == -1) {
          lower <- qnorm(
            .025,
            mean = y[include],
            sd = cpueuse[[colname]][include]
          )
          upper <- qnorm(
            .975,
            mean = y[include],
            sd = cpueuse[[colname]][include]
          )
        }
        
        # T-distribution interval
        if (error > 0) {
          lower <- log(y[include]) +
            qt(.025, df = error) * cpueuse[[colname]][include]
          upper <- log(y[include]) +
            qt(.975, df = error) * cpueuse[[colname]][include]
          if (!log) {
            lower <- exp(lower)
            upper <- exp(upper)
          }
        }
        return(data.frame(lower = lower, upper = upper))
      }
      
      # get total uncertainty
      total_intervals <- get_intervals(total = TRUE)
      lower_total <- total_intervals[["lower"]]
      upper_total <- total_intervals[["upper"]]
      
      # get input uncertainty
      # SE_input column was first available in SS version 3.30.15 but is calculated
      # elsewhere in this function for models that didn't report it
      input_intervals <- get_intervals(total = FALSE)
      lower_input <- input_intervals[["lower"]]
      upper_input <- input_intervals[["upper"]]
      
      if (max(upper_total) == Inf) {
        warning(
          "Removing upper interval on indices with infinite upper quantile values.\n",
          "Check the uncertainty inputs for the indices."
        )
        upper_total[upper_total == Inf] <- 100 *
          max(cpueuse[["Obs"]][upper_total == Inf])
      }
      
      # plot title
      main <- paste0(labels[2], Fleet)
      if (log) {
        main <- paste0(labels[5], Fleet)
      }
      
      # no title
      if (!mainTitle) {
        main <- ""
      }
      
      xlim <- c(max(minyr, min(x)), min(maxyr, max(x)))
      if (legend & length(colvec1) > 1) {
        xlim[2] <- xlim[2] + 0.25 * diff(xlim)
      }
      if (!add) {
        # get range for expected values
        zrange <- NULL
        if (addexpected) {
          zrange <- range(z, na.rm = TRUE)
        }
        logzrange <- range(log(z))
        
        # y-limits for non-log plot
        if (!log) {
          # ylim for standard scale (if lognormal)
          if (error != -1) {
            ylim <- c(
              0,
              1.05 *
                min(
                  max(upper_total, zrange, na.rm = TRUE),
                  max(maximum_ymax_ratio * y, na.rm = TRUE)
                )
            )
          } else {
            ylim <- 1.05 *
              c(
                min(lower_total, zrange, na.rm = TRUE),
                min(
                  max(upper_total, zrange, na.rm = TRUE),
                  max(maximum_ymax_ratio * y, na.rm = TRUE)
                )
              )
          }
        }
        if (log) {
          # ylim for log scale plot
          ylim <- range(c(lower_total, upper_total), na.rm = TRUE)
        }
        
        plot(
          x = x[include],
          y = y[include],
          type = "n",
          xlab = labels[1],
          ylab = "Relative Index of Abundance",
          main = main,
          cex.main = cex.main,
          cex.lab = 1.5,
          cex.axis = 1.3,
          xlim = xlim,
          ylim = ylim,
          yaxs = ifelse(log, "r", "i"),
          ...
        )
        legend("bottomright",legend=c("Model Fit","Index Data","Index Uncertainty"),
               pch=c(NA,1,NA),
               lwd = c(1.5,NA,3),
               col=c("blue","black","black"),
               bty="n",
               cex=1.3)
        
        # add line at 0 if it's not a log-scale plot
        # and the axes include zero
        if (!log & min(ylim) < 0) {
          abline(h = 0, lty = 3)
        }
      }
      
      # set bounds for arrows at total uncertainty
      lower <- lower_total
      upper <- upper_total
      
      if (addexpected) {
        # show thicker lines behind final lines for input uncertainty
        # only in plot with expected value as well
        if (
          show_input_uncertainty &
          !all(lower_input == lower_total, na.rm = TRUE)
        ) {
          segments(
            x[include],
            lower_input,
            x[include],
            upper_input,
            col = colvec1[s],
            lwd = 3,
            lend = 1
          )
        }
      } else {
        # change bounds for arrows at input uncertainty for plots without fit
        # if values are available
        lower <- lower_input
        upper <- upper_input
      }
      
      # add intervals
      arrows(
        x0 = x[include],
        y0 = lower,
        x1 = x[include],
        y1 = upper,
        length = 0.03,
        angle = 90,
        code = 3,
        col = colvec1[s]
      )
      
      # add points and expected values on standard scale
      if (!log) {
        points(
          x = x[include],
          y = y[include],
          pch = pch1,
          cex = cex,
          bg = bg,
          col = colvec1[s]
        )
        if (addexpected) {
          lines(x, z, lwd = 2, col = col3)
          if (length(x) == 1) {
            points(x, z, pch = 23, col = col3)
          }
        }
      } else {
        # add points and expected values on log scale
        points(
          x = x[include],
          y = log(y[include]),
          pch = pch1,
          cex = cex,
          bg = bg,
          col = colvec1[s]
        )
        if (addexpected) {
          lines(x, log(z), lwd = 2, col = col3)
          if (length(x) == 1) {
            points(x, log(z), pch = 23, col = col3)
          }
        }
      }
      if (legend & length(colvec1) > 1) {
        legend(
          x = legendloc,
          legend = seasnames,
          pch = pch1,
          col = colvec1,
          cex = cex
        )
      }
    }
    
    index_resids.fn <- function(option = 1, ...) {
      # plot of time series of residuals
      # options
      # 1: residuals based on total SE
      # 2: residuals based on input SE
      # 3: deviations (independent of index variability)
      
      # choose y value and y-axis label
      
      if (option == 1) {
        # residuals based on total SE
        ylab <- labels[13]
        y <- (log(cpueuse[["Obs"]]) - log(cpueuse[["Exp"]])) / cpueuse[["SE"]]
      }
      if (error == 0 & option == 2) {
        # residuals based on input SE
        ylab <- labels[13]
        # manually calculating residual based on SE_input
        y <- (log(cpueuse[["Obs"]]) - log(cpueuse[["Exp"]])) /
          cpueuse[["SE_input"]]
      }
      if (option == 3) {
        # deviations
        ylab <- labels[14]
        # Dev should be equal to log(Obs/Exp)
        y <- cpueuse[["Dev"]]
      }
      
      # plot title
      main <- paste(ylab, Fleet)
      
      # no plot title
      if (!mainTitle) {
        main <- ""
      }
      # xlim (maybe reduced by inputs minyr and maxyr)
      xlim <- c(max(minyr, min(x)), min(maxyr, max(x)))
      if (legend & length(colvec1) > 1) {
        xlim[2] <- xlim[2] + 0.25 * diff(xlim)
      }
      
      # ylim is symetrical around 0
      ylim <- c(-1.05, 1.05) * max(abs(y[include]))
      if (!add) {
        plot(
          x = x[include],
          y = y[include],
          type = "n",
          xlab = labels[1],
          xlim = xlim,
          ylab = ylab,
          ylim = ylim,
          yaxs = "i",
          main = main,
          cex.main = cex.main,
          ...
        )
      }
      # add points
      points(
        x = x[include],
        y = y[include],
        pch = pch1,
        cex = cex,
        bg = adjustcolor(colvec1[s], alpha.f = 0.7),
        col = adjustcolor(colvec1[s], alpha.f = 0.7)
      )
      
      # add line at 0
      abline(h = 0, lty = 3)
      
      # add legend if more than one color (indicating season) was used
      if (legend & length(colvec1) > 1) {
        legend(
          x = legendloc,
          legend = seasnames,
          pch = pch1,
          pt.bg = colvec1,
          col = colvec1,
          cex = cex
        )
      }
    }
    
    obs_vs_exp.fn <- function(log = FALSE, ...) {
      # plot of observed vs. expected with smoother
      
      # plot title
      main <- paste(labels[2], Fleet, sep = " ")
      # no title
      if (!mainTitle) {
        main <- ""
      }
      
      if (!add) {
        if (!log) {
          # standard plot
          plot(
            y[include],
            z[include],
            type = "n",
            xlab = labels[3],
            ylab = labels[4],
            main = main,
            cex.main = cex.main,
            ylim = c(0, 1.05 * max(z)),
            xlim = c(0, 1.05 * max(y)),
            xaxs = "i",
            yaxs = "i",
            ...
          )
        } else {
          # log-scale plot doesn't specificy y limits
          plot(
            log(y[include]),
            log(z[include]),
            type = "n",
            xlab = labels[6],
            ylab = labels[7],
            main = main,
            cex.main = cex.main
          )
        }
      }
      if (!log) {
        points(y[include], z[include], col = colvec2[s], pch = pch2, cex = cex)
      } else {
        points(
          log(y[include]),
          log(z[include]),
          col = colvec2[s],
          pch = pch2,
          cex = cex
        )
      }
      abline(a = 0, b = 1, lty = 3)
      if (smooth && npoints > 6 && diff(range(y)) > 0) {
        if (!log) {
          psmooth <- loess(z[include] ~ y[include], degree = 1)
          lines(
            psmooth[["x"]][order(psmooth[["x"]])],
            psmooth[["fitted"]][order(psmooth[["x"]])],
            lwd = 1.2,
            col = col4,
            lty = "dashed"
          )
        } else {
          psmooth <- loess(log(z[include]) ~ log(y[include]), degree = 1)
          lines(
            psmooth[["x"]][order(psmooth[["x"]])],
            psmooth[["fitted"]][order(psmooth[["x"]])],
            lwd = 1.2,
            col = col4,
            lty = "dashed"
          )
        }
      }
      if (legend & length(colvec2) > 1) {
        legend(
          x = legendloc,
          legend = seasnames,
          pch = pch2,
          col = colvec2,
          cex = cex
        )
      }
    }
    
    timevarying_q.fn <- function() {
      # plot of time-varying catchability (if present)
      main <- paste(labels[10], Fleet, sep = " ")
      if (!mainTitle) {
        main <- ""
      }
      q <- cpueuse[["Calc_Q"]]
      if (!add) {
        plot(
          x,
          q,
          type = "o",
          xlab = labels[1],
          main = main,
          cex.main = cex.main,
          ylab = labels[9],
          col = colvec2[1],
          pch = pch2
        )
      }
    }
    
    q_vs_vuln_bio.fn <- function() {
      # plot of time-varying catchability (if present)
      main <- paste(labels[12], Fleet, sep = " ")
      if (!mainTitle) {
        main <- ""
      }
      v <- cpueuse[["Vuln_bio"]]
      q1 <- cpueuse[["Calc_Q"]]
      q2 <- cpueuse[["Eff_Q"]]
      if (all(q1 == q2)) {
        ylab <- labels[9]
      } else {
        ylab <- "Effective catchability"
      }
      if (!add) {
        plot(
          v,
          q2,
          type = "o",
          xlab = labels[11],
          main = main,
          cex.main = cex.main,
          ylab = ylab,
          col = colvec2[1],
          pch = pch2
        )
      }
    }
    
    # check for super periods
    if (length(grep("supr_per", cpue[["Supr_Per"]]))) {
      warning(
        "Some indices have superperiods. Values will be plotted\n",
        "in year/season associated with data in report file."
      )
      cpue <- cpue[!is.na(cpue[["Dev"]]), ]
    }
    
    FleetNames <- replist[["FleetNames"]]
    nfleets <- replist[["nfleets"]]
    nseasons <- replist[["nseasons"]]
    
    # find any extra SD parameters
    parameters <- replist[["parameters"]]
    Q_extraSD_info <- parameters[grep("Q_extraSD", parameters[["Label"]]), ]
    # calculate how many of these parameters there are
    nSDpars <- nrow(Q_extraSD_info)
    if (nSDpars > 0) {
      # parse the parameter label to get the fleet number
      Q_extraSD_info[["Fleet"]] <- NA
      for (ipar in 1:nSDpars) {
        if (SS_versionNumeric >= 3.3) {
          # parsing label with ending like "(2)" assuming only one set of parentheses
          num <- strsplit(
            Q_extraSD_info[["Label"]][ipar],
            split = "[()]",
            fixed = FALSE
          )[[1]][2]
        } else {
          num <- strsplit(
            substring(Q_extraSD_info[["Label"]][ipar], nchar("Q_extraSD_") + 1),
            split = "_",
            fixed = TRUE
          )[[1]][1]
        }
        Q_extraSD_info[["Fleet"]][ipar] <- as.numeric(num)
      }
      # NOTE: important columns in Q_extraSD_info to use below are $Value and $Fleet
    }
    if (nseasons > 1) {
      # if seasons, put CPUE at season midpoint
      cpue[["YrSeas"]] <- cpue[["Yr"]] + (cpue[["Seas"]] - 0.5) / nseasons
    } else {
      # if no seasons, put at integer year value
      cpue[["YrSeas"]] <- cpue[["Yr"]]
    }
    if (plotdir == "default") {
      plotdir <- replist[["inputs"]][["dir"]]
    }
    
    if (fleetnames[1] == "default") {
      fleetnames <- FleetNames
    }
    if (fleets[1] == "all") {
      fleets <- 1:nfleets
    } else {
      if (length(intersect(fleets, 1:nfleets)) != length(fleets)) {
        return(
          "Input 'fleets' should be 'all' or a vector of values between 1 and nfleets."
        )
      }
    }
    
    # subset fleets as requested
    fleetvec <- intersect(fleets, unique(as.numeric(cpue[["Fleet"]])))
    
    # empty data.frame to store data for comparison among indices
    allcpue <- data.frame()
    # keep track of whether any indices with negative observations is excluded
    any_negative <- FALSE
    
    # loop over fleets
    for (ifleet in fleetvec) {
      # use fancy colors only if the individual index spans more than one season
      usecol <- FALSE
      if (length(unique(cpue[["Seas"]][cpue[["Fleet"]] == ifleet])) > 1) {
        usecol <- TRUE
      }
      
      # turn off use of legend if there's never more than 1 season per index
      if (!usecol) {
        legend <- FALSE
      }
      
      if (col1[1] == "default") {
        colvec1 <- "black"
        if (usecol & nseasons == 4) {
          colvec1 <- c("blue4", "green3", "orange2", "red3")
        }
        if (usecol & !nseasons %in% c(1, 4)) {
          colvec1 <- rich.colors.short(nseasons)
        }
      } else {
        colvec1 <- col1
        # if user provides single value (or vector of length less than nseasons)
        # make sure it's adequate to cover all seasons
        if (length(colvec1) < nseasons) {
          colvec1 <- rep(col1, nseasons)
        }
      }
      if (col2[1] == "default") {
        colvec2 <- "blue"
        if (usecol & nseasons == 4) {
          colvec2 <- c("blue4", "green3", "orange2", "red3")
        }
        if (usecol & !nseasons %in% c(1, 4)) {
          colvec2 <- rich.colors.short(nseasons)
        }
      } else {
        colvec2 <- col2
        # if user provides single value (or vector of length less than nseasons)
        # make sure it's adequate to cover all seasons
        if (length(colvec1) < nseasons) {
          colvec1 <- rep(col1, nseasons)
        }
      }
      if (is.null(seasnames)) {
        seasnames <- paste("Season", 1:nseasons, sep = "")
      }
      
      Fleet <- fleetnames[ifleet]
      error <- replist[["survey_error"]][ifleet]
      if (error == 0) {
        error_caption <- "lognormal error"
      }
      if (error == -1) {
        error_caption <- "normal error"
      }
      if (error == 1) {
        error_caption <- paste0(
          "T-distributed error with ",
          error,
          " degree of freedom"
        )
      }
      if (error > 1) {
        error_caption <- paste0(
          "T-distributed error with ",
          error,
          " degrees of freedom"
        )
      }
      
      cpueuse <- cpue[cpue[["Fleet"]] == ifleet, ]
      cpueuse <- cpueuse[order(cpueuse[["YrSeas"]]), ]
      
      # look for time-vary
      time <- diff(range(cpueuse[["Calc_Q"]])) > 0
      # look for time-varying effective Q
      time2 <- diff(range(cpueuse[["Eff_Q"]])) > 0
      # Teresa's model had NA values in Eff_Q for unknown reasons
      # line below will allow model to play on
      if (is.na(time2)) {
        time2 <- FALSE
      }
      # if "SE_input" column not available, look for extra SD and
      # calculate input SD (if different from final value)
      if (!"SE_input" %in% names(cpue)) {
        if (exists("Q_extraSD_info") && ifleet %in% Q_extraSD_info[["Fleet"]]) {
          # input uncertainty is final value minus extra SD parameter (if present)
          cpueuse[["SE_input"]] <- cpueuse[["SE"]] -
            Q_extraSD_info[["Value"]][Q_extraSD_info[["Fleet"]] == ifleet]
        } else {
          cpueuse[["SE_input"]] <- cpueuse[["SE"]]
        }
      }
      # use short variable names for often-used quantities
      x <- cpueuse[["YrSeas"]]
      y <- cpueuse[["Obs"]]
      z <- cpueuse[["Exp"]]
      npoints <- length(z)
      include <- !is.na(cpueuse[["Like"]])
      if (any(include)) {
        if (usecol) {
          s <- cpueuse[["Seas"]][which(include)]
        } else {
          s <- 1 # only use colorvector if more than 1 season
        }
        if (datplot) {
          # add index data to data frame which is used to compare all indices
          if (min(cpueuse[["Obs"]] >= 0)) {
            cpueuse[["Index"]] <- rep(ifleet, length(cpueuse[["YrSeas"]]))
            cpueuse[["stdvalue"]] <- cpueuse[["Obs"]] / mean(cpueuse[["Obs"]])
            tempcpue <- cbind(
              cpueuse[["Index"]],
              cpueuse[["YrSeas"]],
              cpueuse[["Obs"]],
              cpueuse[["stdvalue"]]
            )
            colnames(tempcpue) <- c("Index", "year", "value", "stdvalue")
            allcpue <- rbind(allcpue, tempcpue)
          } else {
            if (verbose & 9 %in% subplots & datplot) {
              message(
                "Excluding fleet ",
                ifleet,
                " from index comparison figure because it has negative values"
              )
            }
            any_negative <- TRUE
          }
        }
        
        addlegend <- function(pch, colvec) {
          names <- paste(seasnames, "observations")
        }
        
        if (plot) {
          if (1 %in% subplots & datplot) {
            index.fn(addexpected = FALSE)
          }
          if (2 %in% subplots) {
            index.fn()
          }
          if (3 %in% subplots) {
            obs_vs_exp.fn()
          }
        }
        if (print) {
          if (1 %in% subplots & datplot) {
            file <- paste0("index1_cpuedata_", gsub(" ", "", Fleet), ".png")
            caption <- paste0(
              "Index data for ",
              Fleet,
              ". ",
              "Lines indicate 95% uncertainty interval around index values ",
              "based on the model assumption of ",
              error_caption,
              ". ",
              "Thicker lines (if present) indicate input uncertainty before addition of ",
              "estimated additional uncertainty parameter."
            )
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            index.fn(addexpected = FALSE)
            dev.off()
          }
          if (2 %in% subplots) {
            file <- paste0("index2_cpuefit_", gsub(" ", "", Fleet), ".png")
            caption <- paste0(
              "Fit to index data for ",
              Fleet,
              ". ",
              "Lines indicate 95% uncertainty interval around index values ",
              "based on the model assumption of ",
              error_caption,
              ". ",
              "Thicker lines (if present) indicate input uncertainty before addition of ",
              "estimated additional uncertainty parameter."
            )
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            index.fn()
            dev.off()
          }
          if (3 %in% subplots) {
            file <- paste0("index3_obs_vs_exp_", gsub(" ", "", Fleet), ".png")
            caption <- paste(
              "Observed vs. expected index values with smoother for",
              Fleet
            )
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            obs_vs_exp.fn()
            dev.off()
          }
        }
        
        # same plots again in log space
        # check for lognormal error
        if (error != -1) {
          # plot subplots 4-6 to current device
          if (plot) {
            if (4 %in% subplots & datplot) {
              index.fn(log = TRUE, addexpected = FALSE)
            }
            if (5 %in% subplots) {
              index.fn(log = TRUE)
            }
            if (6 %in% subplots) {
              obs_vs_exp.fn(log = TRUE)
            }
          }
          
          # print subplots 4-6 to PNG files
          if (print) {
            if (4 %in% subplots & datplot) {
              file <- paste0(
                "index4_logcpuedata_",
                gsub(" ", "", Fleet),
                ".png"
              )
              caption <- paste0(
                "Log index data for ",
                Fleet,
                ". ",
                "Lines indicate 95% uncertainty interval around index values ",
                "based on the model assumption of ",
                error_caption,
                ". ",
                "Thicker lines (if present) indicate input uncertainty before addition of ",
                "estimated additional uncertainty parameter."
              )
              plotinfo <- save_png(
                plotinfo = plotinfo,
                file = file,
                plotdir = plotdir,
                pwidth = pwidth,
                pheight = pheight,
                punits = punits,
                res = res,
                ptsize = ptsize,
                caption = caption
              )
              index.fn(log = TRUE, addexpected = FALSE)
              dev.off()
            }
            if (5 %in% subplots) {
              file <- paste0("index5_logcpuefit_", gsub(" ", "", Fleet), ".png")
              caption <- paste0(
                "Fit to log index data on log scale for ",
                Fleet,
                ". ",
                "Lines indicate 95% uncertainty interval around index values ",
                "based on the model assumption of ",
                error_caption,
                ". ",
                "Thicker lines (if present) indicate input uncertainty before addition of ",
                "estimated additional uncertainty parameter."
              )
              plotinfo <- save_png(
                plotinfo = plotinfo,
                file = file,
                plotdir = plotdir,
                pwidth = pwidth,
                pheight = pheight,
                punits = punits,
                res = res,
                ptsize = ptsize,
                caption = caption
              )
              index.fn(log = TRUE)
              dev.off()
            }
            if (6 %in% subplots) {
              file <- paste0(
                "index6_log_obs_vs_exp_",
                gsub(" ", "", Fleet),
                ".png"
              )
              caption <- paste(
                "log(observed) vs. log(expected) index values with smoother for",
                Fleet
              )
              plotinfo <- save_png(
                plotinfo = plotinfo,
                file = file,
                plotdir = plotdir,
                pwidth = pwidth,
                pheight = pheight,
                punits = punits,
                res = res,
                ptsize = ptsize,
                caption = caption
              )
              obs_vs_exp.fn(log = TRUE)
              dev.off()
            }
          }
        } # end plots that require lognormal error
        
        # plots 7 and 8 related to time-varying catchability
        if (plot) {
          if (7 %in% subplots & time) {
            timevarying_q.fn()
          }
          if (8 %in% subplots & time2) {
            q_vs_vuln_bio.fn()
          }
        } # end plot to graphics device
        
        if (print) {
          if (7 %in% subplots & time) {
            file <- paste0(
              "index7_timevarying_q_",
              gsub(" ", "", Fleet),
              ".png"
            )
            caption <- paste("Timeseries of catchability for", Fleet)
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            timevarying_q.fn()
            dev.off()
          }
          if (8 %in% subplots & time2) {
            file <- paste0(
              "index8_q_vs_vuln_bio_",
              gsub(" ", "", Fleet),
              ".png"
            )
            caption <-
              paste0(
                "Catchability vs. vulnerable biomass for fleet ",
                Fleet,
                "<br> \n",
                "This plot should illustrate curvature of nonlinear catchability relationship<br> \n",
                "or reveal patterns associated with random-walk catchability."
              )
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            q_vs_vuln_bio.fn()
            dev.off()
          }
        } # end print to PNG
        
        # residual/deviation plots
        if (plot) {
          if (10 %in% subplots & all(cpueuse[["Obs"]] >= 0)) {
            index_resids.fn(option = 1)
          }
          if (11 %in% subplots) {
            index_resids.fn(option = 2)
          }
          if (12 %in% subplots) {
            index_resids.fn(option = 3)
          }
        }
        if (print) {
          #### residuals based on total uncertainty
          if (10 %in% subplots & all(cpueuse[["Obs"]] >= 0)) {
            file <- paste0(
              "index10_resids_SE_total_",
              gsub(" ", "", Fleet),
              ".png"
            )
            caption <- paste0("Residuals of fit to index for ", Fleet, ".")
            if (error == 0) {
              caption <- paste0(
                caption,
                "<br>Values are (log(Obs) - log(Exp))/SE ",
                "where SE is the total standard error including any ",
                "estimated additional uncertainty."
              )
            } else {
              caption <- paste0(
                caption,
                "<br>Values are based on the total standard error ",
                "including any estimated additional uncertainty."
              )
            }
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            index_resids.fn(option = 1)
            dev.off()
          }
          #### residuals based on input uncertainty
          if (
            11 %in%
            subplots &
            show_input_uncertainty &&
            any(!is.null(cpueuse[["SE_input"]][include])) &&
            any(cpueuse[["SE_input"]] > cpueuse[["SE"]])
          ) {
            file <- paste0(
              "index11_resids_SE_input_",
              gsub(" ", "", Fleet),
              ".png"
            )
            caption <- paste0("Residuals for fit to index for ", Fleet, ".")
            if (error == 0) {
              caption <- paste0(
                caption,
                "<br>Values are (log(Obs) - log(Exp))/SE_input ",
                "where SE_input is the input standard error",
                "excluding any estimated additional uncertainty."
              )
            } else {
              caption <- paste0(
                caption,
                "<br>Values are based on the input standard error ",
                "excluding any estimated additional uncertainty."
              )
            }
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            index_resids.fn(option = 2)
            dev.off()
          }
          #### simple deviation plot
          if (12 %in% subplots) {
            file <- paste0(
              "index12_resids_SE_total_",
              gsub(" ", "", Fleet),
              ".png"
            )
            caption <- paste0("Deviations for fit to index for ", Fleet, ".")
            if (error != -1) {
              # lognormal or T-distributed error
              caption <- paste0(
                caption,
                "<br>Values are log(Obs) - log(Exp) ",
                "and thus independent of index uncertainty."
              )
            }
            if (error == -1) {
              # normal error
              caption <- paste0(
                caption,
                "<br>Values are Obs - Exp ",
                "and thus independent of index uncertainty."
              )
            }
            plotinfo <- save_png(
              plotinfo = plotinfo,
              file = file,
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              caption = caption
            )
            index_resids.fn(option = 3)
            dev.off()
          }
        } # end if(print)
      } # end check for any values to include
    } # end loop over fleets
    
    ### standardized plot of all CPUE indices
    if (datplot == TRUE & nrow(allcpue) > 0) {
      all_index.fn <- function() {
        main <- "All index plot"
        if (!mainTitle) {
          main <- ""
        }
        xlim <- c(
          min(allcpue[["year"]], na.rm = TRUE) - 1,
          max(allcpue[["year"]], na.rm = TRUE) + 1
        )
        
        # change year range if requested
        xlim[1] <- max(xlim[1], minyr)
        xlim[2] <- min(xlim[2], maxyr)
        
        # set y limits
        ylim <- c(0, 1.05 * max(allcpue[["stdvalue"]], na.rm = TRUE))
        # set colors
        if (!is.null(fleetcols) & length(fleetcols) >= nfleets) {
          usecols <- fleetcols
        } else {
          usecols <- rich.colors.short(
            max(allcpue[["Index"]], na.rm = TRUE),
            alpha = 0.7
          )
          if (max(allcpue[["Index"]], na.rm = TRUE) >= 2) {
            usecols <- rich.colors.short(
              max(allcpue[["Index"]], na.rm = TRUE) + 1,
              alpha = 0.7
            )[-1]
          }
        }
        # make empty plot
        if (!add) {
          plot(
            0,
            type = "n",
            xlab = labels[1],
            main = main,
            cex.main = cex.main,
            col = usecols[1],
            ylab = labels[8],
            xlim = xlim,
            ylim = ylim,
            yaxs = "i"
          )
        }
        # add points and lines for each fleet
        for (ifleet in fleetvec) {
          lines(
            x = allcpue[["year"]][allcpue[["Index"]] == ifleet],
            y = allcpue[["stdvalue"]][allcpue[["Index"]] == ifleet],
            col = adjustcolor(usecols[ifleet], alpha.f = 0.7),
            lwd = 2
          )
          points(
            x = allcpue[["year"]][allcpue[["Index"]] == ifleet],
            y = allcpue[["stdvalue"]][allcpue[["Index"]] == ifleet],
            pch = pch1,
            bg = adjustcolor(usecols[ifleet], alpha.f = 0.7),
            col = gray(0, alpha = 0.7),
            cex = cex
          )
        }
        legend(
          legendloc,
          legend = fleetnames[fleetvec],
          ncol = 2,
          bty = "n",
          pch = pch1,
          col = gray(0, alpha = 0.7),
          pt.bg = usecols[fleetvec]
        )
      } # end all_index.fn
      if (plot & (9 %in% subplots)) {
        all_index.fn()
      }
      if (print & (9 %in% subplots)) {
        file <- paste0("index9_standcpueall", ".png")
        caption <- paste(
          "Standardized indices overlaid.",
          "Each index is rescaled to have mean observation = 1.0."
        )
        if (any_negative) {
          caption <- paste(
            caption,
            "Indices with negative observations have been excluded."
          )
        }
        plotinfo <- save_png(
          plotinfo = plotinfo,
          file = file,
          plotdir = plotdir,
          pwidth = pwidth,
          pheight = pheight,
          punits = punits,
          res = res,
          ptsize = ptsize,
          caption = caption
        )
        all_index.fn()
        dev.off()
      }
    } # end datplot
    
    if (!is.null(plotinfo)) {
      plotinfo[["category"]] <- "Index"
    }
    return(invisible(plotinfo))
  } # end function