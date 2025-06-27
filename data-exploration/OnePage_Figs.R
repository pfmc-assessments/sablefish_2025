library(ggplot2)
library(dplyr)
library(stringr)

############################################
#Plot Net Change in Total Biomass Figure
#change directory/file location as needed
mydat <- read.csv("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary\\NetBiomass.csv")
p<-ggplot(data=mydat,aes(x=Year,y=Metric,fill=Population)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("#51b364","#dc322f")) +
  labs(x="Year", y = "Net Change in Total Biomass (%)") +
  coord_cartesian(ylim=c(-20,20))
  p <- p + theme(legend.position = c(0.1,0.90),
                 legend.text=element_text(size=18),
                 legend.title=element_text(size=18),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 panel.grid.major = element_line(color = "gray90"),
                 panel.grid.minor = element_line(color = "gray95"),
                 axis.text.x = element_text(color = "black", size = 18),
                 axis.text.y = element_text(color = "black", size = 18),  
                 axis.title.x = element_text(color = "black", size = 18),
                 axis.title.y = element_text(color = "black", size = 18)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5
    )
ggsave("NetBiomassChange.png", p,width=12, height=8)
ggsave("NetBiomassChange.pdf", p,width=12, height=8)
############################################

############################################
#Circle bar (radar) graph for recent recruitment strength with coloring by above/below age at 50% mature (age 7 for sablefish)
#change directory/file location as needed
setwd("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary")
radar <- read.csv("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary\\RecruitRadar.csv")
Equil_rec <- 25981  #note: taken from stock assessment mean estimate
radar$Year <- as.factor(radar$Year)

plt <- ggplot(radar) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 30000),
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
  # Lollipop shaft for mean recruitment
  geom_segment(
    aes(
      x = Year,
      y = 0,
      xend = Year,
      yend = 90000
    ),
    linetype = "dashed",
    linewidth = 0.5,
    color = "gray12"
  ) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1)
  ) +
# Add dots to represent the mean gain
  geom_hline(
    yintercept = Equil_rec,
    linetype = "dashed",
    color = "maroon",
    size = 0.4
  ) + 
  coord_polar()

plt <- plt +
  # Annotate the bars and the lollipops so the reader understands the scaling
  annotate(
    x = 8.5, 
    y = Equil_rec*1.4,
    label = "Equilibrium \n Recruitment",
    geom = "text",
    #angle = -20,
    color = "maroon",
    size = 5,
    family = "sans"
  ) +
  # Annotate custom scale inside plot
  annotate(
    x = 16.7, 
    y = 32000, 
    label = "30", 
    size = 9,
    geom = "text", 
    color = "gray12", 
    family = "sans"
  ) +
  annotate(
    x = 16.7, 
    y = 62000, 
    label = "60", 
    size = 9,
    geom = "text", 
    color = "gray12", 
    family = "sans"
  ) +
  annotate(
    x = 16.7, 
    y =92000, 
    label = "90 billion fish", 
    size = 9,
    geom = "text", 
    color = "gray12", 
    family = "sans"
  ) +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-40000, 105500),
    expand = c(0, 0),
    breaks = c(0, 30000, 60000, 90000)
  ) +
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 16),
    # Move the legend to the bottom
    legend.position = c(0.92,0.90),
    legend.text=element_text(size=14),
    legend.title=element_text(size=14)
  ) +
  guides(fill=guide_legend(title="Mature (> 50%)"))


plt <- plt + 
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "sans"),
    
   # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("Rec_radar.png", plt,width=8, height=8)
ggsave("Rec_radar.pdf", plt,width=8, height=8)
############################################


############################################
#TSC style graph to show catch and depletion
#Start with slight adaptation to r4ss TSC plot function
#Change TSC function manually for now to say "Relative Spawning Output" instead of Depletion and change colors
#run this function first, then build graph afterwards
####
TSCplot <- function(
    SSout,
    yrs = "default",
    ylimBar = "default",
    ylimDepl = c(0, 1.025),
    colBar = "slategray3",
    cexBarLabels = 1.1,
    cex.axis = 1.1,
    space = 0.0,
    pchDepl = 19,
    colDepl = "lightsalmon3",
    lwdDepl = 3,
    shiftDepl = 0.25,
    pchSpace = 5,
    ht = 4,
    wd = 7,
    labelLines = 2.8,
    makePDF = NULL,
    makePNG = NULL,
    MCMC = FALSE
) {
  ### Plots the barchart of catches and depletion trajctory for the TSC report
  
  if (!is.null(makePDF) & !is.null(makePNG)) {
    stop("Cannot specify both makePDF and makePNG. Choose only one.\n")
  }
  
  indVirgin <- which(SSout[["timeseries"]][["Era"]] == "VIRG")
  ind <- which(SSout[["timeseries"]][["Era"]] == "TIME")
  ind <- c(ind, max(ind) + 1)
  if (yrs[1] == "default") {
    yrs <- unique(sort(SSout[["timeseries"]][["Yr"]][ind]))
  }
  
  # get catches + discards summed over areas
  deadCatch <- SSplotCatch(SSout, plot = FALSE, verbose = FALSE)[[
    "totcatchmat"
  ]]
  if (ncol(deadCatch) > 2) {
    # sum over fisheries
    deadCatch <- cbind(
      apply(deadCatch[, -ncol(deadCatch)], 1, sum),
      deadCatch[, ncol(deadCatch)]
    )
  }
  deadCatch <- deadCatch[match(yrs, deadCatch[, 2]), ]
  rownames(deadCatch) <- yrs
  
  if (!MCMC) {
    SBzero <- SSout[["SBzero"]]
    SB <- SSout[["derived_quants"]][
      substring(SSout[["derived_quants"]][["Label"]], 1, 4) == "SSB_",
    ]
    SB <- SB[match(as.character(yrs), substring(SB[["Label"]], 5)), ]
    depl <- SSout[["derived_quants"]][
      substring(SSout[["derived_quants"]][["Label"]], 1, 7) == "Bratio_",
    ]
    depl <- depl[match(as.character(yrs), substring(depl[["Label"]], 8)), ]
    SP <- data.frame(
      Yr = yrs,
      SpawnBio = SB[, "Value"],
      Depl = depl[, "Value"],
      Dead_Catch = deadCatch[, 1]
    )
  }
  if (MCMC) {
    if (is.null(SSout[["mcmc"]])) {
      stop(
        "There is no mcmc element on the model list.\n",
        "Set MCMC = FALSE or add in the mcmc element to the list.\n"
      )
    }
    SBzero <- median(SSout[["mcmc"]][["SSB_Virgin"]])
    SB <- SSout[["mcmc"]][, substring(names(SSout[["mcmc"]]), 1, 4) == "SSB_"]
    SB <- apply(
      SB[, match(as.character(yrs), substring(names(SB), 5))],
      2,
      median
    )
    depl <- SSout[["mcmc"]][,
                            substring(names(SSout[["mcmc"]]), 1, 7) == "Bratio_"
    ]
    tmp1 <- match(as.character(yrs), substring(names(depl), 8)) # can have an NA in it and will cause an error
    tmp2 <- tmp1[!is.na(tmp1)] # remove NA's to get the medians
    depl <- apply(depl[, tmp2], 2, median)
    depl <- depl[match(as.character(yrs), substring(names(depl), 8))]
    SP <- data.frame(
      Yr = yrs,
      SpawnBio = SB,
      Depl = depl,
      Dead_Catch = deadCatch[, 1]
    )
  }
  
  if (ylimBar == "default") {
    ylimBar <- c(0, max(SP[["Dead_Catch"]], na.rm = TRUE) * 1.05)
  }
  ind <- seq(1, nrow(SP), pchSpace)
  
  if (is.null(makePDF) & is.null(makePNG)) {
    dev.new(height = ht, width = wd)
  }
  if (!is.null(makePDF)) {
    pdf(file = makePDF, width = wd, height = ht)
  }
  if (!is.null(makePNG)) {
    png(
      filename = makePNG,
      width = wd,
      height = ht,
      units = "in",
      pointsize = 10,
      res = 300
    )
  }
  par(mar = c(4, 5, 2, 5))
  barOut <- barplot(
    SP[["Dead_Catch"]],
    names.arg = SP[["Yr"]],
    ylim = ylimBar,
    ylab = "",
    col = "slategray3",
    cex = cexBarLabels,
    cex.axis = cex.axis,
    space = space,
    xlim = c(0, nrow(SP)),
    axisnames = FALSE
  )
  axis(1, at = barOut[ind, 1], labels = yrs[ind])
  par(new = TRUE)
  xpts <- (0:(nrow(SP) - 1)) + shiftDepl
  plot(
    xpts,
    SP[["Depl"]],
    yaxt = "n",
    yaxs = "i",
    xaxt = "n",
    ylab = "",
    xlab = "",
    ylim = ylimDepl,
    type = "l",
    col = colDepl,
    lwd = lwdDepl,
    cex.axis = cex.axis,
    xlim = c(0, nrow(SP))
  )
  points(xpts[ind], SP[["Depl"]][ind], pch = pchDepl, col = colDepl)
  axis(4, at = seq(ylimDepl[1], ylimDepl[2], 0.1), cex.axis = cex.axis)
  mtext(
    c("Year", "Total mortality catch (t)", "Relative Spawning Output"),
    side = c(1, 2, 4),
    line = labelLines,
    cex = 1.5
  )
  
  if (!is.null(makePDF)) {
    dev.off()
    message("The plot is in pdf file ", makePDF)
  }
  if (!is.null(makePNG)) {
    dev.off()
    message("The plot is in png file", makePNG)
  }
  
  invisible(SP)
}
####
#change directory/file location as needed
setwd("C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\TwoPageSummary")
base <- SS_output(dir="C:\\Users\\Aaron.Berger\\Documents\\AMB\\Groundfish\\Assessments\\Sablefish2025\\model\\Base\\8.36_base_model")
png(file="Sablefish_2025_TSC.png", width=300, height=200)
TSCplot(base,ylimDepl = c(0,1.4),pchSpace=1,MCMC=F,colBar="slategray3",colDepl="navyblue",makePNG = "Sablefish_2025_TSC.png")
TSCplot(base,ylimDepl = c(0,1.4),pchSpace=1,MCMC=F,colBar="slategray3",colDepl="navyblue",makePDF = "Sablefish_2025_TSC.pdf")
############################################







