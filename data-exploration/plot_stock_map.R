png(
  filename = here::here("report/figures/stock_range_map.png"),
  width = 6,
  height = 8,
  units = "in",
  res = 300
)
mod.names <- c("Management  Range", "Assessment Range")
mod.cols <- c("blue", "lightblue", "yellow")

map(
  'worldHires',
  regions = c("Canada", "Mexico"),
  xlim = c(-130, -114),
  ylim = c(31, 51),
  col = 'grey',
  fill = TRUE,
  interior = TRUE,
  ,
  lwd = 1
)

# map with US states
map(
  'state',
  regions = c(
    "Wash",
    "Oreg",
    "Calif",
    "Idaho",
    "Montana",
    "Nevada",
    "Arizona",
    "Utah"
  ),
  add = TRUE,
  col = 'grey',
  fill = TRUE,
  interior = TRUE,
  lwd = 1
)
axis(2, at = seq(32, 50, 2), lab = paste0(seq(32, 50, 2), "°N"), las = 1)
axis(1, at = seq(-130, -114, 4), lab = paste0(abs(seq(-130, -114, 4)), "°W"))

## add vertical lines indicating range for each stock
# North of 36
text(-128 - .8, 40.5, mod.names[1], srt = 90)
latrange <- c(32.5, 36) + c(.2, -.2)
lines(rep(-128, 2), latrange, lwd = 10, col = mod.cols[1])

# South of 36
latrange <- c(36, 48.5) + c(.2, -.2)
lines(rep(-128, 2), latrange, lwd = 10, col = mod.cols[2])
#text(-128-.8, mean(latrange), mod.names[1], srt=90)
# Assessment area
latrange <- c(32.5, 48.5) + c(.2, -.2)
lines(rep(-126, 2), latrange, lwd = 10, col = mod.cols[3])
text(-126 - .8, mean(latrange), mod.names[2], srt = 90)

text(-120, 50, "Canada")
text(-120, 47, "Washington")
text(-121, 44, "Oregon")
text(-119.5, 37, "California")
text(-115.5, 32.1, "Mexico")
dev.off()
