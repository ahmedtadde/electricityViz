source("data/functions.R")
libraries()
# color map
REP_COLOR_MAP <- c(
  "REP1" = "#1f77b4",
  "REP2" = "#ff7f0e",
  "REP3" = "#2ca02c",
  "OTHER" = "#dddddd"
)
# data <- get.data()
# data[, COLORMAP:= color.mapper(data, REP_COLOR_MAP,"YEP","TEXPO ENERGY","AMBIT ENERGY", mode = "all")]

test <- data[, .(COLORMAP,KWH500), by = list(REP,PRODUCT)][, PRODUCTTAG:= paste0(PRODUCT," (",REP,")")]
colors <- test[, unique(COLORMAP), by =PRODUCTTAG]
color.map <- colors[, V1]
names(color.map) <- colors[,PRODUCTTAG]