source("data/functions.R")
libraries()
# color map
REP_COLOR_MAP <- c(
  "REP1" = "#1f77b4",
  "REP2" = "#ff7f0e",
  "REP3" = "#2ca02c",
  "OTHER" = "#dddddd"
)
data <- get.data()
result <- color.mapper(data, REP_COLOR_MAP,"YEP","TEXPO ENERGY","AMBIT ENERGY")
