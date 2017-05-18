 source("data/functions.R")
libraries()


# get base DF
df_base <- get.data()


# ui.R variables
# -------------------------------------
# Input choices
choices <- list(
    tdus = unique(df_base$TDU),
    reps = unique(df_base$REP),
    rate_types = unique(df_base$RATE_TYPE),
    booleans = c("ALL","TRUE","FALSE"),
    usage = c("KWH500", "KWH1000", "KWH2000")
  )


# color map
REP_COLOR_MAP <- c(
  "REP1" = "#1f77b4",
  "REP2" = "#ff7f0e",
  "REP3" = "#2ca02c",
  "OTHER" = "#dddddd"
)