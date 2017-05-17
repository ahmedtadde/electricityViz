 source("data/functions.R")
libraries()


# get base DF
df_base <- get.data()


# ui.R variables
# -------------------------------------
choices <- list(
    tdus = unique(df_base$TDU),
    reps = unique(df_base$REP),
    rate_types = unique(df_base$RATE_TYPE),
    booleans = c("ALL","TRUE","FALSE"),
    usage = c("KWH500", "KWH1000", "KWH2000")
)


# # server.R variables and functions
# # -------------------------------------
# # histogram_tooltip helper function
# histogram_tooltip <- function(data) {
#     if(is.null(data)) return(NULL)
#     sprintf("Price: %s - %s c/kWh<br />
#             Count: %s<br />",
#             round(data$xmin, 1), round(data$xmax, 1),
#             data$stack_upr - data$stack_lwr)
# }


# color map
REP_COLOR_MAP <- c(
  "REP1" = "#1f77b4",
  "REP2" = "#ff7f0e",
  "REP3" = "#2ca02c",
  "OTHER" = "#dddddd"
)