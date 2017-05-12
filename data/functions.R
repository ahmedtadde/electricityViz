# =========================================================================
# function libraries
# @description: load all libraries for project
# @return: N/A
# =========================================================================
libraries <- function(){
  library(magrittr)
  library(data.table)
  library(dplyr)
  library(shiny)
  library(ggvis)
  library(scales)
  library(DT)
  library(markdown)
  library(foreach)
}




# =========================================================================
# function:  color.mapper
# @description: 
# @return: 
# =========================================================================

color.mapper <- function(vector, map, input1,input2,input3){
  
  mapper <- function(x){
    switch(
      x,
      input1= map[1],
      input2= map[2],
      input3= map[3],
      map[4]
    )
  }
  
  return(foreach(i = 1:length(vector), .combine = c) %do% {mapper(vector[i])})
}

# =========================================================================
# function:  clean.data
# @description: Renaming data.table columns, subset and define types of columns 
#               of interest and define column types
# @return: cleaned data.table
# =========================================================================
clean.data <- function(df){
  columns <- c("ID", "TDU", "REP", "PRODUCT",
               "KWH500", "KWH1000", "KWH2000",
               "FEES", "PREPAID", "TOU",
               "FIXED", "RATE_TYPE", "RENEWABLE",
               "TERM_LENGTH", "CANCEL_FEE", "WEdBSITE",
               "TERMS", "TERMS_URL", "PROMOTION", "PROMOTION_DESCRIPTION",
               "EFL_URL", "ENROLL_URL", "PREPAID_URL", "ENROLL_PHONE"
              )
  
  setnames(df, names(df)[1:24],  columns)  # rename columns
  rm(columns)
  
  return(
    
    df[, .(ID, TDU, REP, PRODUCT,
           KWH500, KWH1000, KWH2000,
           RATE_TYPE, TERM_LENGTH, RENEWABLE,
           PREPAID, TOU, PROMOTION, EFL_URL
           ) 
       
      ][,
        # convert units from $/kWh to c/kWh
        c("KWH500","KWH1000","KWH2000","PREPAID","TOU","PROMOTION") := list(
            as.numeric(KWH500) * 100,
            as.numeric(KWH1000) * 100,
            as.numeric(KWH2000) * 100,
            as.logical(PREPAID),
            as.logical(TOU),
            as.logical(PROMOTION)
          )
      ]
  )
}


# =========================================================================
# function get.data
#
# @description: get data from url.
# @return: raw dataframe returned by data.table's fread from provided url.
# =========================================================================
get.data <- function(update = NULL) {

  if(is.null(update)){
    
    return(setkey(fread("./data/data.csv"), ID))
    
  }else{
    
    if(update %in% c("update", 1, "Update","yes","Yes","new","New"))
      
      "http://www.powertochoose.org/en-us/Plan/ExportToCsv" %>% 
        
        fread(
          header=TRUE, 
          stringsAsFactors=FALSE
        ) %>% 
        
        clean.data() %>%
        
        fwrite(file="./data/data.csv")
        
        return(setkey(fread("./data/data.csv"), ID))
  }
}



# server.R variables and functions
# -------------------------------------
# histogram_tooltip helper function
histogram_tooltip <- function(data) {
  if(is.null(data)) return(NULL)
  sprintf("Price: %s - %s c/kWh<br />
          Count: %s<br />",
          round(data$xmin, 1), round(data$xmax, 1),
          data$stack_upr - data$stack_lwr)
}