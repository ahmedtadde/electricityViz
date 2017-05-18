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
  library(plotly)
  # library(ggvis)
  library(scales)
  library(DT)
  library(markdown)
  library(foreach)
}


`%notin%` <-  Negate(`%in%`)

# =========================================================================
# function:  color.mapper
# @description: 
# @return: 
# =========================================================================

color.mapper <- function(data, map, input1,input2,input3, mode = "unique"){
  
  if (mode %in% "unique"){
    result <- rep("", length(unique(data$REP)))
    names(result) <- sort(unique(data$REP))
  }else{
    result <- rep("dummy", length(data$REP))
    names(result) <- data$REP
  }
  
  result <- foreach(i = 1:length(result), .combine = c) %do% {
    if(setequal(names(result)[i], input1)){
      result[i] <- unname(map)[1]
    }else if(setequal(names(result)[i], input2)){
      result[i] <- unname(map)[2]
    }else if(setequal(names(result)[i], input3)){
      result[i] <- unname(map[3])
    }else{
      result[i] <- unname(map[4])
    }
    
  }
  
  if (mode %in% "unique"){
    names(result) <- sort(unique(data$REP))
  }else{
    names(result) <- data$REP
  }
  
  return(result)
  
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
        c("KWH500","KWH1000","KWH2000") := list(
            as.numeric(KWH500) * 100,
            as.numeric(KWH1000) * 100,
            as.numeric(KWH2000) * 100
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
    
    return(setkey(fread("./data/data.csv")[, c("PREPAID","TOU","PROMOTION"):=
                                             list(toupper(as.character(PREPAID)),
                                                  toupper(as.character(TOU)),
                                                  toupper(as.character(PROMOTION))
                                                  )
                                          ], ID
                  )
           )
    
  }else{
    
    if(update %in% c("update", 1, "Update","yes","Yes","new","New"))
      
      "http://www.powertochoose.org/en-us/Plan/ExportToCsv" %>% 
        
        fread(
          header=TRUE, 
          stringsAsFactors=FALSE
        ) %>% 
        
        clean.data() %>%
        
        fwrite(file="./data/data.csv")
        
    return(setkey(fread("./data/data.csv")[, c("PREPAID","TOU","PROMOTION"):=
                                             list(toupper(as.character(PREPAID)),
                                                  toupper(as.character(TOU)),
                                                  toupper(as.character(PROMOTION))
                                                  )
                                          ], ID
                  )
           )
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