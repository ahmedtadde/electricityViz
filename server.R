shinyServer(function(input, output) {
    # --------------------------------
    # dataframe resource
    resource.df <- reactive({
      if( "" %in% c(input$prepaid,input$rate_type,input$tou,input$promotion,
                    input$tdu,input$rep1,input$rep2,input$rep3)) return(NULL)
      
      
      if(input$prepaid %in% "ALL"){ prepaid <- c("TRUE","FALSE")}else{prepaid <- input$prepaid}
      if(input$tou %in% "ALL"){ tou <- c("TRUE","FALSE")}else{tou<- input$tou}
      if(input$promotion %in% "ALL"){ promotion <- c("TRUE","FALSE")}else{promotion <- input$promotion}
      
      copy(df_base)[,
                    PRICE:= df_base[, get(input$usage)]
                  ][
                    PREPAID %in% prepaid &
                    TOU %in% tou &
                    PROMOTION %in% promotion &
                    PRICE %between% list(1,25) & # filter out unreasonable prices
                    TDU %in% input$tdu &
                    RATE_TYPE %in% input$rate_type &
                    TERM_LENGTH  %between% list(input$term_lengths[1], input$term_lengths[2]) &
                    RENEWABLE  %between% list(input$renewables[1], input$renewables[2])
                  ]-> df
      if( is.null(df) | dim(df)[1] == 0 | !is.data.table(df)) return(NULL)
      
      df[,RANK := frank(PRICE,ties.method = "min")
         ][, REP_COLOR:= color.mapper(df,REP_COLOR_MAP,input$rep1,input$rep2,input$rep3, "all")]
      
      return(setorder(df, RANK))
    })
    
    
    # Server outputs
    # --------------------------------
    # datatable output
    output$datatable <- renderDataTable({
      
      if (is.null(resource.df())) return(NULL)
      if (!is.data.table(resource.df())) return(NULL)
      
      datatable(

        resource.df(),

        filter = 'top',
        rownames = FALSE,
        selection="multiple", 
        escape=FALSE,
        extensions = c(
          'Buttons',
          'Responsive'
        ),
        
        options = list(
          dom = 'Blrtip',
          autoWidth = TRUE,
          buttons = list('excel', 'csv'),
          Responsive = TRUE,
          lengthMenu = list(c(10, 20, 50, 100), c('10', '20','50','100'))
        )
      )
    })
    
    
    
    # summary output
    output$rankingSummary <- renderUI({
        # subsets of data for summary table
      
        if (is.null(resource.df())) return(HTML("<b> There are no records matching 
                                                the combination of parameters chosen. 
                                                Try other selections.</b>"
                                                )
                                           )
      
      if (!is.data.table(resource.df())) return(HTML("<b>ISSUE WITH RANK AND/OR REP_COLOR VARIABLES</b>"))
      
      
        df_best <-  resource.df()[RANK ==1]
        df_top10 <- resource.df()[RANK <= 10]
        df_top30 <- resource.df()[RANK <= 30]
        
        summary <- HTML(
            sprintf("
                    <b>Best:</b> %s c/kWh <small><i>( %s )</i></small><br />
                    <b>Top 10:</b> %s c/kWh<br />
                    <b>Top 30:</b> %s c/kWh<br />
                    <b>Mean:</b> %s c/kWh <small><i>( %s products )</i></small><br /><br />
                    <b>Top 10 PRODS:</b> <small>%s</small><br />
                    ",
                    min(df_best$PRICE), paste(df_best$PRODUCT, collapse=", "),
                    max(df_top10$PRICE),
                    max(df_top30$PRICE),
                    round(resource.df()[, mean(PRICE, na.rm = T)], 1),  
                    resource.df()[PRICE < mean(PRICE, na.rm = T), .N],
                    paste(df_top10$PRODUCT, collapse=", ")
                  )
        )
        
        
        return(summary)
    })
    
    
    # ggvis outputs
    # --------------------------------
    # ggvis tooltip helper
    tooltip_helper <- function(data) {
      df = resource.df()
      sprintf("<b class='text-warning'><i>%s</i></b><br />
              <b>REP:</b> %s<br />
              <b>RANK:</b> #%s<br />
              <b>PRICE:</b> %sc/kWh<br />
              <b>TERM:</b> %sM<br />
              <b>PROMOTION:</b> %s<br />",
              df$PRODUCT[df$ID == data$ID],
              df$REP[df$ID == data$ID],
              df$RANK[df$ID == data$ID],
              df$PRICE[df$ID == data$ID],
              df$TERM_LENGTH[df$ID == data$ID],
              df$PROMOTION[df$ID == data$ID])
    }
    
    # rankings_plot ggvis output
    if(!is.null(resource.df)){
    resource.df %>% 
      ggvis(x=~PRICE, y=~REP, size=~TERM_LENGTH, fill:=~REP_COLOR, stroke="lightsteelblue", key:=~ID) %>% 
      layer_points() %>% set_options(height = 600, width = 1100) %>%
      add_axis("x", subdivide=5) %>%
      add_axis("y", title="") %>%
      hide_legend("stroke") %>%
      add_tooltip(tooltip_helper, "hover") %>%
      bind_shiny("rankings_plot")
    }
    
    
    # market_histogram ggvis output
    if(!is.null(resource.df)){
      resource.df %>%
        ggvis(x=~PRICE, fill:=~REP_COLOR, stroke="lightsteelblue") %>%
        set_options(height = 600, width = 1100) %>%
        group_by(REP_COLOR) %>%
        layer_histograms(width=input_slider(label="Binwidth", min=0.1, max=2, value=0.2, step=0.1)) %>%
        add_axis("x", subdivide=5) %>%
        hide_legend("stroke") %>%
        add_tooltip(histogram_tooltip, "hover") %>%
        bind_shiny("market_histogram", "market_histogram_slider")
    }
    
    
    # market_scatterplot ggvis output
    if(!is.null(resource.df)){
    resource.df %>%
      ggvis(x=~PRICE, y=~TERM_LENGTH, size:=50, fill:=~REP_COLOR, stroke="lightsteelblue", key:=~ID) %>%
      layer_points() %>% set_options(height = 600, width = 1100) %>%
      add_axis("x", subdivide=5) %>%
      hide_legend("stroke") %>%
      add_tooltip(tooltip_helper, "hover") %>%
      bind_shiny("market_scatterplot")
    }
})