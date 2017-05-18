shinyServer(function(input, output) {
    
    # Reactive resources
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
                    PRICE <= 25 &
                    PRICE >= 1 &  # filter out unreasonable data
                    TDU == input$tdu &
                    RATE_TYPE %in% input$rate_type &
                    TERM_LENGTH >= input$term_lengths[1] &
                    TERM_LENGTH <= input$term_lengths[2] &
                    RENEWABLE >= input$renewables[1] &
                    RENEWABLE <= input$renewables[2]
                  ]-> df
      if( is.null(df) | dim(df)[1] == 0 | !is.data.table(df)) return(NULL)
      
      
      df[,RANK := min_rank(PRICE)]
      
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
    
    # rankings_plot plotly output
    output$rankings_plot <- renderPlotly({
      
      if(is.null(resource.df()) | !is.data.table(resource.df())) return(NULL)
      # color.map <- color.mapper(resource.df(), REP_COLOR_MAP, input$rep1,input$rep2,input$rep3)
      
      ax <- list(
        title = "PRICE",
        titlefont= list(
          family = "Courier New, monospace",
          size = 17
        ),
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE
      )
      
      ay <- list(
        title = "Retail Electric Provide (REP)",
        titlefont= list(
          family = "Courier New, monospace",
          size = 17
        ),
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
        
      )
      
      

      df<- copy(resource.df())[, PRODUCTTAG:= paste0(PRODUCT," (",REP,")")
                              ][, COLORMAP:= unname(color.mapper(resource.df(), REP_COLOR_MAP, input$rep1,input$rep2,input$rep3, mode = ""))
                              ]
      
      colors <- df[, unique(COLORMAP), by =PRODUCTTAG]
      color.map <- colors[, V1]
      names(color.map) <- colors[,PRODUCTTAG]
      
      
      
      plot_ly(data = df[, PRODUCTTAG := factor(PRODUCTTAG, levels = unique(PRODUCTTAG))], 
              x = ~PRICE,
              y = ~REP,
              mode = 'markers',
              type = 'scatter', 
              color = ~PRODUCTTAG,
              colors = color.map,
              hoverinfo = 'text',
              text = ~paste0(
                '</br> Product: ', PRODUCT,
                '</br> Price: $', PRICE,
                '</br> Rank: ', RANK,
                '</br> Rate Type: ', RATE_TYPE,
                '</br> Term length: ', TERM_LENGTH,
                '</br> Retail Electric Provide (REP): ', REP,
                '</br> Transmission and Distribution Service Provider (TDU): ', TDU
              )
      )%>%
        layout(xaxis = ax, yaxis = ay) #showlegend = FALSE
    })
    # resource.df %>%
    #   ggvis(x=~PRICE, y=~REP, size=~TERM_LENGTH, fill:=~REP_COLOR, key:=~ID) %>%
    #   layer_points() %>%
    #   add_axis("x", subdivide=4) %>%
    #   add_axis("y", title="") %>%
    #   hide_legend("stroke") %>%
    #   add_tooltip(tooltip_helper, "hover") %>%
    #   bind_shiny("rankings_plot")

    
    

    #================================ market_histogram plotly output
    # resource.df %>%
    #   ggvis(x=~PRICE, fill:=~REP_COLOR) %>%
    #   group_by(REP_COLOR) %>%
    #   layer_histograms(width=input_slider(label="Binwidth", min=0.1, max=2, value=0.2, step=0.1)) %>%
    #   add_axis("x", subdivide=4) %>%
    #   hide_legend("stroke") %>%
    #   add_tooltip(histogram_tooltip, "hover") %>%
    #   bind_shiny("market_histogram", "market_histogram_slider")

    
    
    #================================ market_scatterplot plotly output
    output$market_scatterplot <- renderPlotly({
      
      if(is.null(resource.df()) | !is.data.table(resource.df())) return(NULL)
      color.map <- color.mapper(resource.df(), REP_COLOR_MAP, input$rep1,input$rep2,input$rep3, mode = "unique")
       
      ax <- list(
        title = "PRICE",
        titlefont= list(
          family = "Courier New, monospace",
          size = 17
        ),
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE
      )
      
      ay <- list(
        title = "Lenght Term",
        titlefont= list(
          family = "Courier New, monospace",
          size = 17
        ),
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = TRUE,
        showgrid = FALSE
        
      )
      
      plot_ly(data = copy(resource.df())[, REP:= factor(REP, levels = sort(unique(REP)))], 
              x = ~PRICE,
              y = ~TERM_LENGTH,
              mode = 'markers',
              type = 'scatter', 
              color = ~REP,
              colors = color.map,
              hoverinfo = 'text',
              text = ~paste0(
                '</br> Price: $', PRICE,
                '</br> Product: ', PRODUCT,
                '</br> Rate Type: ', RATE_TYPE,
                '</br> Term length: ', TERM_LENGTH,
                '</br> Retail Electric Provide (REP): ', REP,
                '</br> Transmission and Distribution Service Provider (TDU): ', TDU
                )
              )%>%
        layout(xaxis = ax, yaxis = ay, showlegend = FALSE)
    })
    
    # resource.df %>%
    #   ggvis(x=~PRICE, y=~TERM_LENGTH, size:=50, fill:=~REP_COLOR, key:=~ID) %>%
    #   layer_points() %>%
    #   add_axis("x", subdivide=4) %>%
    #   hide_legend("stroke") %>%
    #   add_tooltip(tooltip_helper, "hover") %>%
    #   bind_shiny("market_scatterplot")

    # rankings_plot ggvis output
    # fill:=~REP_COLOR
    # resource.df %>% 
    #     ggvis(x=~PRICE, y=~REP, size=~TERM_LENGTH, stroke="lightsteelblue", key:=~ID) %>% 
    #     layer_points() %>%
    #     add_axis("x", subdivide=4) %>%
    #     add_axis("y", title="") %>%
    #     hide_legend("stroke") %>%
    #     add_tooltip(tooltip_helper, "hover") %>%
    #     bind_shiny("rankings_plot")
    
    # market_histogram ggvis output
    # fill:=~REP_COLOR
    # resource.df %>% 
    #     ggvis(x=~PRICE, stroke="lightsteelblue") %>% 
    #     group_by(REP_COLOR) %>%
    #     layer_histograms(width=input_slider(label="Binwidth", min=0.1, max=2, value=0.2, step=0.1)) %>% 
    #     add_axis("x", subdivide=4) %>%
    #     hide_legend("stroke") %>%
    #     add_tooltip(histogram_tooltip, "hover") %>%
    #     bind_shiny("market_histogram", "market_histogram_slider")
    
    # market_scatterplot ggvis output
    # fill:=~REP_COLOR
    # resource.df %>% 
    #     ggvis(x=~PRICE, y=~TERM_LENGTH, size:=50, stroke="lightsteelblue", key:=~ID) %>% 
    #     layer_points() %>% 
    #     add_axis("x", subdivide=4) %>%
    #     hide_legend("stroke") %>%
    #     add_tooltip(tooltip_helper, "hover") %>%
    #     bind_shiny("market_scatterplot")

})