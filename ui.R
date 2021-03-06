shinyUI(fluidPage(
  tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="https://fonts.googleapis.com/css?family=Architects+Daughter|Ubuntu")),
    
    titlePanel("Power-To-Choose Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            p(class="text-small",
              a(href="https://ahmedtadde.github.io/DataQuest", target="_blank", "by Ahmed Tadde"),
              a(href="https://github.com/ahmedtadde/electricityViz", target="_blank", icon("github")), " | ",
              a(href="https://www.linkedin.com/in/ahmedtadde", target="_blank", icon("linkedin"))),
            hr(),
            p(class="text-small", "Data visualizations on market rankings of electricity products and prices in the ERCOT electricity market.  All data is derived from the actual PUC website: ",
              a(href="http://www.powertochoose.org", target="_blank", "www.powertochoose.org")),
            hr(),
            
            selectInput(inputId="tdu", label="Select TDU:", choices=choices$tdus, selected=choices$tdus[[1]]),
            selectInput(inputId="usage", label=" Select Usage:", choices=choices$usage, selected=choices$usage[[2]]),
            selectInput(inputId="rate_type", label="Choose Rate Types:", choices= choices$rate_types, selected=choices$rate_types[[1]]),
            hr(),
            
            selectInput(inputId="rep1", label="REP 1:", choices=choices$reps, selected=choices$reps[[1]]),
            selectInput(inputId="rep2", label="REP 2:", choices=choices$reps, selected=choices$reps[[2]]),
            selectInput(inputId="rep3", label="REP 3:", choices=choices$reps, selected=choices$reps[[3]]),
            p(class="text-small", "(REP: Retail Electricity Provider)"),
            hr(),
            
            selectInput(inputId="prepaid", label="Choose Prepaid:", choices=choices$booleans, selected="ALL"),
            selectInput(inputId="tou", label="Choose Time-of-Use:", choices=choices$booleans, selected="ALL"),
            selectInput(inputId="promotion", label="Choose Promotion:", choices=choices$booleans, selected="ALL"),
            sliderInput(inputId="term_lengths", label="Filter Term Length:", min=0, max=36, value=c(0, 36)),
            sliderInput(inputId="renewables", label="Filter Renewable:", min=0, max=100, value=c(0, 100)),
            hr(),
            width=3
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Rankings",
                     h2("Rankings"),
                     p(class="text-small-italic", "At times, a programming bug will stop 
                        the App's processing and the screen turns grey. This mean your inputs did not match any data records.
                        The weak binding logic between Shiny and the visualization package GGVIS means that 
                        the former isn't able to handle such a case (at this time).Simply refresh the page to continue using the app."
                      ),
                     hr(),
                     p(class="text-small", "This section visualizes the rankings of Retail Electric Providers in ERCOT,
                           providing details on the specific energy plans and prices against the market.  
                           A summary section is provided to help determine quick insights on the key observations in the market."),
                     hr(),
                     
                     h3("Summary"),
                     htmlOutput("rankingSummary"),
                     hr(),
                     
                     h3("Rankings Plot"),
                     p(class="text-small", "Rankings of products at a given price range and associated variables. "),
                     ggvisOutput("rankings_plot"),
                     hr()
            ),
            tabPanel("Market",
                     h2("Market"),
                     p(class="text-small-italic", "At times, a programming bug will stop 
                       the App's processing and the screen turns grey. This mean your inputs did match any data records.
                       The weak binding logic between Shiny and the visualization package GGVIS means that 
                       the former isn't able to handle such a case (at this time).Simply refresh the page to continue using the app."
                     ),
                     p(class="text-small", "This section zooms in on the aggregate visualization of the given market (histogram and scatterplot).  
                           Interact with the selection widgets to redefine market definitions and conditions."),
                     hr(),
                     
                     h3("Market Histogram"),
                     p(class="text-small", "Histogram of products at a given price range and binwidth, highlighting selected REPs in the market."),
                     ggvisOutput("market_histogram"),
                     div(class="row offset1", uiOutput("market_histogram_slider")),
                     hr(),
                     
                     h3("Market scatterplot"),
                     p(class="text-small", "Scatterplot of products at a given price range, highlighting selected REPs in the market."),
                     ggvisOutput("market_scatterplot"),
                     hr()
            ),
            tabPanel("Data",
                     h2("Data"),
                     p(class="text-small-italic", "At times, a programming bug will stop 
                       the App's processing and the screen turns grey. This mean your inputs did match any data records.
                       The weak binding logic between Shiny and the visualization package GGVIS means that 
                       the former isn't able to handle such a case (at this time).Simply refresh the page to continue using the app."
                     ),
                     p(class="text-small", "This section provides a search datatable similar to that found on",
                       a(href="http://www.powertochoose.org", target="_blank", "www.powertochoose.org")),
                     hr(),
                     
                     h3("Datatable"),
                     dataTableOutput("datatable"),
                     hr()
            ),
            
            tabPanel("About",
                     fluidRow(includeMarkdown("README.md"))
            )
          ),
        width=9
      )
    )
  )
)
