# Shiny Dashboard App

# Potential data sources:
# github, cryptocurrency, f1 racing, SFMoMA, NYPL Menus
# https://www.rstudio.com/resources/webinars/dynamic-dashboards-with-shiny/

# 12 rows that can be divided as needed ... fluidrow(column(width = 12))
# adding column value provdes a margin

# infoBox() for small amounts of info
# valueBox() for small amounts of info
library('shinydashboard')
library('shiny')
source("coin_data.r")

shinyApp(ui, server) #preview dashboard

ui <- dashboardPage(
        dashboardHeader(title = "Randy Avalos"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem(text = "Github", 
                                 tabName = "github", 
                                 icon = icon("github"),
                                 badgeLabel = "Commit History",
                                 badgeColor = "teal"
                                 ),
                        menuItem(text = "Coins",
                                 tabName = "coins",
                                 icon = icon("dashboard")
                                 ),
                        menuItem(text = "Fantasy Football",
                                 tabName = "football",
                                 icon = icon("dashboard")
                                 )
                        )
                ),
        
        dashboardBody(
                tabItems(
                # Github tab
                        tabItem(tabName = "github",
                                fluidRow(box(plotOutput("testPlot", 
                                                        height = 250)),
                                box(plotOutput("testPlot2", height = 250)),
                                box(title = "tesPlot2 Controls",
                                    sliderInput("slider2", "Count", 1, 500, 100)
                                    ),
                                box(title = "testPlot Controls",
                                    sliderInput("slider", "Obs Count:", 1, 100, 50)
                                    )    
                                 
                                 )
                        ),
                        #Coin Tab
                        tabItem(tabName = "coins",
                                h2("Test"),
                                fluidRow(
                                        box(ggvisOutput("eth_price_history")),
                                         fluidRow(infoBox("Ethereum", 
                                                          paste0("$", 
                                                                 prettyNum(content(ethereum_price)$data$amount, big.mark = ",")
                                                                 )
                                                          ),
                                        infoBox("Bitcoin", 
                                                paste0("$", prettyNum(content(bitcoin_price)$data$amount, big.mark = ",")
                                                       )
                                                )
                                        )
                                        )
                                ),
                        # Football Tab
                        tabItem(tabName = "football",
                                h2("Fantasy Football")
                        )
                )
        )
)


server <- function(input,output){
        set.seed(42)
        histdata <- rnorm(500)
        histdata2 <- rnorm(1000)
        
        output$testPlot <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
                })
        output$testPlot2 <- renderPlot({
                        data <- histdata2[seq_len(input$slider2)]
                        hist(data)
                        })
        
        output$eth_pricePlot <- renderPlot({
                plot_eth_price_hist <- ggplot(eth_price_history, aes(x = time, y = close)) +
                        geom_hline(yintercept = eth_price_history$close[1], 
                                   color = "light grey") +
                        geom_line() +
                        ggtitle("Ethereum Price History") +
                        ylab("Closing Price USD") +
                        xlab("") +
                        theme_tufte()
        })
        

        eth_price_history %>%
                ggvis(x = ~time, y = ~close, 
                      fill = ~volume, 
                      opacity := 0.5) %>%
                layer_points() %>%
                bind_shiny("eth_price_history") #closing price plot
        
}

