#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
df <- read.csv(file = 'data/air_quality.csv')
df$X <- as.character(df$X)
df$start_date <- as.Date(df$start_date)
df$name <- as.factor(df$name)
df$geo_place_name <- as.factor(df$geo_place_name)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Air Quality Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "geo",
                        label = "Neighborhood:",
                        #choices = as.vector(levels(df$geo_place_name)),
                        choices = c("West Queens", "Upper West Side", "Rockaways"),
                        selected = "Upper West Side")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        myColors <- c('#9C2E35', rep('#D3D3D3', 33))
        print(myColors)
        names(myColors) <- c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        print(myColors)
        colScale <- scale_colour_manual(name = "geo_place_name",values = myColors)
        
        pm <- subset(df, name == 'Fine Particulate Matter (PM2.5)')
        
        # Line plot of particulate matter
        ggplot(data=pm, aes(x=start_date, y=data_value, group=geo_place_name, color=geo_place_name)) +
            geom_line()+
            geom_point()+
            colScale+
            theme(legend.position = 'none')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
