#
# This is a Shiny web application for visualizing air quality indicators by New
# York City neighborhood. You can run the application by clicking the 'Run App' 
# button above.
#

# Set up
library(shiny)
library(tidyverse)

# Load and clean air quality data
# See air_quality_dashboard/data subfolder for more information on the data 
# source 
df <- read.csv(file = 'data/air_quality.csv')
df$X <- as.character(df$X) # row index
df$start_date <- as.Date(df$start_date) # annual data 
df$name <- as.factor(df$name) # name of air quality indicator
df$geo_place_name <- as.factor(df$geo_place_name) # name of NYC neighborhood

# Define UI for application that plots air quality data
ui <- fluidPage(

    # Application title
    titlePanel("NYC Air Quality Data by Neighborhood"),

    # Sidebar with inputs for neighborhood and plot titles
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "geo",
                        label = "Neighborhood:",
                        #choices = as.vector(levels(df$geo_place_name)),
                        choices = c("West Queens", "Upper West Side", "Rockaways"),
                        selected = "Upper West Side")
        ),

        # Show a plots of the fine paticulate matter and nitrogen dioxide 
        # indicators by neighborhood
        mainPanel(
           plotOutput("pmPlot"),
           plotOutput("no2Plot")
        )
    )
)

# Define server logic required to draw air quality plots
server <- function(input, output) {
    
    # Create a color scale that highlights the neighborhood selected by user
    # https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
    colScale <- reactive({
        myColors <- c('#9C2E35', rep('#D3D3D3', 33)) # selected is red, all else gray
        names(myColors) <- c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        # https://stackoverflow.com/questions/48467884/remove-an-element-of-a-list-by-name]
        scale_colour_manual(name = "geo_place_name",values = myColors)
    })
    
    # Draw the air quality plots
    output$pmPlot <- renderPlot({
        # Subset the particulate matter data
        pm <- subset(df, name == 'Fine Particulate Matter (PM2.5)')
        
        # Draw line plot of particulate matter
        ggplot(data=pm, aes(x=start_date, y=data_value, group=geo_place_name, color=geo_place_name)) +
            geom_line()+
            geom_point()+
            colScale()+
            theme(legend.position = 'none')
    })
    
    output$no2Plot <- renderPlot({
        
        # Subset the nitrogen dioxide data
        no2 <- subset(df, name == 'Nitrogen Dioxide (NO2)')
        
        # Draw line plot of nitrogen dioxide
        ggplot(data=no2, aes(x=start_date, y=data_value, group=geo_place_name, color=geo_place_name)) +
            geom_line()+
            geom_point()+
            colScale()+
            theme(legend.position = 'none')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
