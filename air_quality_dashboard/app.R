#
# This is a Shiny web application for visualizing air quality indicators by New
# York City neighborhood. You can run the application by clicking the 'Run App' 
# button above.
#

# Set up
library(shiny)
library(tidyverse)
library(gridExtra)

# Load and clean air quality data
# See air_quality_dashboard/data subfolder for more information on the data 
# source 
df <- read.csv(file = 'data/air_quality.csv')
df$X <- as.character(df$X) # row index
df$start_date <- as.Date(df$start_date) # annual data, inconsistently begins in dec or jan
df$start_month <- format(df$start_date, "%m")
df$start_year <- as.numeric(format(df$start_date, "%Y"))
df$dec_flag <- as.numeric(df$start_month==12)
df$start_year <- df$start_year + df$dec_flag # years consistent now
df$name <- as.factor(df$name) # name of air quality indicator
df$geo_place_name <- as.factor(df$geo_place_name) # name of NYC neighborhood

# Create subsets for graphing later 
pm <- subset(df, name == 'Fine Particulate Matter (PM2.5)') # subset by indicator
no2 <- subset(df, name == 'Nitrogen Dioxide (NO2)') 
pm_2018 <- subset(pm, start_year == 2018) # subset by year 
no2_2018 <- subset(no2, start_year == 2018)


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
                        selected = "Upper West Side"),
            
           checkboxGroupInput(inputId = "plots",
                               label = "Select plot type:",
                               choices = c("Line", "Bar", "Heat Map"),
                               selected = "Heat Map"),
           
           # Show data table ---------------------------------------------
           checkboxInput(inputId = "show_data",
                         label = "Show data table:",
                         value = TRUE)
        ),

        # Show a plots of the fine particulate matter and nitrogen dioxide 
        # indicators by neighborhood
        mainPanel(
           plotOutput("line"),
           plotOutput("heat"),
           plotOutput("bar"),
           
           # Show data table ---------------------------------------------
           DT::dataTableOutput(outputId = "air_quality_table")
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
    
    # Make a list of the plot types selected by user
    #plotList <- reactive({input$plots})
    
    # Draw the air quality plots

    output$line <- renderPlot({
        
        if('Line' %in% input$plots) {
        
            # Draw line plot of particulate matter
            line_pm <- ggplot(data=pm, aes(x=start_date, y=data_value, group=geo_place_name, color=geo_place_name)) +
                geom_line()+
                geom_point()+
                colScale()+
                theme(legend.position = 'none')
            
            # Draw line plot of nitrogen dioxide
            line_no2 <- ggplot(data=no2, aes(x=start_date, y=data_value, group=geo_place_name, color=geo_place_name)) +
                geom_line()+
                geom_point()+
                colScale()+
                theme(legend.position = 'none')
            
            grid.arrange(line_pm, line_no2, ncol=2)
        }
    })
    
    output$heat <- renderPlot({
        
        if('Heat Map' %in% input$plots) {
            # Draw heat map of particulate matter
            heat_pm <- ggplot(pm, aes(x=start_year, y=geo_place_name, fill=data_value)) + geom_tile()
            
            # Draw heat map of nitrogen dioxide
            heat_no2 <- ggplot(no2, aes(x=start_year, y=geo_place_name, fill=data_value)) + geom_tile()
            
            grid.arrange(heat_pm, heat_no2, ncol=2)
        }
    })
    
    output$bar <- renderPlot({
        
        if('Bar' %in% input$plots) {
            #Draw bar plot for particulate matter in 2018
            bar_pm <- ggplot(pm_2018, aes(x=geo_place_name, y=data_value)) + geom_bar(stat='identity') +
                coord_flip()
            
            #Draw bar plot for nitrogen dioxide in 2018
            bar_no2 <- ggplot(no2_2018, aes(x=geo_place_name, y=data_value)) + geom_bar(stat='identity') +
                coord_flip()
            
            grid.arrange(bar_pm, bar_no2, ncol=2)
        }
    })
    
    # Print data table if checked -------------------------------------
    output$air_quality_table <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = df[, 1:7], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
