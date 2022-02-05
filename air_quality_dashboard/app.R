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
df_original <- df # save copy of df before cleaning
df$X <- as.character(df$X) # row index
df$start_date <- as.Date(df$start_date) # annual data, inconsistently begins in dec or jan
df$start_month <- format(df$start_date, "%m")
df$start_year <- as.numeric(format(df$start_date, "%Y"))
df$dec_flag <- as.numeric(df$start_month==12)
df$start_year <- df$start_year + df$dec_flag # years consistent now
df$name <- as.factor(df$name) # name of air quality indicator
df$geo_place_name <- as.factor(df$geo_place_name) # name of NYC neighborhood

# Create subsets for graphing line plots later 
pm <- subset(df, name == 'Fine Particulate Matter (PM2.5)') # subset by indicator
no2 <- subset(df, name == 'Nitrogen Dioxide (NO2)') 


# Define UI for application that plots air quality data
ui <- fluidPage(

    # Application title
    titlePanel("Air Quality by New York City Neighborhood"),

    # Sidebar with inputs for neighborhood, plot types, data table, and data download
    sidebarLayout(
        sidebarPanel(
            
            # Select neighborhood to highlight
            selectInput(inputId = "geo",
                        label = "Neighborhood:",
                        choices = c("Bedford Stuyvesant - Crown Heights",
                                    "Central Harlem - Morningside Heights", "Chelsea-Village",
                                    "Coney Island - Sheepshead Bay", "Downtown - Heights - Slope",
                                    "East Harlem", "Greenpoint", "Northeast Bronx", 
                                    "South Bronx", "Southeast Queens", "Southwest Queens",
                                    "Union Square-Lower Manhattan", "Upper East Side-Gramercy",
                                    "Upper West Side", "Washington Heights", 
                                    "West Queens", "Williamsburg - Bushwick"),
                        selected = "Upper West Side"),
            
            # Select plot type
            selectInput(inputId = "plots",
                               label = "Select plot type:",
                               choices = c("Line", "Bar", "Heat Map"),
                               selected = "Heat Map"),
           
           # Show data table 
           checkboxInput(inputId = "show_data",
                         label = "Show data table:",
                         value = TRUE),
           
           # Data download
            downloadButton(outputId = "downloadData", label = "Download")
        ),
        
        # Main panel with plot and data table output
        mainPanel(
            plotOutput("plots"),
            DT::dataTableOutput(outputId = "air_quality_table")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # Create a color scale that highlights the neighborhood selected by user (line plot)
    # https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
    colScale <- reactive({
        myColors <- c('#9C2E35', rep('#D3D3D3', 33)) # selected is red, all else gray
        names(myColors) <- c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        # https://stackoverflow.com/questions/48467884/remove-an-element-of-a-list-by-name]
        scale_colour_manual(name = "geo_place_name",values = myColors)
    })
    
    # Create sorted subsets of the data for the heat maps
    sorted_pm <- reactive({
        geo_order <- 
            c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        sorted_geo <- factor(df$geo_place_name, levels = geo_order)
        sorted_df <- cbind(df, sorted_geo)
        subset(sorted_df, name == 'Fine Particulate Matter (PM2.5)')
    })
    sorted_no2 <- reactive({
        geo_order <- 
            c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        sorted_geo <- factor(df$geo_place_name, levels = geo_order)
        sorted_df <- cbind(df, sorted_geo)
        subset(sorted_df, name == 'Nitrogen Dioxide (NO2)')
    })
    
    # Create sorted subsets of the data for bar charts
    sorted_pm_2018 <- reactive({
        geo_order <- 
            c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        sorted_geo <- factor(df$geo_place_name, levels = geo_order)
        sorted_df <- cbind(df, sorted_geo)
        sorted_pm <- subset(sorted_df, name == 'Fine Particulate Matter (PM2.5)')
        subset(sorted_pm, start_year == 2018)
    })
    sorted_no2_2018 <- reactive({
        geo_order <- 
            c(input$geo, purrr::discard(levels(df$geo_place_name),.p = ~stringr::str_detect(.x,input$geo)))
        sorted_geo <- factor(df$geo_place_name, levels = geo_order)
        sorted_df <- cbind(df, sorted_geo)
        sorted_no2 <- subset(sorted_df, name == 'Nitrogen Dioxide (NO2)')
        subset(sorted_no2, start_year == 2018)
    })
    
    # Draw the air quality plots and create data table
    output$plots <- renderPlot({
        
        # Line plots
        if(input$plots == 'Line') {
            
            # Particulate matter
            line_pm <- ggplot(data=pm, aes(x=start_date, y=data_value, 
                group=geo_place_name, color=geo_place_name)) +
                geom_line() +
                colScale() +
                theme(legend.position = 'none', axis.title.x = element_blank()) +
                ggtitle('Fine particulate matter') +
                ylab('Annual average, mcg per cubic meter')
            
            # Nitrogen dioxide
            line_no2 <- ggplot(data=no2, aes(x=start_date, y=data_value, 
                group=geo_place_name, color=geo_place_name)) +
                geom_line() +
                colScale() +
                theme(legend.position = 'none', axis.title.x = element_blank()) +
                ggtitle('Nitrogen dioxide') + 
                ylab('Annual average, ppb') 
            
            # Draw plots side-by-side
            grid.arrange(line_pm, line_no2, ncol = 2, bottom = 'Year')
        }
        
        # Heat Maps
        else if (input$plots=='Heat Map') {
            
            # Particulate matter
            heat_pm <- ggplot(sorted_pm(), aes(x=start_year, y=fct_rev(sorted_geo), 
                fill=data_value)) + 
                geom_tile() + 
                theme(legend.title = element_blank(), axis.title = element_blank()) +
                ggtitle('Fine particulate matter\n(Annual avg, mcg per cubic meter)') +
                scale_fill_gradient(low = '#fee0d2', high = '#de2d26', guide = 'colorbar')
            
            # Nitrogen dioxide
            heat_no2 <- ggplot(sorted_no2(), aes(x=start_year, y=fct_rev(sorted_geo), 
                fill=data_value)) + 
                geom_tile() + 
                theme(legend.title = element_blank(), axis.title = element_blank()) +
                ggtitle('Nitrogen dioxide\n(Annual avg, ppb)') + 
                scale_fill_gradient(low = '#fee0d2', high = '#de2d26', guide = 'colorbar')
            
            # Draw plots side-by-side
            grid.arrange(heat_pm, heat_no2, ncol = 2, bottom = 'Year')
        }
        
        # Bar plots
        else if (input$plots == 'Bar') {
            
            # Particulate matter
            bar_pm <- ggplot(sorted_pm_2018(), aes(x=fct_rev(sorted_geo), y=data_value)) + 
                geom_bar(stat='identity') +
                ggtitle('Fine particulate matter') +
                labs(y = 'Mcg per cubic meter, 2018', x = '') +
                coord_flip()
            
            # Nitrogen dioxide
            bar_no2 <- ggplot(sorted_no2_2018(), aes(x=fct_rev(sorted_geo), y=data_value)) + 
                geom_bar(stat='identity') +
                ggtitle('Nitrogen dioxide') +
                labs(y = 'Ppb, 2018', x = '') +
                coord_flip()
            
            # Draw plots side-by-side
            grid.arrange(bar_pm, bar_no2, ncol=2)
        }
    })
    
    # Print data table if checked
    output$air_quality_table <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = df_original, 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
    
    # Download button
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(filename) {
            write.csv(df_original, filename)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
