## IBM course "Data Visualization with R"
## On Coursera: https://www.coursera.org/learn/data-visualization-r
## Final project
## Visualizing UCI Adult data set
## https://archive.ics.uci.edu/dataset/2/adult
## Instructor set up the basic structure and provide instruction
## for completing the assignment.
 
## server.R

# Load libraries
library(shiny)
library(tidyverse)

# Read in data
adult <- read_csv("UCI_adult_dataset")

# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
    df_country <- reactive({
        req(input$country)
        filter(adult, native_country == input$country)
    })
    
    # Graph theme for continuous variables
    my_theme = theme(plot.title = element_text(size = 20, face = "bold"),
                     axis.text = element_text(size=14, face = "bold"),
                     axis.title = element_text(size=16, face = "bold"),
                     strip.text = element_text(size = 12, face = "bold"))
    
    # Task 5. Create logic to plot histogram or boxplot
    output$p1 <- renderPlot({
        if (input$graph_type == "histogram") {
            # Histogram
            ggplot(df_country(), aes_string(x = input$continuous_variable)) +
                geom_histogram(color="#0072B2", fill = "#56B4E9") +
                labs(x = input$continuous_variable,
                     y = "Number of people",
                     title = paste("Trend of ", input$continuous_variable)) +
                facet_wrap(~prediction) + my_theme
        } else {
            # Boxplot
            ggplot(df_country(), aes_string(y = input$continuous_variable)) +
                geom_boxplot() + 
                coord_flip() + 
                labs(x = "Number of people",
                     y = input$continuous_variable,
                     title = paste("Trend of ", input$continuous_variable)) +
                facet_wrap(~prediction) + my_theme
        }
    })
    
    # Task 6. Create logic to plot faceted bar chart or stacked bar chart
    output$p2 <- renderPlot({
        # Bar chart
        p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
            labs(x = input$categorical_variable,
                 y = "Number of people",
                 title = paste("Trend of ", input$categorical_variable)) + 
            theme(plot.title = element_text(size = 20, face = "bold"), 
                  axis.text.x = element_text(angle = 45, size = 14, face = "bold", hjust = 1),
                  axis.text.y = element_text(size = 14, face = "bold"),
                  axis.title = element_text(size=16, face = "bold"),
                  legend.text = element_text(size = 14, face = "bold"),
                  legend.position = "bottom",
                  legend.title = element_text(size = 16, face = "bold"))
        if (input$is_stacked) {
            p + geom_bar(aes(fill = prediction))
        } else {
            p + geom_bar(aes_string(fill = input$categorical_variable)) + 
                theme(strip.text = element_text(size = 12, face = "bold")) +  
                facet_wrap(~prediction)
        }
    })
})
