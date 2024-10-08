## IBM course "Data Visualization with R"
## On Coursera: https://www.coursera.org/learn/data-visualization-r
## Final project
## Visualizing UCI Adult data set
## https://archive.ics.uci.edu/dataset/2/adult
## Instructor set up the basic structure and provide instruction
## for completing the assignment.

## ui.R

# Load libraries
library(shiny)
library(tidyverse)

# Application Layout
shinyUI(
    fluidPage(
        br(),
        # TASK 1: Application title
        titlePanel("Trends in Demographics and Income"),
        p("Explore the difference between people who earn less than 50K and more than 50K. You can filter the data by country, then explore various demogrphic information."),
        
        # TASK 2: Add first fluidRow to select input for country
        fluidRow(
            column(12, 
                   wellPanel(
                       selectInput("country", 
                                   "Select a country:",
                                   choices = c("Cambodia", "China", "England", "France", "Germany", "Hungary", "India", "Jamaica", "Mexico", "Peru", "Philippines", "Poland", "Laos", "United-States")),
                   )
            )
        ),
        
        # TASK 3: Add second fluidRow to control how to plot the continuous variables
        fluidRow(
            column(3, 
                   wellPanel(
                       p("Select a continuous variable and graph type (histogram or boxplot) to view on the right."),
                       # add radio buttons for continuous variables
                       radioButtons("continuous_variable",
                                    "Continuous:",
                                    choices = c("age", "hours_per_week")),
                       # add radio buttons for chart type
                       radioButtons("graph_type",
                                    "Graph:",
                                    choices = c("histogram", "boxplot")),
                   )
            ),
            # add plot output
            column(9, 
                   plotOutput("p1")) 
        ),
        
        # TASK 4: Add third fluidRow to control how to plot the categorical variables
        fluidRow(
            column(3, 
                   wellPanel(
                       p("Select a categorical variable to view bar chart on the right. Use the check box to view a stacked bar chart to combine the income levels into one graph. "),
                       radioButtons("categorical_variable",
                                    "Categorical:",
                                    choices = c("education", "workclass", "sex")),
                       checkboxInput("is_stacked",
                                     "Stacked bars",
                                     value = FALSE)
                   )
            ),
            column(9,
                   plotOutput("p2"))
        )
    )
)
