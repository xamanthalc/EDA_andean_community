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
library(readr)

#loading data set
data_uni_bt <- read_csv("Data sets made in process/final")
data_uni_bt <- data_uni_bt %>%
    #make NA values = "No answer"
    mutate(economy_compared_12 = if_else(is.na(economy_compared_12), 
                                         "No answer", 
                                         economy_compared_12))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Univariate Main Topic Plots"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right",
        sidebarPanel(
            selectInput(inputId = "variable", 
                        label = "Select Variable:",
                        choices = list("Economy Perception" = "economy_compared_12",
                                       "Satisfaction with Health Services" = "satisf_health_services", 
                                       "Satisfaction with Education" = "satisf_public_schools"),
                        selected = "satisf_public_schools"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myplot")
        )
    )
)

#matching variables 
variables <- c("Main Problem" = "country_main_problem",
               "Economy Perception" = "economy_compared_12",
               "Satisfaction with Health Services" = "satisf_health_services", 
               "Satisfaction with Education" = "satisf_public_schools")


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$myplot <- renderPlot({
        #getting x mapping 
        the_var <- data_uni_bt[[input$variable]]
        variable_name <- names(variables)[variables == input$variable]
        
        #plot
        ggplot(data = data_uni_bt, 
               mapping = aes(x = the_var, 
                             fill = the_var)) +
            #making bar chart
            geom_bar(position = position_dodge(), 
                     color = "black", 
                     show.legend = FALSE, 
                     #omiting NAs
                     na.rm = TRUE) +
            #personalizing colors
            scale_fill_manual(values = c("#9E9E9E", "#973539", 
                                         "#65A59D", "#613D76", 
                                         "#1E4367")) +
            labs(y = "Number of Observation in the Data Set",
                 #x axis text depends on input
                 x = paste(variable_name, "of Observations"), 
                 #title text depends on input
                 title = paste(variable_name, "in the Data Set (2006 - 2014)"), 
                 subtitle = "Aggregate data of all years") +
            theme(plot.title = element_text(face = "bold"), 
                  plot.subtitle = element_text(face = "bold"), 
                  legend.position = "bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
