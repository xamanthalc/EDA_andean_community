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
data_uni_d <- read_csv("Data sets made in process/final")
data_uni_d <- data_uni_d %>%
    select(sex, race_id, urban_rural) %>%
    #recoding race id
    mutate(race_id = if_else(race_id == "Other", 
                             "No answer", 
                             race_id))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Univariate Demographic Plots: Categorical Variables"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right",
        sidebarPanel(
            selectInput(inputId = "variable", 
                        label = "Select Variable:",
                        choices = list("Race" = "race_id",
                                       "Gender" = "sex",
                                       "Urban Status" = "urban_rural"),
                        selected = "sex"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myplot")
        )
    )
)

#matching variables 
variables <- c("Race" = "race_id",
               "Gender" = "sex",
               "Urban Status" = "urban_rural")


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$myplot <- renderPlot({
        
        #getting x mapping 
        the_var <- data_uni_d[[input$variable]]
        variable_name <- names(variables)[variables == input$variable]
        
        #making interactive bar chart
        ggplot(data = data_uni_d, 
               mapping = aes(x = the_var, 
                             #fill according to input
                             fill = the_var)) +
            geom_bar(position = position_dodge(), 
                     color = "black") +
            #personalizing colors
            scale_fill_manual(values = c("#9E9E9E", "#973539", 
                                         "#65A59D", "#613D76", 
                                         "white","#1E4367")) +
            labs(y = "Number of Observation in the Data Set",
                 #x axis dependant on input
                 x = paste(variable_name, "of observations"), 
                 fill = variable_name, 
                 #title dependant on input
                 title = paste(variable_name, "in the Data Set (2006 - 2014)"), 
                 subtitle = "Aggregate data of all years") +
            theme(plot.title = element_text(face = "bold"), 
                  plot.subtitle = element_text(face = "bold"), 
                  legend.position = "bottom") +
            coord_flip()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
