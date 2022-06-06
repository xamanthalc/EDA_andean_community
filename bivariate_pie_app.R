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

#importing data set
data_biv_crime <- read_csv("Data sets made in process/final")

#wrangling data
data_biv_crime <- data_biv_crime %>%
    filter(victim_crime_12 == "Yes") %>%
    mutate(race_id = if_else(race_id == "Other", 
                             "No answer", 
                             race_id)) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bivariate Pie Chart"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right",
        sidebarPanel(
            selectInput(inputId = "variable_3", 
                        label = "Select Variable:",
                        choices = list("Race" = "race_id",
                                       "Country" = "country",
                                       "Year" = "year", 
                                       "Gender" = "sex", 
                                       "Military Takeover to Combat Crime" = "military_takeover_crime",
                                       "Trust Justice with Criminal Punishment" = "trust_judicial_punishment", 
                                       "Trust National Police" = "trust_national_police", 
                                       "Administration Imposes Safety" = "administration_imp_safety",
                                       "AAFF should Combate Crime" = "aaff_should_combate_crime"),
                        selected = "country"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myplot_3")
        )
    )
)

#matching variables 
variables_3 <- c("Race" = "race_id",
                 "Country" = "country",
                 "Year" = "year", 
                 "Gender" = "sex", 
                 "Military Takeover to Combat Crime" = "military_takeover_crime",
                 "Trust Justice with Criminal Punishment" = "trust_judicial_punishment", 
                 "Trust National Police" = "trust_national_police", 
                 "Administration Imposes Safety" = "administration_imp_safety",
                 "AAFF should Combate Crime" = "aaff_should_combate_crime")


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$myplot_3 <- renderPlot({
        #getting x mapping 
        the_var_3 <- data_biv_crime[[input$variable_3]]
        variable_name_3 <- names(variables_3)[variables_3 == input$variable_3]
        
        #plot
        ggplot(mapping = aes(x = "x", 
                             #fill depends on user input
                             fill = as.factor(the_var_3))) +
            geom_bar() +
            #pie chart conversion
            coord_polar("y") +
            #personalizing colors
            scale_fill_manual(values = c("#9E9E9E", "#973539", 
                                         "#65A59D", "#613D76", 
                                         "white","#1E4367", 
                                         "#B3782b")) +
            labs(fill = variable_name_3, 
                 title = "Breakdown of People who were Victims of Crime during the Past Year", 
                 subtitle = "Aggregate data of all years") +
            theme(plot.title = element_text(face = "bold"), 
                  plot.subtitle = element_text(face = "bold"), 
                  axis.title = element_blank(), 
                  axis.text = element_blank())
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
