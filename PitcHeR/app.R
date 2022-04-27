#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggalt)
library(dplyr)
library(baseballr)
library(reshape2)
library(tidyverse)
library(shinythemes)
library(bslib)
library(shiny)
library(pitcheR)

theme_set(theme_minimal())
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    # Application title
    headerPanel(

        title = h1(tags$img(src='pitcHeR.png', height=50,width=50),"Baseball PitcHeR"
        )),
    p("This app gives Pitch Stats of Baseball Players")
    ,


    # Application title
    #titlePanel("Pitch of Baseball Players"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
         selectInput("heatm","Choose preferred Heatmap",
                     choices=c("Location Heatmap","Contact Heatmap","Custom Heatmap"),
                     selected = "Location Heatmap"

         ),
         radioButtons("table", "Request Table",
                      choices = "Activate"

         )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
           tabPanel("Heatmaps",plotOutput("heatedm")),
           tabPanel("Table",verbatimTextOutput("ptable"))
        )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output){
output$heatedm<- renderPlot({
    load(file='deGrom.rda')
    load(file='wheeler.rda')
    names(deGrom)[1] <- "pitch_type"
    names(wheeler)[1] <- "pitch_type"

    if(input$heatm=="Location Heatmap"){
        location_heatmap(file = wheeler)
    }
        else if(input$heatm=="Contact Heatmap"){
            contact_location_heatmap(file = wheeler)
        }
            else (custom_heatmap(file = deGrom))

    #output$ptable<- renderPrint({
        #if (input$table=="Activate"){
            #kableExtra::kable(swg_strike(file= wheeler))}
    #})
}

)

}

# Run the application
shinyApp(ui = ui, server = server)
