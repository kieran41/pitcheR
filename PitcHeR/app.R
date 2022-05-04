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
#library(pitcheR)


#theme_set(theme_minimal())
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

        selectInput("goption","Data Option",
                     choices = c("Upload a file","Use default"),
                     selected = "Use default"),
        #condition for filetype upload
        conditionalPanel(condition = "input.goption=='Upload a file'",
        fileInput("upload", "Select a .csv file",  buttonLabel = "Upload...", accept = ".csv", multiple = FALSE)),
        #condition for filetype usedefault
        #conditionalPanel(
            #conditin="input.goption=='Use default'",
         #selectInput("dplayer","Default Players",
                     #choices = c("wheeler","deGrom")),

         selectInput("heatm","Choose preferred Visualization",
                     choices=c("Location Heatmap","Contact Heatmap","Custom Heatmap",
                               "Velo Time","Velo BP","Break Func"),
                     selected = "Location Heatmap"

         ),
         radioButtons("table", "Request Table",
                      choices = "Swg strike"

         )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
           tabPanel("Heatmaps",plotOutput("heatedm")
                    ),
           tabPanel("Table",tableOutput("ptable")
           )
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



    if (input$goption=="Use default"){
        if(input$heatm=="Location Heatmap"){
            location_heatmap(file = wheeler)
        }
        else if(input$heatm=="Contact Heatmap"){
            contact_location_heatmap(file = deGrom)
        }
        else if(input$heatm=="Velo Time"){
            velo_time(file = deGrom)
        }
        else if(input$heatm=="Velo BP"){
            velo_bp(file = wheeler)
        }
        else if(input$heatm=="Break Func"){
            break_func(file = deGrom)
        }
        else {custom_heatmap(file = wheeler)}
    }
    else {
        inFile<- input$upload
        if(is.null(inFile))
            return("Please upload a .csv file")
        newfile<- read.csv(inFile$datapath, header=T)
        names(newfile)[1]<- "pitch_type"

        if(input$heatm=="Location Heatmap"){
        location_heatmap(file = newfile)
    }
        else if(input$heatm=="Contact Heatmap"){
            contact_location_heatmap(file = newfile)
        }
        else if(input$heatm=="Velo Time"){
            velo_time(file = newfile)
        }
        else if(input$heatm=="Velo BP"){
            velo_bp(file = newfile)
    }
        else if(input$heatm=="Break Func"){
            break_func(file = newfile)
    }
     else {custom_heatmap(file = newfile)}
        }


}

)
output$ptable<- renderTable({
    if(input$goption=="Use default"){
    swg_strike(file= wheeler)}
    else{
        inFile<- input$upload
        if(is.null(inFile))
            return("Please upload a .csv file")
        newfile<- read.csv(inFile$datapath, header=T)
        names(newfile)[1]<- "pitch_type"
        swg_strike(file=newfile)
    }
})
}

# Run the application
shinyApp(ui = ui, server = server)
