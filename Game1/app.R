#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(httr)
library(tidyverse)
library(shiny)
library(jsonlite)
library(imager)


r1 <- GET("https://www.amiiboapi.com/api/amiibo")
#This is an  Amiibo database that holds all amiibo information in a single API.
stop_for_status(r1)
json1<- content(r1, as = "text", encoding = "UTF-8")
a1<- fromJSON(json1, flatten = TRUE) %>% as.data.frame() 
# %>% filter(amiibo.character == ?...)
#Add this If you want save time and get the specific character

id_tail <- a1 %>% select(amiibo.tail)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Nintendo Amiibo: A new world of fantasy"),

    # Sidebar with a Input
    sidebarLayout(
        sidebarPanel(
            selectInput("tail", "ID_tail", id_tail)
        ),

        # Show a picture of this amiibo and some other information
        mainPanel(
           plotOutput("Picture1"),
           textOutput("character1"),
           textOutput("gameseries1"),
           textOutput("name1"),
           textOutput("type1"),
           tableOutput("release1"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    amiibo1 <- reactive({
        a1 %>% filter(amiibo.tail == input$tail)
    })
    output$Picture1 <- renderPlot({
        # Get a image from website
        title1 = paste("Image for", amiibo1()$amiibo.character)
        plot(load.image(amiibo1()$amiibo.image),
                        main = title1,
             axes = FALSE)
    })
    
    output$character1 <- renderText({
        
        paste("This character is:", amiibo1()$amiibo.character,".")
    })
    
    output$gameseries1 <- renderText({
       
        paste("He/She is from Gameseries:",amiibo1() $amiibo.gameSeries,".")
    })
    
    output$name1 <- renderText({
      
        paste("This amiibo's name is ",amiibo1() $amiibo.name)
    })
    
    output$type1 <- renderText({
      
        paste("and it is a ",amiibo1()$amiibo.type,". Let's take a look of its release data in different areas.")
    })
    
    output$release1 <- renderTable({
        
        amiibo1() %>% select(amiibo.release.au,amiibo.release.eu,amiibo.release.jp,amiibo.release.na)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

