#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(readr)


setwd("/srv/shiny-server/faraon/datos/")


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
  titlePanel(
    h1("Pharaoh BRC-20", align = "center"),
    
  ),
  
  
  fluidRow(
  column(2, textOutput("uno")),
  column(8, dataTableOutput("coronas")),
  column(2, textOutput("dos"))
  ),
  
  
  
  #textOutput("coronas"),
    
  br(),
  br(),
  br(),
  br(),
  
  tabsetPanel(
    # Sidebar with a slider input for number of bins
      
       tabPanel("Coins",
                
                fluidRow(
                  
                  
                  column(3,
                         br(),
                         br(),
                         br(),
                         br(),
                         sliderInput("sales", "Select range of Sales", 0, 0.5, 0.01, value = c(0, 0.1)),
                         #sliderInput("change_sales", "Change in Sales", 0, 1000000, 10000, value = c(0, 1000000)),
                         sliderInput("holders", "Holders", 0, 1000, 1, value = c(0, 1000)),
                         #sliderInput("change_holders", "Change in Holders", 0, 100, 1, value = c(0, 100)),
                         sliderInput("minted", "Minted (%)", 0, 100, 1, value = c(0, 100))
                         ),
                  column(9,
                         br(),
                         br(),
                         dataTableOutput("tabla"))
                )
        
        
        
               ),
      
       tabPanel("Wallet Scanner",
                textInput("adresse", "Insert Adresse"),
                actionButton("Analyze", "Analyze"),
               dataTableOutput("wallet")
               ),
               
       tabPanel("Collections",
               dataTableOutput("collection1"),
               dataTableOutput("collection2")
                ),
               
               
      )
      
))
