library(shiny)
library(shinythemes)
library(leaflet)
library(owmr)
library(ggplot2)
library(gganimate)
library(plyr)
library(dplyr)
library(yarrr)
library(ggmap)
shinyUI(fluidPage(theme = shinytheme('cerulean'),
                  
                  navbarPage(  title = "Indian Terrain Climate",  
                               id= "nav",
                               tabPanel("Climate", value ="Climate",
                                        ## First Row
                                        
                                        fluidRow(style="background-color:white;",
                                                 column(6,column(12,style="background-color:#239eba;",
                                                                 helpText(h2("Current Weather",style="color:white;")),
                                                                 div(style = "height:50px;")),
                                                        helpText(h5("Data Source:National Oceanic and Atmospheric Administration(US)
                                                                    & https://www.openweathermap.org")),
                                                        
                                                        leafletOutput("map"),
                                                       div(style = "height:20px;"))
                                                          , 
                                                          column(6, column(12,style="background-color:#239eba;",
                                                                           helpText(h4("Historical Data",style="color:white;")),
                                                                           
                                                                 column(5,selectInput(inputId="station", label = "Weather Station", 
                                                                                      choices = c("Leh",
                                                                                                  "Bangalore",
                                                                                                  "Gangtok",
                                                                                                  "Dehradun",
                                                                                                  "Kolkata",
                                                                                                  "Bikaner",
                                                                                                  "Srinagar"),
                                                                                      selected='Gangtok')),
                                                                 column(4,textInput(inputId="color",value = "#b6e5af",
                                                                                    label="Color Code"))),
                                                                 imageOutput("plot1"),div(style = "height:20px;"))),
                                        
                                        ## Next Row
                                        
                                                 fluidRow(style="background-color:white;",
                                                          column(6,
                                                                 column(12,style="background-color:#239eba;",
                                                                 helpText(h4("Historical Rainfall Data for past 115 years",style="color:white;")),
                                                                 column(3,radioButtons(inputId="Rainfall",label="Rainfall",
                                                                              choices = c("pre 1950","post 1950"),
                                                                              selected="post 1950")),
                                                                 column(3,selectInput(inputId="Season",label="Season",
                                                                             choices=c("Winter","Summer","Monsoon","Fall")))),
                                                        plotOutput("yarrplot1"), div(style = "height:600px;")
                                                                 
                                                 ),
                                                 column(6,
                                                        
                                                        column(12,style="background-color:#239eba;",
                                                               helpText(h4("Surface Temperature for past 4 years",style="color:white;")),
                                                               column(6, selectInput(inputId="Temperature",label="Average Temperature",
                                                                                     choices=c("Above Mean Temp of 4 years", "Below Mean Temp of 4 years"))),
                                                               div(style = "height:50px;")),
                                                               
                                                        helpText(h4("Data Source: National Oceanic & Atmospheric Administration",style="color:#239eba;")),
                                                        
                                                        imageOutput("plot2")
                                                        
                                                        )
                                        )
                               ),
                               ## Next Panel
                               
                               tabPanel("Data and Source Code",value = "Resources",
                                        fluidRow(column(12,style="background-color:#D5F5E3;"
                                                        ))
                                        
                                        
                               )
                     
                  )
 )
)

