library(ggplot2)
library(shiny)
library(plyr)
library(reshape)
library(reshape2)
library(ggvis)
library(plotly)

shinyUI(navbarPage("World Data",
                   tabPanel("Plot",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput('region'),
                                uiOutput('country'),
                                sliderInput('popSize', 'Scale Population',
                                            min = 100, max = 1000, 
                                            value = 500, step = 100, ticks = F),
                                h5("Hover over a point to see the country name; click for more information.",align='center')
                              ),
                              #Render the results
                              mainPanel(
                                fluidRow(
                                  ggvisOutput("plot")
                                  
                                ),
                                fluidRow(
                                  column(3),
                                  column(1, actionButton("play", "Play")),
                                  column(8,sliderInput('year', 'Please Select A Year',
                                                        min = 1960, max = 2014, value = 1960, step = 1,
                                                        sep = ''))
                                )
                              )
                            )
                   ),
                   tabPanel("Data",
                              mainPanel(
                                dataTableOutput('df')
                              )
                            )))