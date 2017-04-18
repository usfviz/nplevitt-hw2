packageList = c('ggplot2', 'shiny', 'plyr', 'reshape', 'resahpe2', 'ggvis')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(ggplot2)
library(shiny)
library(plyr)
library(reshape)
library(reshape2)
library(ggvis)

shinyUI(navbarPage("World Data",
                   tabPanel("Plot",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput('xaxis', 'Select X-Axis Variable', 
                                            choices = list(Life_Expectancy='life', Fertility_Rate='fertility', Population='population')),
                                selectInput('yaxis', 'Select Y-Axis Variable', 
                                            choices = list(Fertility_Rate='fertility', Life_Expectancy='life', Population='population')),
                                selectInput('plotscale', 'Select Variable to Scale Size By', 
                                            choices = list(Population='population', Fertility_Rate='fertility', Life_Expectancy='life')),
                                uiOutput('region'),
                                uiOutput('country'),
                                sliderInput('popSize', 'Scale Magnitude',
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
                            sidebarLayout(
                              sidebarPanel(h3("Selected Data"), textOutput('tst')),
                              mainPanel(dataTableOutput('df'))
                            )
                              
                            )))