#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Urban Heat Island Calculation Tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          textInput("pop", "Population"),
          textInput("baselineISA", "Baseline ISA"),
          textInput("baselineTrees", "Baseline Trees"),
          textInput("baselineGrass", "Baseline Grass"),
          textInput("ScenarioISA", "Scenario ISA"),
          textInput("ScenarioTrees", "Scenario Trees"),
          textInput("ScenarioGrass", "Scenario Grass")
          
          #CSV upload 
         
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("Results"),
            textOutput("test")
        )
    )
))
