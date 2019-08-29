#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#install.packages("dqshiny")
library(dqshiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Urban Heat Island Calculation Tool"),

    # Sidebar with a slider input for number of bins

        # Show a plot of the generated distribution
        tabsetPanel(
            tabPanel("Inputs", textInput("pop", "Population of study area"),
                    textInput("baselineISA", "Baseline Impervious Surface Area (in sq. ft.)"),
                    textInput("baselineTrees", "Baseline Tree Canopy Cover (in sq. ft.)"),
                    textInput("baselineGrass", "Baseline Grass"),
                    textInput("ScenarioISA", "Scenario ISA"),
                    textInput("ScenarioTrees", "Scenario Trees"),
                    textInput("ScenarioGrass", "Scenario Grass"),
            fileInput("file1", "Choose Temperature Data CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            )),
            tabPanel("Results", textOutput("Results")),
            tabPanel("Test", textOutput("test")),
            tabPanel("Tempdata", tableOutput("contents")))
    )
)
