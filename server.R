#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("data_gather.R")


# Define server logic required to draw a histogram
server = shinyServer(function(input, output) {
   
  output$plot_p1_hp <- renderPlot({build_plot_p1_hp(input$p1_pokemon_type)})
  output$plot_p1_attack <- renderPlot({build_plot_p1_attack(input$p1_pokemon_type)})
  output$plot_p1_deffense <- renderPlot({build_plot_p1_deffense(input$p1_pokemon_type)})
  output$plot_p1_speed <- renderPlot({build_plot_p1_speed(input$p1_pokemon_type)})
  
})
