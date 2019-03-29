#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
source("data_gather.R")


# Define server logic required to draw a histogram
server = shinyServer(function(input, output) {
  
  output$plot_p1_hp <- renderPlot({build_plot_p1_hp(input$p1_pokemon_type)})
  output$plot_p1_attack <- renderPlot({build_plot_p1_attack(input$p1_pokemon_type)})
  output$plot_p1_deffense <- renderPlot({build_plot_p1_deffense(input$p1_pokemon_type)})
  output$plot_p1_speed <- renderPlot({build_plot_p1_speed(input$p1_pokemon_type)})
  output$plot_p1_hist_t1 <- renderPlot({get_p1_type1_counts()})
  output$plot_p1_hist_t2 <- renderPlot({get_p1_type2_counts()})
  output$p3_plot_scatter <- renderPlot({get_p3_plot_scatter(input$p3_weight_filter,
                                                            input$p3_height_filter,
                                                            input$p3_hp_filter,
                                                            input$p3_type1_filter
                                                            )})
  output$p3_plot_corrplot <- renderPlot({get_p3_corrplot_scatter(input$p3_weight_filter,
                                                                 input$p3_height_filter,
                                                                 input$p3_hp_filter,
                                                                 input$p3_type1_filter
  )})
  output$p2_box_plot <- renderPlot({get_p2_box_plot(input$p2_weak_against,
                                                    input$p2_pokemon_generations,
                                                    input$p2_against_slide,
                                                    input$p2_power_range,
                                                    input$p2_attack_min_max,
                                                    input$p2_deffense_min_max,
                                                    input$p2_speed_min_max
  )})
  output$p2_pie_plot <- renderPlot({get_p2_pie_plot(
                                input$p2_weak_against,
                                input$p2_against_slide,
                                input$p2_power_range,
                                input$p2_attack_min_max,
                                input$p2_deffense_min_max,
                                input$p2_speed_min_max
  )})
  
  output$My_Pokemon<-renderDT(
    useful_data[c("name","type1","type2","abilities","attack","defense","hp","speed","height_m","weight_kg","capture_rate","base_total")], # reactive data 
    class="display nowrap compact", # style
    filter="top", # put the column filters 
    
    option=list(  # options
      scrollX=TRUE # allow user to scroll wide tables horizontally
    ) # close option
  ) #renderDT
  
})
