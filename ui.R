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
ui <- navbarPage("Shiny app",
                 tabPanel("Base Stats Densities",
                          fluidPage(
                            fluidRow(
                              column(3, h3("Base Stats"),
                                     selectInput("p1_pokemon_type", label ="Select pokemon type", 
                                                 choices = pokemon_types_type1,
                                                 selected = pokemon_types_type1[1])
                              )
                            ),
                            fluidRow(
                              column(6,
                                     plotOutput("plot_p1_hp")
                              ),
                              column(6,
                                     plotOutput("plot_p1_attack")
                              )
                            ),
                            fluidRow(
                              column(6,
                                     plotOutput("plot_p1_deffense")
                              ),
                              column(6,
                                     plotOutput("plot_p1_speed")
                              )
                            )
                            
                          )
                 ), #  titlePanel
                 
                 # CHOOSE YOUR POKEMON by 4 requests 
                 tabsetPanel(type="tabs",
                             tabPanel("Overview",
                                      h2("Pokemon Basic Data"),
                                      DT::dataTableOutput("My_Pokemon")) #tabpanel
                 ) #tabsetPanel
) # navbarPage
