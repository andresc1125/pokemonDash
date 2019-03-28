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
                            ),
                            fluidRow(
                              column(6,
                                     plotOutput("plot_p1_hist_t1")
                              ),
                              column(6,
                                     plotOutput("plot_p1_hist_t2")
                              )
                            )
                            
                          )
                 ),
                 tabPanel("Weak Map",
                          fluidPage(
                            fluidRow(
                              column(2,"",
                                     selectInput("p2_weak_against", label ="Select Against ability", 
                                                 choices =colnames_againts_p2_powers,
                                                 selected = colnames_againts_p2_powers[1])
                              ),
                              column(2,"",
                                     selectInput("p2_pokemon_generations", label ="Select Generation", 
                                                 choices = pokemon_generations,
                                                 selected = pokemon_generations[1])
                              ),
                              column(4,"",
                                     sliderInput("p2_against_slide", label = "Damage Range", min = p2_attrib_min_max$min, 
                                                 max =  p2_attrib_min_max$max, value =c(p2_attrib_min_max$min, p2_attrib_min_max$max)  )
                              )
                            ),
                            fluidRow(
                              column(3,"",
                                     sliderInput("p2_power_range", label = "Hp Range", min = p2_hp_min_max$min, 
                                                 max =  p2_hp_min_max$max, value =c(p2_hp_min_max$min, p2_hp_min_max$max)  )
                              ),
                              column(3,"",
                                     sliderInput("p2_attack_min_max", label = "Attack Range", min = p2_attack_min_max$min, 
                                                 max =  p2_attack_min_max$max, value =c(p2_attack_min_max$min, p2_attack_min_max$max)  )
                              ),
                              column(3,"",
                                     sliderInput("p2_deffense_min_max", label = "Deffense Range", min = p2_deffense_min_max$min, 
                                                 max =  p2_deffense_min_max$max, value =c(p2_deffense_min_max$min, p2_deffense_min_max$max)  )
                              ),
                              column(3,"",
                                     sliderInput("p2_speed_min_max", label = "Speed Range", min = p2_speed_min_max$min, 
                                                 max =  p2_speed_min_max$max, value =c(p2_speed_min_max$min, p2_speed_min_max$max)  )
                              )
                            ),
                            fluidRow(
                              column(6,
                                     "Graph"
                              ),
                              column(6,
                                     "Graph"
                              )
                            )
                          )
                 ),
                 tabPanel("Power Concentrations",
                          ### add plot in ####
                          sidebarLayout(
                            sidebarPanel(
                              textInput("title","Title","Pokemon Attack vs. Defense"),
                              
                              checkboxInput("fit","add line of best fit", FALSE),
                              
                              sliderInput("weight","Weight",min(na.omit(pokemon$weight_kg)),
                                          max(na.omit(pokemon$weight_kg)),
                                          value=c(20,25.5)),
                              
                              sliderInput("height","Height",min(na.omit(pokemon$height_m)),
                                          max(na.omit(pokemon$height_m)),
                                          value=c(8,8.5)),
                              
                              sliderInput("hp","HP value",min(pokemon$hp),max(pokemon$hp),
                                          value=c(20,35)),
                              
                              selectInput("type1","Primary type",
                                          choices = levels(pokemon$type1),
                                          multiple = TRUE,
                                          selected = "grass"),
                              selectInput("type2","Second type",
                                          choices = levels(pokemon$type2),
                                          multiple = TRUE,
                                          selected = "flying"),
                              checkboxInput(inputId = "Lengendary",label = "Only for Lengendary",
                                            FALSE),
                              colourInput("color","Point Colour",value="yellow")
                            ),  #sidebarPanel
                            mainPanel(
                              # output with plotly version
                              plotlyOutput("H_W_plot")
                            ) #mainpanel
                          )#sidebarlayout
                 )
                 #  titlePanel
) # navbarPage
