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
                              column(12,
                                     plotOutput("p2_box_plot")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     plotOutput("p2_pie_plot")
                              )
                            )
                          )
                 ),
                 tabPanel("Power Concentrations",
                          ### add plot in ####
                          sidebarLayout(
                            sidebarPanel(
                              textInput("title","Title","Pokemon Speed vs. Defense"),
                              
                              
                              sliderInput("p3_weight_filter","Weight",min(na.omit(p3_weight_min_max$min)),
                                          max(na.omit(p3_weight_min_max$max)),
                                          value=c(p3_weight_min_max$min,p3_weight_min_max$max)),
                              
                              sliderInput("p3_height_filter","Height",min(na.omit(p2_height_min_max$min)),
                                          max(na.omit(p2_height_min_max$max)),
                                          value=c(p2_height_min_max$min,p2_height_min_max$max)),
                              
                              sliderInput("p3_hp_filter","HP value",p2_hp_min_max$min,p2_hp_min_max$max,
                                          value=c(p2_hp_min_max$min,p2_hp_min_max$max)),
                              
                              selectInput("p3_type1_filter","Primary type",
                                          choices = pokemon_types_type1,
                                          selected = pokemon_types_type1[2]),
                              selectInput("p3_type2_filter","Second type",
                                          choices = pokemon_types_type2,
                                          selected = pokemon_types_type2[2])
                              ),  #sidebarPanel
                            mainPanel(
                              # output with plotly version
                              fluidPage(
                                fluidRow(
                                  column(10,"",
                                         plotOutput("p3_plot_scatter")
                                  )
                                ),
                                fluidRow(
                                  column(10,"",plotOutput("p3_plot_corrplot")
                                  )
                                )
                                )
                            ) #mainpanel
                          )#sidebarlayout
                 )
                 #  titlePanel
) # navbarPage
