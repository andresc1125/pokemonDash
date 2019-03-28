library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(forcats)

library(colourpicker)
library(crosstalk)
library(leaflet)

# This is responsible for Loading Pokemon Data
pokemon<-read_csv("~/Desktop/Statistics Master /Tidying R/pokemonPJ/Npokemon.csv")
attach(pokemon)
pokemon<-as.data.frame(pokemon)
pokemon$is_legendary<-as.factor(pokemon$is_legendary)
pokemon$generation<-as.factor(pokemon$generation)
pokemon$type1<-as.factor(pokemon$type1)

pokemon$type2[is.na(pokemon$type2)]<-"no"
pokemon$type2<-as.factor(pokemon$type2)

pokemon$weight_kg<-as.numeric(pokemon$weight_kg)
pokemon$height_m<-as.numeric(pokemon$height_m)

# reorder 
pokemon$type1<-fct_infreq(pokemon$type1)
pokemon$type2<-fct_infreq(pokemon$type2)

# Define UI for application that draws a histogram
ui<-navbarPage(
  h1("Welcome to Pokemon"),
  
  
  ##First tab panel about General Data
  
  # tabPanel("Pokemon World",
  #         fluidPage(
  #          mainPanel(
  
  # CHOOSE YOUR POKEMON by 4 requests 
  tabsetPanel(type="tabs",
              tabPanel("Overview",
                       h2("Pokemon Basic Data"),
                       DT::dataTableOutput("My_Pokemon")) #tabpanel
  ), #tabsetPanel
  # ) #fmainpanel
  # ) #fluidpage
  #), #tabpanel
  
  
  ############ add plot in ############
  ####################################
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
  ),#sidebarlayout
  
  
  ##########choose by experience and generations ###############   
  ######################### ####################################     
  sliderInput(inputId = "experience",label="Experience Growth",
              ## from range(pokemon$experience_growth
              min=600000,max=1640000, 
              value=c(800000,1000000)),
  selectInput("generation","Generation",
              choice=c("ALL",levels(pokemon$generation))),
  
  # add one plot here
  plotOutput("Plot_Experience_Generation"),
  tableOutput("Table_against_information")
  
  
) #navbar


# Server 
server<-function(input,output){
  
  
  ##render data table ###
  
  output$My_Pokemon<-renderDT(
    pokemon[c(1,38,39,26:29,34:36)], # reactive data 
    class="display nowrap compact", # style
    filter="top", # put the column filters 
    
    option=list(  # options
      scrollX=TRUE # allow user to scroll wide tables horizontally
    ) # close option
  ) #renderDT
  
  
  ############ H_W_plot ############
  ####################################    
  output$H_W_plot<-renderPlotly({
    #convert the ggplot2 to a plotly plot
    ggplotly({
      data1<-subset(na.omit(pokemon[26:41]),
                    is_legendary %in% input$Legendary &
                      type1 %in% input$type1 & type2 %in% input$type2 &
                      weight_kg>=input$weight[1] & weight_kg<=input$weight[2] &
                      height_m>=input$height[1] & height_m<=input$height[2] &
                      hp>=input$hp[1] & hp<=input$hp[2])
      
      p1<-ggplot(data1,aes(x=attack,y=defense,group=type1))+
        geom_point(col=input$colour)+ggtitle(input$title)
      
      if (input$fit){
        p1<-p1+geom_smooth(method = "lm")
      }   #if
      p1
    })  #ggplotly
    
  }) #renderPlotly
  
  
  #########output of experience and generations ###############   
  ######################## ################################### 
  output$Table_against_information<-renderTable({
    data2<-subset(
      pokemon[c(1,40,30,5:25)],
      experience_growth>=input$experience[1] & 
        experience_growth<=input$experience[2]
    )
    if (input$generation!="ALL"){
      data2<-subset(
        pokemon[c(1,40,30,5:25)],
        generation==input$generation
      )
    } #if
    data2
  }) #renderTable  
  
  
  ## with the plot render 
  output$Plot_Experience_Generation<-renderPlot({
    # use the same filtered data with previous table 
    data2<-subset(
      pokemon[c(1,40,30,5:25)],
      experience_growth>=input$experience[1] & 
        experience_growth<=input$experience[2]
    )
    if (input$generation!="ALL"){
      data2<-subset(
        pokemon[c(1,40,30,5:25)],
        generation==input$generation 
      )
    } #if
    ggplot(data2,aes(base_total,experience_growth))+
      geom_point()+
      scale_y_log10()
  }) #renderPlot
  
} #server



# Run the application 
shinyApp(ui = ui, server = server)

