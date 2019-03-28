library(tidyverse);

raw_data = read.csv("pokemon.csv")

useless_columns = c("japanese_name","pokedex_number","percentage_male","experience_growth","base_happiness")

#import the data as a tibble
tb <- as_tibble(raw_data)


#remove columns we dont care about
useful_data = tb %>%select(-one_of(useless_columns))

#the classification of the pokemon have the work "pokemon" and is a visual trash for the  dashboards
#we have to mutate the data 
useful_data = useful_data %>% mutate(classfication = str_remove_all(classfication, "Pokémon"))


useful_data %>% filter(against_dark == 1)

##GGplots to vary with tidy filter by pokemon type base stats, hp, attack an deffense

# filter by type apply tidy control, noly apply tidy
# hp barplot

##general data for controls
pokemon_types_type1 = levels(useful_data$type1) 
pokemon_types_type2 = levels(useful_data$type2) 

#filter all the colnames that start with against_ as the have useful information
colnames_againts_p2_powers = colnames(useful_data %>%  select(starts_with("against_")) )

#base pokemon generatiions
pokemon_generations = useful_data %>% distinct(generation)

#get min_max power

p2_hp_min_max = list(min=min(useful_data %>% select(hp)), max=max(useful_data %>% select(hp)))
p2_attack_min_max = list(min=min(useful_data %>% select(sp_attack)), max=max(useful_data %>% select(sp_attack)))
p2_deffense_min_max = list(min=min(useful_data %>% select(sp_defense)), max=max(useful_data %>% select(sp_defense)))
p2_speed_min_max = list(min=min(useful_data %>% select(speed)), max=max(useful_data %>% select(speed)))

p3_weight_min_max = list(min=min(useful_data %>% na.omit() %>% select(weight_kg)), max=max(useful_data %>% na.omit() %>% select(weight_kg)))
p2_height_min_max = list(min=min(useful_data %>% na.omit() %>%select(height_m)), max=max(useful_data %>% na.omit() %>% select(height_m)))


variable_to = "against_psychic"
p2_attrib_min_max = list(min=min(useful_data %>% select(variable_to)), max=max(useful_data %>% select(variable_to)))


build_plot_p1_hp <- function(pokemon_type){
  data_to_plot_p1_hp =  useful_data %>% filter(type1 == pokemon_type)
  plot_hp_filtered = ggplot(data = data_to_plot_p1_hp,  aes(x=hp)) +
    geom_histogram(aes(y=..density..),fill="red", alpha=0.8, bins = 10,color="black") +
    geom_density(alpha=.3, fill="white") + 
    ggtitle(paste("Health Points for type ", pokemon_type )) + 
    labs(y="Density", x = "Pokemon Health Points")
  
  return (plot_hp_filtered)
}

build_plot_p1_attack <- function(pokemon_type){
  data_to_plot =  useful_data %>% filter(type1 == pokemon_type)
  plot_hp_filtered = ggplot(data = data_to_plot,  aes(x=sp_attack)) +
    geom_histogram(aes(y=..density..),fill="orange", alpha=0.8, bins = 10,color="black") +
    geom_density(alpha=.3, fill="white") + 
    ggtitle(paste("Attack Points for type ", pokemon_type )) + 
    labs(y="Density", x = "Pokemon Attack Points")
  
  return (plot_hp_filtered)
}

build_plot_p1_deffense <- function(pokemon_type){
  data_to_plot =  useful_data %>% filter(type1 ==pokemon_type)
  plot_hp_filtered = ggplot(data = data_to_plot,  aes(x=sp_defense)) +
    geom_histogram(aes(y=..density..),fill="yellow", alpha=0.8, bins = 10,color="black") +
    geom_density(alpha=.3, fill="white")+ 
    ggtitle(paste("Deffense Points for type ", pokemon_type )) + 
    labs(y="Density", x = "Pokemon Deffense Points")
  
  return (plot_hp_filtered)
}

build_plot_p1_speed <- function(pokemon_type){
  data_to_plot =  useful_data %>% filter(type1 ==pokemon_type)
  plot_hp_filtered = ggplot(data = data_to_plot,  aes(x=speed)) +
    geom_histogram(aes(y=..density..),fill="green", alpha=0.8, bins = 10,color="black") +
    geom_density(alpha=.3, fill="white")+ 
    ggtitle(paste("Speed Points for type ", pokemon_type )) + 
    labs(y="Density", x = "Pokemon Speed Points")
  
  return (plot_hp_filtered)
}



#tidy function
get_p1_type1_counts <-function(){
  p1_freq_type_1 = useful_data %>% mutate(
    type1frec = fct_infreq(useful_data$type1),
  ) %>% select(type1frec)
  
  hist<-ggplot(p1_freq_type_1,aes(x=type1frec,y=..count..,fill=factor(..x..)))+geom_bar(aes(y=..count..,group=1))+
    ggtitle("Pokemon Primary Skill")+
    guides(fill=guide_legend(title="Primary Type"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(lineheight=1, face="bold"))
  
  return(hist)
}

get_p1_type2_counts <-function(){
  #tidy function
  p1_freq_type_2 = useful_data  %>% filter(type2 != "") %>% mutate(
    type2frec = fct_infreq(type2),
  ) %>% select(type2frec)
  
  hist<-ggplot(p1_freq_type_2,aes(x=type2frec,y=..count..,fill=factor(..x..)))+geom_bar(aes(y=..count..,group=1))+
    ggtitle("Pokemon Seccond Skill")+
    guides(fill=guide_legend(title="Seccond Type"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(lineheight=1, face="bold"))
  
  return(hist)
}


