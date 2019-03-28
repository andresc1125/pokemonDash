library(tidyverse);

raw_data = read.csv("pokemon.csv")

factor(raw_data$pokedex_number)
useless_columns = c("japanese_name","is_legendary","pokedex_number","percentage_male","experience_growth","base_happiness")

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


build_plot_p1_hp <- function(pokemon_type){
  data_to_plot_p1_hp =  useful_data %>% filter(type1 ==pokemon_type)
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







