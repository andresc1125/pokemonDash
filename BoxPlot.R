library(ggplot2)
library(reshape2)

pokemon<-read_csv("~/Desktop/Statistics Master /Tidying R/pokemonPJ/pokemon.csv")
useless_columns = c("japanese_name","pokedex_number","percentage_male","experience_growth","base_happiness")

#remove columns we dont care about
useful_data = tb %>%select(-one_of(useless_columns))


# check the vars names and prepare for boxplot colunmns
#names(useful_data)

# here we got New Variables value and varaible for boxplot.
new_var<-melt(useful_data,id_vars="names",measure.vars = c("defense","attack","sp_defense","sp_attack","hp","speed"))
new_var<-as.data.frame(new_var)

new_var$generation<-as.factor(new_var$generation)
new_var$is_legendary<-as.factor(new_var$is_legendary)
# boxplot
(boxplot1<-ggplot(new_var,aes(x=variable,y=value,fill=is_legendary))+geom_boxplot(position="dodge")
                 +ggtitle("Output BoxPlot")+scale_fill_brewer(palette="Greens"))

  



########################################################
###############Correlation ######################
library(ggcorrplot)
library(RColorBrewer)

## aim to create dataset with all AGAINST_vars
against_inf<-useful_data[c( "name","against_bug","against_dark","against_dragon","against_electric","against_fairy",   
                        "against_fight","against_fire","against_flying","against_ghost","against_grass","against_ground",  
                          "against_ice","against_normal","against_poison","against_psychic","against_rock","against_steel",   
                           "against_water")]
against_inf[,-1]<-as.data.frame(against_inf[,-1])
corr<-round(cor(against_inf[,-1]),1)
## to comput the matrix of correlation p_value
# p.mat = cor_pmat(against_inf[,-1])

## visualize 
(ggcorrplot(corr,hc.order = TRUE,method = "circle",type = "lower",insig = "blank",
           colors = brewer.pal(n = 3,name = "RdYlBu")))

