#reorder data to make stacked bars sort by proportion
data2 <- data %>%
mutate(Plant_genera = fct_reorder(Plant_genera, Scaled_plant_prop)) 

#make plant genera collections by bee species plot
ggplot(data2, aes(fill=Plant_genera, y=Scaled_plant_prop, x=Bee_species)) + 
  geom_bar(position="fill", stat="identity", colour="white")+ 
  theme(axis.text.x = element_text(face = "italic"))+
  geom_text(aes(label=Plant_genera), size= 4, position=position_fill(vjust=0.5), colour="white")+
  labs(y = "Proportion collected", x = "Bee species")
