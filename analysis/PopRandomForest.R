library(randomForest)

colnames(field_data)
data = field_data %>% 
  select(pop_ord, pop_clean, all_of(scent_cols), 
         floral_flare, herkogamy, tube_length, corolla_diam) %>% 
  mutate(pop_clean=as.factor(pop_clean)) %>% 
  na.omit()%>% 
  select(pop_ord, pop_clean, where(~ is.numeric(.x) && sum(.x) !=0 ))

data %>% 
  count(pop_ord)

main.forest <- randomForest(pop_clean~., data = data[,-1], importance=TRUE)

# View the forest results.
print(main.forest) 

# Importance of each predictor.
import<-importance(main.forest,type = 1)
varImpPlot(main.forest)

import<-data.frame(import)
import$traits<-row.names(import)
import$trait_type <- "scent"
import$trait_type[33:36] <- "morphology"

ylabels<- c( 'R(-) linalool', 
             'floral flare',
             'tube length',
             'corolla diameter',
             'jasmine lactone',
             'methyl farnesoate',
             'trans beta ocimene',             
             'beta myrcene',
             'cis beta ocimene',
             'herkogamy',
             'isophytol',
             'E,E farnesol',
             'beta caryophyllene',
             'phenylacetonitrile',
             'caryophyllene oxide',
             'alpha humulene',
             'methyl benzoate',
             'E E alpha farnesene',
             'geraniol',
             'nitrophenylethane',
             'E Z alpha farnesene',
             'alpha terpineol',
             'phenylacetaldoxime',
             'trans-beta farnesene',
             '2 phenylethanol',
             'methyl geranate',
             'trans nerolidol')
            
ggplot(import, aes(x=MeanDecreaseAccuracy, y=reorder(traits, MeanDecreaseAccuracy), color = trait_type)) +
  geom_point(shape=16, size =3, aes(fill=trait_type))+
  ylab('traits') +
  xlab('Mean Decrease in Accuracy') +
  #scale_y_discrete(labels=rev(ylabels))+
  scale_color_manual(values=c("darkgray","black"))+
  theme(#axis.text.x = element_blank(), 
        panel.grid.major.x= element_blank(),
        panel.grid.minor.x= element_blank(),
        panel.grid.major.y = element_line(colour = "lightgray", linetype = 'dashed'),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.x  = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggsave("Figs/RandomForest.pdf", width = 11, height = 8.5, units = "in")

