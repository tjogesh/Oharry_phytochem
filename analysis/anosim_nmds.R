
#scent
scent_data = field_data %>% 
  select(pop_ord, year, lin_phenotype, all_of(scent_cols), tic_peak_area) %>% 
  na.omit() %>% 
  mutate(year=as.character(year), 
         pop_ord=as.character(pop_ord)) 

scent_data<-scent_data %>% 
  mutate_if(is.numeric, ~./tic_peak_area) 

scent_data %>% 
  count(lin_phenotype)

sqrt_scent.dis<-distance(sqrt(scent_data[,scent_cols]), "bray-curtis")
pop <- scent_data %>% select(pop_ord) %>% pull()
popDiffall<- anosim(sqrt_scent.dis, pop)
popDiffall

#nmds
scent.nmds <- ecodist::nmds(sqrt_scent.dis , mindim=2, maxdim=2, nits=3)

scent.nmds.df <- ecodist::nmds.min(scent.nmds)

scent.nmds.df$pop_ord <-scent_data$pop_ord
scent.nmds.df$lin_phenotype <-scent_data$lin_phenotype
scent.nmds.df$year <-scent_data$year

scent.nmds.df %>% 
  ggplot(aes(x=X2, y=X1, color = factor(lin_phenotype))) + 
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = lin_phenotype), color="gray")+
  geom_point( size = 0.8, alpha=0.6, aes(color=factor(lin_phenotype)) )+
  labs(x = "NMDS 1", y = "NMDS 2", fill = "Chemotype", color = "Chemotype") +
  theme_bw()+
  scale_color_manual(labels = c("lin+", "lin-"), values = c("#1A5276","#FF5733")) +
  scale_fill_manual(labels = c("lin+", "lin-"), values = c("#1A5276","#FF5733"))+
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))+
  geom_hline(yintercept = 0, alpha=0.3)+
  geom_vline(xintercept = 0, alpha=0.3)+
  xlab("NMDS1")+
  ylab("NMDS2")

scent.nmds.df %>% 
  filter(pop_ord %in% c("CC","PW","DC","TRIN")) %>% 
  mutate(pop_ord=fct_relevel(pop_ord,c("CC","PW","TRIN", "DC"))) %>% 
  ggplot(aes(x=X2, y=X1, color = pop_ord, shape=factor(year))) + 
  geom_point( size = 2, alpha=0.6) +
  labs(x = "NMDS 1", y = "NMDS 2", color = "Population",
       shape = "Year") +
  theme_bw()+
  scale_color_manual(values = c("#FF5733","#FFC300", "#5DADE2", "#1A5276")) +
  scale_shape_manual(values=c(0, 4, 15))+
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))+
  geom_hline(yintercept = 0, alpha=0.3)+
  geom_vline(xintercept = 0, alpha=0.3)

#anosim multiple years
get_anosim<-function(df){
  sqrt_scent.dis<-distance(sqrt(df[,scent_cols]), "bray-curtis")
  year<-df %>% select(year) %>% pull()
  popDiffall<- anosim(sqrt_scent.dis, year)
  return(popDiffall)
}

get_anosim(multiple_years_CC)

multiple_years<- scent_data %>% 
  filter(pop_ord %in% c("CC","PW","DC","TRIN")) 

yearwise_per_pop<-multiple_years %>% 
  droplevels() %>% 
  split(.$pop_ord) %>% 
  map(~get_anosim(.x))



#scent by year 
field_data %>% 
  filter(tic_peak_area<1000000) %>% 
  ggplot(aes(x= year , y = tic_peak_area, fill = year))+
  facet_grid(.~pop_ord)+
  geom_boxplot(position=position_dodge2(preserve = "single"))+
  theme_bw()+
  scale_fill_manual(values = wes_palette("Royal1"))+
  ylab("Total emission / ng / g flw")+
  xlab("")+
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(1)),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))

#prop plants with linalool 
prop_linalool<-field_data %>% 
  mutate(linalool_poly= ifelse(linalool>0, 1, 0)) %>% 
  group_by(pop_ord, lin_phenotype) %>% 
  summarise(lin_plants=sum(linalool_poly, na.rm=T), 
            total=n(),
            prop=lin_plants/total) 

prop_linalool%>% 
  ungroup() %>% 
  group_by(lin_phenotype) %>% 
  summarise(average_num_per_pop= mean(prop, na.rm = T))
  
#in proportion
scent_data %>% 
  pull(linalool) %>% 
  max()
