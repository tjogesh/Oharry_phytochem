#morph
morph_data <- field_data %>% 
  select(pop_ord, year, all_of(morph_cols)) %>% 
  na.omit() %>% 
  mutate(year=as.character(year), 
         pop_ord=as.character(pop_ord)) 

#anosim
sqrt_morph.dis<-distance(sqrt(morph_data[,morph_cols]), "bray-curtis")
pop <- morph_data %>% select(pop_ord) %>% pull()
popDiffmorph<- anosim(sqrt_morph.dis, pop)
popDiffmorph

#scent
scent_data <- field_data %>% 
  mutate(linalool_poly= ifelse(linalool>0, 'lin+', 'lin-')) %>% 
  select(pop_ord, year, linalool_phenotype, all_of(scent_cols), tic_peak_area, linalool_poly) %>% 
  na.omit() %>% 
  mutate(year=as.character(year), 
         pop_ord=as.character(pop_ord)) 

#convert into proportions for analysis
scent_data<-scent_data %>% 
  mutate_if(is.numeric, ~./tic_peak_area) 

scent_data %>% 
  count(linalool_phenotype)

#anosim
sqrt_scent.dis<-distance(sqrt(scent_data[,scent_cols]), "bray-curtis")
pop <- scent_data %>% select(pop_ord) %>% pull()
popDiffall<- anosim(sqrt_scent.dis, pop)
popDiffall

#nmds
scent.nmds <- ecodist::nmds(sqrt_scent.dis , mindim=2, maxdim=2, nits=3)

scent.nmds.df <- ecodist::nmds.min(scent.nmds)

scent.nmds.df$pop_ord <-scent_data$pop_ord
scent.nmds.df$linalool_phenotype <-scent_data$linalool_phenotype
scent.nmds.df$year <-scent_data$year
scent.nmds.df$poly<-scent_data$linalool_poly

#field data NMDS
scent.nmds.df %>% 
  ggplot(aes(x=X2, y=X1)) + 
  #stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = linalool_phenotype), color="gray")+
  geom_point( size = 2, alpha=0.8, shape=21,color = "black",
              aes(fill=factor(linalool_phenotype)) )+
  labs(x = "NMDS 1", y = "NMDS 2", fill = "Chemotype", color = "Chemotype") +
  theme_bw()+
  #scale_color_manual(values = lin_palette) +
  scale_fill_manual(values = lin_palette)+
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
ggsave("Figs/NMDSallfield_v2.pdf", width = 11, height = 8.5, units = "in")


#NMDS by year
scent.nmds.df %>% 
  filter(pop_ord %in% c("CC","PW","DC","TRIN")) %>% 
  mutate(pop_ord=fct_relevel(pop_ord,c("CC","PW","TRIN", "DC"))) %>% 
  ggplot(aes(x=X2, y=X1)) + 
  geom_point( size = 2, alpha=0.8, color = "black",
              aes(fill=factor(linalool_phenotype), 
                  shape=factor(year)) )+
  labs(x = "NMDS 1", y = "NMDS 2", fill = "Chemotype",
       shape = "Year") +
  theme_bw()+
  scale_fill_manual(values = lin_palette) +
  scale_shape_manual(values=c(21, 22, 24))+
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
ggsave("Figs/NMDSyear_v2.pdf", width = 11, height = 8.5, units = "in")

#anosim multiple years
get_anosim<-function(df){
  sqrt_scent.dis<-distance(sqrt(df[,scent_cols]), "bray-curtis")
  year<-df %>% select(year) %>% pull()
  popDiffall<- anosim(sqrt_scent.dis, year)
  return(popDiffall)
}


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
  facet_grid(.~pop_ord, scales = "free_x")+
  #geom_boxplot(position=position_dodge2(preserve = "single"), alpha = 0.7)+
  geom_violin(alpha = 0.7)+
  theme_bw()+
  scale_fill_manual(values = wes_palette("Royal1"))+
  ylab("Total emission / ng / g flw")+
  xlab("")+
  #coord_flip() +
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(1)),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/TotalEmissionYear_violin.pdf", width = 11, height = 8.5, units = "in")

#stats yr + pop comparisson 
total_em.lm<-lm(log(tic_peak_area)~year+pop_ord, field_data)
summary(total_em.lm)
anova(total_em.lm)
plot(total_em.lm, 2)
plot(total_em.lm, 3)
plot(total_em.lm, 1)
plot(total_em.lm, 5)

hist(log(field_data$tic_peak_area) )

#comparison of total emissions without linalool 
total_em_no_lin.lm<-lm( (tic_peak_area-linalool)~year+pop_ord, field_data)
summary(total_em_no_lin.lm)
anova(total_em_no_lin.lm)


field_data %>% 
  mutate(linalool_poly= ifelse(linalool>0, 'lin+', 'lin-')) %>% 
  select(linalool_poly, pop_ord, linalool, tic_peak_area) %>% 
  na.omit() %>% 
  ggplot(aes(x=linalool_poly, y=tic_peak_area, fill = linalool_poly))+
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
        axis.text.x = element_text(size = rel(1)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/TotalEmissionLinPoly.pdf", width = 11, height = 8.5, units = "in")

#stats lin polymorphism + pop comparisson 
data<-field_data %>% 
  mutate(linalool_poly= ifelse(linalool>0, 'lin+', 'lin-')) 
total_em_poly.lm<-aov(log(tic_peak_area)~linalool_poly*pop_ord, data)
summary(total_em_poly.lm)
anova(total_em_poly.lm)  
TukeyHSD(total_em_poly.lm)
  
  
#prop plants with linalool 
prop_linalool<-field_data %>% 
  mutate(linalool_poly= ifelse(linalool>0, 1, 0)) %>% 
  group_by(pop_ord, linalool_phenotype) %>% 
  summarise(lin_plants=sum(linalool_poly, na.rm=T), 
            total=n(),
            prop=lin_plants/total) 

prop_linalool%>% 
  ungroup() %>% 
  group_by(linalool_phenotype) %>% 
  summarise(average_num_per_pop= mean(prop, na.rm = T))
  
#lin proportion
scent_data %>% 
  pull(linalool) %>% 
  max()
