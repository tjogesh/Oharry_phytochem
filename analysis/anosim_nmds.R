#morph
morph_data <- field_data %>% 
  select(pop_ord, year, all_of(morph_cols)) %>% 
  na.omit() %>% 
  mutate(year = as.character(year), 
         pop_ord = as.character(pop_ord)) 

#scent
scent_data <- field_data %>% 
  mutate(linalool_poly= ifelse(linalool>0, 'lin+', 'lin-')) %>% 
  select(pop_ord, year, linalool_phenotype, all_of(scent_cols), tic_peak_area, linalool_poly) %>% 
  na.omit() %>% 
  mutate(year=as.character(year), 
         pop_ord=as.character(pop_ord)) %>% 
#convert into proportions for analysis
  mutate_if(is.numeric, ~./tic_peak_area) 

scent_data %>% 
  count(linalool_phenotype)

## --------------------------------------------------------
#scent by year violin plot Fig 3A
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

## ----------------------------
#NMDS PLOTS
scent.nmds <- ecodist::nmds(sqrt_scent.dis , mindim=2, maxdim=2, nits=3)
scent.nmds.df <- ecodist::nmds.min(scent.nmds)

scent.nmds.df$pop_ord <-scent_data$pop_ord
scent.nmds.df$linalool_phenotype <-scent_data$linalool_phenotype
scent.nmds.df$year <-scent_data$year
scent.nmds.df$poly<-scent_data$linalool_poly

#field data NMDS Fig 3B
scent.nmds.df %>% 
  ggplot(aes(x=X2, y=X1)) + 
  #stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = linalool_phenotype), color="gray")+
  geom_point( size = 4, alpha=0.8, shape=21,color = "black",
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


#NMDS by year Fig 3C
scent.nmds.df %>% 
  filter(pop_ord %in% c("CC","PW","DC","TRIN")) %>% 
  mutate(pop_ord=fct_relevel(pop_ord,c("CC","PW","TRIN", "DC"))) %>% 
  ggplot(aes(x=X2, y=X1)) + 
  geom_point( size = 4, alpha=0.8, color = "black",
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


## ----------------------------
# ANOSIMS 

#anosim -- scent differentiation by linalool + and - plants - 3B 
sqrt_scent.dis <-distance(sqrt(scent_data[,scent_cols]), "bray-curtis")
linalool_poly <- scent_data %>% select(linalool_poly) %>% pull()
lin_poly_Diffall <- anosim(sqrt_scent.dis, linalool_poly)
lin_poly_Diffall

#anosim -- scent differentiation by phenotype - 3B 
linalool_pheno <- scent_data %>% select(linalool_phenotype) %>% pull()
lin_pheno_Diffall <- anosim(sqrt_scent.dis, linalool_pheno)
lin_pheno_Diffall

#anosim -- scent differentiation by pop - 3B 
pop <- scent_data %>% select(pop_ord) %>% pull()
popDiffall <- anosim(sqrt_scent.dis, pop)
popDiffall


#anosim multiple years
get_anosim <- function(df){
  sqrt_scent.dis <- distance(sqrt(df[,scent_cols]), "bray-curtis")
  year <- df %>% select(year) %>% pull()
  popDiffall <- anosim(sqrt_scent.dis, year)
  return(popDiffall)
}


multiple_years <- scent_data %>% 
  filter(pop_ord %in% c("CC","PW","DC","TRIN")) 

# across years overall
get_anosim(multiple_years)

# across years by lin phenotype
yearwise_lin_poly <- multiple_years %>% 
  droplevels() %>% 
  split(.$linalool_poly) %>% 
  map(~get_anosim(.x))

yearwise_lin_phenotype <- multiple_years %>% 
  droplevels() %>% 
  split(.$linalool_phenotype) %>% 
  map(~get_anosim(.x))


## -------------------------------------------------
# lm to compare total emission by year and pop

hist(log(field_data$tic_peak_area) )

#comparison of total emissions with linalool 
total_em_pop.lm <- lm( log(tic_peak_area)~year+pop_ord, field_data)
summary(total_em_pop.lm)
anova(total_em_pop.lm)

#comparison of total emissions without linalool 
total_em_no_lin.lm <- lm( log(tic_peak_area-linalool)~year+pop_ord, field_data)
summary(total_em_no_lin.lm)
anova(total_em_no_lin.lm)


