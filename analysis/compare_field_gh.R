
#greenhouse data
greenhouse_morph<-read_excel("raw_data/OharryGreenhouseMorph20082009.xlsx") %>% 
  rename_cols() %>% 
  mutate(vial_number=as.character(vial_number), 
         site_name=toupper(site_name),
         join_id=paste0(site_name,"_",vial_number))  

colnames(greenhouse_morph)

greenhouse_data <-gh_data %>% 
  select(source, name, pop_ord, lin_phenotype, species, id, all_of(scent_cols), tic_peak_area) %>% 
  mutate(join_id=paste0(pop_ord,"_",id)) %>% 
  left_join(greenhouse_morph) %>% 
  mutate(plant_id=ifelse(plant_id=="B57-12-2", "B 57-12-2", plant_id),
         plant_id=ifelse(plant_id=="BMR-30-9-1", "BMR 30-9-1", plant_id)) %>% 
  separate(plant_id, into=c("pop_morph", "parental_id"), sep=(" "), remove=F) %>% 
  separate(parental_id, into=c("mat_id", "flw_id", "rep"), sep=("-"), remove=F)


single_mat_lin_gh<-greenhouse_data %>% 
  group_by(site_name, mat_id) %>% 
  filter(flw_id==min(flw_id, na.rm=T)) %>% 
  filter(date==min(date, na.rm=T)) %>% 
  filter(time==min(time, na.rm=T)) %>% 
  ungroup()

single_mat_lin_gh %>% 
  count(pop_ord, lin_phenotype) %>% 
  write_csv('data_processed/pop_year_samples_gh.csv')

pops_in_gh<-c("BAC","FLO","PW","DC", "BMR", "BLOOM")

combined_dataset<-unadjusted_field_data %>% 
  select(source, name, pop_ord, lin_phenotype, species, id, all_of(scent_cols), tic_peak_area) %>% 
  filter(pop_ord %in% pops_in_gh) %>% 
  bind_rows(single_mat_lin_gh %>% 
              select(source, name, pop_ord, lin_phenotype, species, id, all_of(scent_cols), tic_peak_area)) %>%
  mutate(linalool_poly= ifelse(linalool>0, 'lin+', 'lin-')) %>% 
  na.omit()  



colnames(combined_dataset)
#convert to prop. of emissions

combined_dataset$Total <- rowSums(combined_dataset[,7:42])

combined_dataset_prop<-combined_dataset %>% 
  mutate_if(is.numeric, ~./Total) 
# 



##**************PLot Boxplots

num_samps<-combined_dataset %>% 
  mutate(linalool_PA = ifelse(linalool==0,0,1)) %>% 
  group_by(pop_ord,  source) %>% 
  summarise(prop_lin = sum(linalool_PA, na.rm = T)/n(), 
            total=n(),
            med_em = quantile(tic_peak_area, probs = 1))

combined_dataset %>% 
  #filter(linalool<400000) %>% 
  ggplot(aes(x=pop_ord, y= tic_peak_area, fill= source))+
  geom_boxplot()+
  geom_text(data = num_samps,
            size=3.5,
            aes(y=med_em, x = pop_ord,  
                label = paste0("n=",total), color=source), vjust = -0.7,
            position = position_dodge(width = .75))+
  theme_bw()+
  scale_fill_manual(labels=c("field", "greenhouse"),values = c("white","darkgray"))+
  scale_color_manual(values = c("black","black"))+
  ylab("Total emission / ng / flw")+
  xlab("")+
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(1)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/gh_field_boxplot.pdf", width = 11, height = 8.5, units = "in")

#stats compare gh vs field total emissions
total_em_gh.lm<-lm(log(tic_peak_area)~source+pop_ord, combined_dataset)
summary(total_em_gh.lm)
anova(total_em_gh.lm)

hist(log(combined_dataset$tic_peak_area) )

combined_dataset %>% 
  #filter(linalool<400000) %>% 
  ggplot(aes(x=pop_ord, y= linalool, fill= source))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values = c("#1A5276","#FF5733"))


combined_dataset %>% 
  ggplot(aes(x=source, y= tic_peak_area, fill= lin_phenotype))+
  geom_boxplot()+
  theme_bw()



##**************NMDS PROP
library(ecodist)
Combo_prop.md <- distance(sqrt(combined_dataset_prop[,scent_cols]), "bray-curtis")
Combo_prop.nmds <- nmds(Combo_prop.md, mindim=2, maxdim=2, nits=3)
Combo_prop.nmin <- nmds.min(Combo_prop.nmds)


##**************PLot NMDS

Combo_prop.nmin$source <-combined_dataset_prop$source
Combo_prop.nmin$lin_phenotype <-combined_dataset_prop$lin_phenotype
Combo_prop.nmin$poly <-combined_dataset_prop$linalool_poly

Combo_prop.nmin$pop_ord <-combined_dataset_prop$pop_ord

ggplot(Combo_prop.nmin) + 
  geom_point(aes(x=X1, y=X2, color=poly, shape=source), alpha=0.8, size=3)+
  #geom_text(aes(label=source),hjust=0.4, vjust=-1,size = rel(6))+
  labs(color = "Chemoytype\n", shape="Source\n") +
  scale_color_manual(labels = c("lin-", "lin+"), values = c("#1A5276","#FF5733")) +
  scale_shape_manual(labels = c("field collected", "greenhouse grown"), values=c(0, 16))+
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
ggsave("Figs/NMDS_GH_field_v2.pdf", width = 11, height = 8.5, units = "in")


#ANOSIM 
library(vegan)
data_lin_plus = combined_dataset_prop %>% 
  filter(lin_phenotype=="linalool") 

data_lin_plus %>% 
  count(source)

data_lin_minus = combined_dataset_prop %>% 
  filter(lin_phenotype=="no linalool") 

data_lin_minus %>% 
  count(source)

com_plus<-sqrt(data_lin_plus[,scent_cols])
sour_plus <- factor(data_lin_plus$source)
dist_plus.com <- vegdist(com_plus, method = "bray")
sourceDiff_plus<- anosim(dist_plus.com, sour_plus)


com_minus<-sqrt(data_lin_minus[,scent_cols])
sour_minus <- factor(data_lin_minus$source)
dist_minus.com <- vegdist(com_minus, method = "bray")
sourceDiff_minus<- anosim(dist_minus.com, sour_minus)

combined_dataset %>% 
  count(source, lin_phenotype)

stat.test <- combined_dataset %>%
  select(source, pop_ord, linalool) %>% 
  mutate(source= as.factor(source)) %>% 
  filter(pop_ord!="BMR") %>% 
  droplevels() %>% 
  split(.$pop_ord) %>% 
  map(function(df) t.test(linalool ~source , data = df))

combined_dataset %>% 
  count(source, pop_ord, lin_phenotype)

