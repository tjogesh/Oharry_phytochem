#convert to prop. of emissions
combined_dataset$Total <- rowSums(combined_dataset[,10:45])

combined_dataset_prop <- combined_dataset %>% 
  mutate_if(is.numeric, ~./Total) 
#


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

##**************NMDS PROP
library(ecodist)
Combo_prop.md <- distance(sqrt(combined_dataset_prop[,scent_cols]), "bray-curtis")
Combo_prop.nmds <- nmds(Combo_prop.md, mindim=2, maxdim=2, nits=3)
Combo_prop.nmin <- nmds.min(Combo_prop.nmds)


##**************PLot NMDS

Combo_prop.nmin$source <-combined_dataset_prop$source
Combo_prop.nmin$linalool_phenotype <-combined_dataset_prop$linalool_phenotype
Combo_prop.nmin$poly <-combined_dataset_prop$linalool_poly

Combo_prop.nmin$pop_ord <-combined_dataset_prop$pop_ord

ggplot(Combo_prop.nmin) + 
  geom_point(aes(x=X1, y=X2, fill=linalool_phenotype, shape=source), alpha=0.8, size=5)+
  #geom_text(aes(label=source),hjust=0.4, vjust=-1,size = rel(6))+
  labs(color = "Chemoytype\n", shape="Source\n") +
  scale_fill_manual(values = lin_palette) +
  scale_shape_manual( values=c(21, 23))+
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
data_lin_high = combined_dataset_prop %>% 
  filter(linalool_phenotype == "high") 

data_lin_high %>% 
  count(source)

data_lin_none = combined_dataset_prop %>% 
  filter(linalool_phenotype=="none") 

data_lin_none %>% 
  count(source)

data_lin_low = combined_dataset_prop %>% 
  filter(linalool_phenotype == "low") 

data_lin_low %>% 
  count(source)

get_anosim_gh <- function(.df) {
  com_plus <- sqrt(.df[,scent_cols])
  sour_plus <- factor(.df$source)
  dist_plus.com <- vegdist(com_plus, method = "bray")
  sourceDiff_plus <- anosim(dist_plus.com, sour_plus)
  return(sourceDiff_plus)
}

get_anosim_gh(data_lin_low)
get_anosim_gh(data_lin_high)
get_anosim_gh(data_lin_none)


stat.test <- combined_dataset %>%
  select(source, pop_ord, linalool) %>% 
  mutate(source = as.factor(source)) %>% 
  filter(pop_ord !="BMR") %>% 
  droplevels() %>% 
  split(.$pop_ord) %>% 
  map(function(df) kruskal.test(linalool ~source , data = df))


