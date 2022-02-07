#create bar plots of major phytochemical groups 
phytochem_lookup<-read_csv("raw_data/phytochem_lookup.csv") 

emit_by_grp_pop<-field_data %>% 
  select(pop_ord, scent_cols, join_id) %>% 
  pivot_longer(-c("pop_ord", "join_id"), names_to = "compound") %>% 
  left_join(phytochem_lookup) %>% 
  mutate(group=ifelse(compound=="linalool", "Linalool", group),
         group=ifelse(group=="Monoterpenoids", "Monoterpenoids\nnot Linalool", group)) %>% 
  #filter(compound!="linalool") %>% 
  filter(!is.na(group)) %>% 
  group_by(group, pop_ord, join_id) %>% 
  #calculate total per plant
  summarise(total_emission=sum(value, na.rm = T)) %>% 
  ungroup() 

emit_by_grp_pop%>% 
  group_by(group, pop_ord) %>% 
  summarise(mean_total_emission=mean(total_emission, na.rm = T), 
            se= sd(total_emission, na.rm = T)/sqrt(n()),
            ci =  se*1.96,
            lower=mean_total_emission-ci, 
            upper=mean_total_emission+ci) %>% 
  ggplot(aes(x=pop_ord, y=mean_total_emission))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(y = mean_total_emission, x= pop_ord,
                    ymin = lower, ymax = upper), width = 0.2)+
  facet_grid(group~., scales = "free_y")+
  #coord_flip()+
  theme_classic()+
  ylab("Mean emission rate +/- 95% CI\n(ng / flower / hr)")+
  xlab("")

ggsave("Figs/Emission_by_phyto_grp_pop.pdf", width = 11, height = 8.5, units = "in")

emit_by_grp_pop %>% 
  split(.$group) %>%
  map(~ kruskal.test(total_emission ~ pop_ord, data = .x)) 


# emit_by_grp_pop%>% 
#   ggplot()+
#   geom_histogram(aes(x=sqrt(total_emission)) )+
#   facet_grid(group~., scales = "free_y")
