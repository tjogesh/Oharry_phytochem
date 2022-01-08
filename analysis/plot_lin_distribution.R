field_data %>% 
  mutate(linalool_PA = ifelse(linalool==0,T,F)) %>% 
  ggplot(aes(x=log(linalool+1), fill = linalool_PA)) +
  geom_histogram(color="#e9ecef", alpha=0.9) +
  ggtitle("Linalool distribution") +
  theme_bw() +
  theme(
    plot.title = element_text(size=15)
  )

field_data %>% 
  select(linalool, lin_phenotype, pop_ord) %>% 
  View()

field_data %>% 
  group_by(pop_ord, lin_phenotype) %>% 
  summarise(mean_lin_emission = mean(linalool, na.rm = T),
            se_lin= sd(linalool, na.rm = T)/sqrt(n()),
            ci=1.96*se_lin,
            med_lin_emission = median(linalool, na.rm = T)) %>% 
  mutate(lower=mean_lin_emission-se_lin, upper=mean_lin_emission+se_lin) %>% 
  ggplot() +
  geom_bar(aes(y=mean_lin_emission, x = pop_ord, fill=lin_phenotype),
           color="#e9ecef",  alpha=0.9, stat='identity') +
  geom_errorbar(aes(y = mean_lin_emission, x= pop_ord,
                    ymin = lower, ymax = upper), width = 0.2)+
  geom_point(aes(y=med_lin_emission, x = pop_ord),
             color="black",  size=3) +
  theme_bw() +
  ylab("Mean linalool emission rate +/- SE\n(ng / flower / hr)")+
  xlab("")+
  scale_fill_manual(values=c("#595E63", "lightgray"))+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/Lin_distribution.pdf", width = 14, height = 8, units = "in")

prop_emit<-field_data %>% 
  mutate(linalool_PA = ifelse(linalool==0,0,1)) %>% 
  group_by(pop_ord) %>% 
  summarise(prop_lin = sum(linalool_PA, na.rm = T)/n(), 
            total=n())

field_data %>% 
  filter(linalool!=0) %>% 
  group_by(pop_ord, lin_phenotype) %>% 
  summarise(mean_lin_emission = mean(linalool, na.rm = T),
            se_lin= sd(linalool, na.rm = T)/sqrt(n()),
            ci=1.96*se_lin,
            med_lin_emission = median(linalool, na.rm = T)) %>% 
  mutate(lower=mean_lin_emission-se_lin, upper=mean_lin_emission+se_lin) %>% 
  left_join(prop_emit) %>% 
  ggplot() +
  geom_bar(aes(y=mean_lin_emission, x = pop_ord, fill=lin_phenotype),
           color="#e9ecef",  alpha=0.9, stat='identity') +
  geom_text(aes(y=upper, x = pop_ord,  
                label = paste0( round(prop_lin, 3)*100 ,"%","\nn=" ,total ) ), vjust = -0.2)+
  geom_errorbar(aes(y = mean_lin_emission, x= pop_ord,
                    ymin = lower, ymax = upper), width = 0.2)+
  geom_point(aes(y=med_lin_emission, x = pop_ord),
             color="black",  size=3) +
  theme_bw() +
  ylab("Mean linalool emission rate +/- SE\n(ng / flower / hr)")+
  xlab("")+
  scale_fill_manual(values=c("#595E63", "lightgray"))+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/Lin_distribution_only_lin_plus.pdf", width = 16, height = 10, units = "in")
