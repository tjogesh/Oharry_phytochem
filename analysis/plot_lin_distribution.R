# summary table for Table 1
field_data %>% 
  group_by(pop_ord, linalool_phenotype) %>% 
  summarise(mean = mean(linalool, na.rm = T), 
            median =  median(linalool, na.rm = T),
            count = n()) %>% 
  pivot_wider(names_from = linalool_phenotype, 
              values_from = c("count", "median", "mean")) %>% 
  mutate(count_high = ifelse(is.na(count_high), 0, count_high),
         count_low = ifelse(is.na(count_low), 0, count_low),
         count_none = ifelse(is.na(count_none), 0, count_none)) %>% 
  left_join(field_data %>% group_by(pop_ord) %>% count(), by = "pop_ord") %>% 
  mutate(high_emitting_prop = count_high/n,
         low_emitting_prop = count_low/n,
         no_linalool_prop = count_none/n) %>% 
  write_csv("data_processed/summary_props.csv")
  

# histograms for all pops
field_data %>% 
  ggplot(aes(x = log(linalool + 1), fill = linalool_phenotype)) +
  geom_histogram(color = "black", alpha = 0.9) +
  facet_grid(pop_ord~., scales = "free_y") +
  scale_fill_manual(values = lin_palette) +
  ggtitle("Linalool distribution") +
  theme(
    plot.title = element_text(size = 15)
  ) +
  theme_classic() +
  xlab("Log linalool emission rate +/- SE\n(ng / flower / hr)")

ggsave("Figs/field_linalool_distribution_pop_hist.pdf", width = 11, height = 8.5, units = "in")


#pies for all pops
field_data %>% 
  group_by(pop_ord, linalool_phenotype) %>% 
  count() %>% 
  rename(num_lin = n) %>% 
  ungroup() %>% 
  left_join(field_data %>% group_by(pop_ord) %>% count(), by = "pop_ord") %>% 
  mutate(prop_lin = num_lin/n) %>% 
  ggplot(aes(x = "", y = prop_lin, fill = linalool_phenotype)) +
  geom_bar(stat = "identity", width = 0.3, color = "dark gray") +
  scale_fill_manual(values = lin_palette) +
  coord_polar("y", start = 0) +
  facet_wrap(vars(pop_ord)) +
  theme_void() 
ggsave("Figs/prop_lin_pie_pops.pdf", width = 11, height = 8.5, units = "in")



# mean linalool emission (archieved)
prop_emit <- field_data %>% 
  mutate(linalool_PA = ifelse(linalool == 0,0,1)) %>% 
  group_by(pop_ord) %>% 
  summarise(prop_lin = sum(linalool_PA, na.rm = T)/n(), 
            total = n())

field_data %>% 
  filter(linalool!= 0) %>% 
  group_by(pop_ord, lin_pop_phenotype) %>% 
  summarise(mean_lin_emission = mean(linalool, na.rm = T),
            se_lin= sd(linalool, na.rm = T)/sqrt(n()),
            ci=1.96*se_lin,
            med_lin_emission = median(linalool, na.rm = T)) %>% 
  mutate(lower=mean_lin_emission-se_lin, upper=mean_lin_emission+se_lin) %>% 
  left_join(prop_emit) %>% 
  ggplot() +
  geom_bar(aes(y=mean_lin_emission, x = pop_ord, fill=lin_pop_phenotype),
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
ggsave("Figs/Lin_distribution_only_lin_plus_bar.pdf", width = 16, height = 10, units = "in")
