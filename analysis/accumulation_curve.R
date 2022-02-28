accumulation_curve_df <- read_excel("raw_data/o harry cum curve.xlsx", 
                                  sheet = "by plant") %>% 
  rename_cols() 

accumulation_curve_df %>% 
  filter(!is.na(cum_count)) %>% 
  ggplot(aes(x = cum_count, y = cum_vocs)) +
  geom_line() +
  xlab("Cumulative Individual Plants Sampled") +
  ylab("Total VOCs") +
  theme_bw()+
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/accumulation_indv.pdf", width = 11, height = 8.5, units = "in")


accumulation_curve_df %>% 
  filter(!is.na(cum_count)) %>% 
  ggplot(aes(x = cum_pop, y = cum_vocs)) +
  geom_line() +
  xlab("Cumulative Populations Sampled") +
  ylab("Total VOCs") +
  theme_bw()+
  theme(panel.background = element_blank(),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        axis.line.x = element_line(color="black", size = 0.3),
        axis.line.y = element_line(color="black", size = 0.3),
        axis.text.y = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(2)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)))
ggsave("Figs/accumulation_pop.pdf", width = 11, height = 8.5, units = "in")
