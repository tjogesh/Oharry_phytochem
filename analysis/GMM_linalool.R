# Use a Gaussian mixture model to deliniate low vs high emitting linalool plants 
# model-based clustering method to seperate a bimodal distribution
# normalmixEM procedure for fitting normal mixture densities
#  iterative expectation maximization (EM) algorithm, 

p_load("mixtools")
set.seed(1)


log_linalool_df <- scent_and_morph %>% 
  filter(linalool > 0) %>% 
  mutate(log_linalool = log(linalool)) %>% 
  select(log_linalool, join_id)

log_linalool <- log_linalool_df %>% 
  pull(log_linalool)

mixmdl <- normalmixEM(log_linalool, k = 2)
mixmdl$mu
mixmdl$sigma
mixmdl$lambda
post.df <- as.data.frame(cbind(x = mixmdl$x, mixmdl$posterior))

log_linalool_df <- log_linalool_df %>% 
  bind_cols(post.df) %>% 
  mutate(linalool_phenotype = ifelse(comp.1 < 0.5, "high", "low")) %>% 
  select(join_id, linalool_phenotype, log_linalool)

#add the types back to main dataset
scent_and_morph <- scent_and_morph %>% 
  left_join(log_linalool_df) %>% 
  mutate(linalool_phenotype = ifelse(is.na(linalool_phenotype), "none", linalool_phenotype),
         error = ifelse(log(linalool) != log_linalool, T, F)) 

scent_and_morph %>% count(error)

scent_and_morph %>% 
  ggplot(aes(x=log(linalool+1), fill = linalool_phenotype)) +
  geom_histogram(color="black", alpha=0.9) +
  ggtitle("Linalool distribution") +
  scale_fill_manual(values=c( "#595E63", "lightgray", "white")) +
  theme_bw() +
  theme(
    plot.title = element_text(size=15)
  )+
  xlab("Log linalool emission rate +/- SE\n(ng / flower / hr)")

ggsave("Figs/field_linalool_distribution_hist.pdf", width = 11, height = 8.5, units = "in")

