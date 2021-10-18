# process data and calculate scent emit rates 
source('always_run/libraries.R')
scent_and_morph<-read_csv('data_processed/scent_morph.csv')

#check for outliers 
morph_cols<-c("floral_flare", 
              "herkogamy", 
              "tube_length", 
              "nectar_length", 
              "corolla_diam", 
              "sucrose_equivalents")

scent_cols<-scent_and_morph[,4:41] %>% 
  select(-c("tic_peak_area","toluene_int_std")) %>% 
  colnames()

field_data %>% 
  skim(morph_cols)

field_data %>% 
  select(morph_cols) %>% 
  plot_histogram()

field_data %>% 
  select(dry_flower_weight) %>% View()
  plot_histogram()
  
  field_data %>% 
    select(morph_cols, pop_ord) %>% 
    plot_boxplot(by="pop_ord")

field_data %>% 
  select(tic_peak_area) %>% 
  plot_histogram()

field_data %>% 
  select(tic_peak_area, pop_ord) %>% 
  plot_boxplot(by="pop_ord")

field_data %>% 
  select(scent_cols) %>% 
  plot_histogram()

field_data %>% 
  select(scent_cols) %>% 
  skim_without_charts() %>% 
  select(numeric.p100, skim_variable) %>%
  arrange(desc(numeric.p100)) %>% 
  top_n(5)

field_data %>% 
  select(tic_peak_area) %>% 
  plot_histogram()

field_data %>% skim()
field_data %>% filter(is.na(dry_flower_weight)) %>% View()
#two flowers have no dry flower weight and are missing. 

gh_data %>% skim()
#missing all morphology data (?)
