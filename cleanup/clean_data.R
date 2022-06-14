##----------------------------------------------
# combine morph and scent
##----------------------------------------------
greenhouse_morph <- read_csv("data_cleaned/greenhouse_morph.csv")
morph <- read_csv("data_cleaned/morph.csv")
all_scent_data <- read_csv("data_cleaned/all_scent_data.csv")
phytochem_lookup <- read_csv("data_cleaned/phytochem_lookup.csv") 
pairwiseFst<-read_csv('data_cleaned/pairwiseFst.csv')


scent_and_morph <- all_scent_data %>% 
  left_join(morph) %>% 
  mutate(pop_ord = fct_relevel(pop_clean, c("CC",
                                            "FLO",
                                            "GG",
                                            "MSR",
                                            "PW",
                                            "CHALMAV",
                                            "BAC",
                                            "SB",
                                            "HUB",
                                            "WALS",
                                            "BMR",
                                            "HZN",
                                            "ROU",
                                            "MONS",
                                            "LUD",
                                            "BER",
                                            "TRIN",
                                            "CMT",
                                            "BLOOM",
                                            "DC"
  ))
  )


##----------------------------------------------
# add linalool phenotype
##----------------------------------------------

lin_pops<-c("MSR",
            "GG",
            "FLO",
            "CC",
            "SB",
            "BMR",
            "HUB",
            "PW",
            "CHALMAV",
            "WALS",
            "BAC")

scent_and_morph <- scent_and_morph %>% 
  mutate(lin_pop_phenotype=ifelse(pop_clean %in% lin_pops, "linalool", "no linalool"))

source("analysis/GMM_linalool.R")
##----------------------------------------------
# calculate toluene equivalents
##----------------------------------------------

morph_cols<-c("floral_flare", 
              "herkogamy", 
              "tube_length", 
              "nectar_length", 
              "corolla_diam", 
              "sucrose_equivalents")

scent_cols<-scent_and_morph[,4:41] %>% 
  select(-c("tic_peak_area","toluene_int_std")) %>% 
  colnames()

# To express any TIC peak as a toluene equivalent: 						
#   x ng VOC / peak area VOC = 23.6 ng toluene / peak area toluene…. Rearranges algebraically to x ng VOC = peak area VOC/ peak area toluene X 23.6 ng
# 1. integrate the TIC peak area					
# 2. divide by the peak area for the toluene internal standard (usually about 7M counts in our Dimensions samples) (normalizing by toluene)					
# 3. multiply by 23.6 ng (converting to toluene equivalent)					
# 4. multiply by 55 uL (total volume of scent sample)					
# 5. Now you have the total ng of VOC in the scent sample, expressed as ng / flower / hr (both = 1 in our data)				
# calc_tol_equiv<-function(compound){
#   (compound*23.4*55)/1/1
# }

calc_tol_equiv<-function(compound, tol_peak){
  ((compound/tol_peak)*23.4*55)/1/1
}

gh_data<-scent_and_morph %>% 
  filter(source=="GH") %>% 
  mutate_at(scent_cols, ~calc_tol_equiv(., toluene_int_std)) %>% 
  mutate(tic_peak_area=calc_tol_equiv(tic_peak_area, toluene_int_std)) 

field_data<-scent_and_morph %>% 
  filter(!year==2008) %>% 
  filter(source=="field") %>% 
  mutate_at(scent_cols, ~calc_tol_equiv(., toluene_int_std)) %>% 
  mutate_at(scent_cols, ~(.)/dry_flower_weight) %>% 
  mutate(tic_peak_area=calc_tol_equiv(tic_peak_area, toluene_int_std)) %>% 
  mutate(tic_peak_area=(tic_peak_area)/dry_flower_weight)

# scent_and_morph %>% 
#   write_csv("data_processed/scent_morph.csv")

field_data %>% 
  count(is.na(tic_peak_area))

field_data %>% 
  filter(is.na(tic_peak_area))

#remove where no total peak area
field_data %>% 
  filter(!is.na(tic_peak_area)) %>% 
  count(pop_ord, lin_pop_phenotype, year) %>% 
  pivot_wider(names_from = year, values_from=n) %>% 
  write_csv('data_processed/pop_year_samples.csv', na = "")

#field data not adjusted for dry wt of flower
unadjusted_field_data<-scent_and_morph %>% 
  filter(!year==2008) %>% 
  filter(source=="field") %>% 
  mutate_at(scent_cols, ~calc_tol_equiv(., toluene_int_std)) %>% 
  mutate(tic_peak_area=calc_tol_equiv(tic_peak_area, toluene_int_std)) 


#------
#greenhouse scent data

greenhouse_data <- gh_data %>% 
  select(source, name, pop_ord, year, linalool_phenotype, lin_pop_phenotype, species, id, all_of(scent_cols), tic_peak_area, join_id) %>% 
  #mutate(join_id=paste0(pop_ord,"_",id)) %>% 
  left_join(greenhouse_morph) %>% 
  mutate(plant_id=ifelse(plant_id=="B57-12-2", "B 57-12-2", plant_id),
         plant_id=ifelse(plant_id=="BMR-30-9-1", "BMR 30-9-1", plant_id)) %>% 
  separate(plant_id, into=c("pop_morph", "parental_id"), sep=(" "), remove=F) %>% 
  separate(parental_id, into=c("mat_id", "flw_id", "rep"), sep=("-"), remove=F)


# green house data consists of replicates so need to filter to one sample per plant
single_mat_lin_gh<-greenhouse_data %>% 
  group_by(site_name, mat_id) %>% 
  filter(flw_id==min(flw_id, na.rm=T)) %>% 
  filter(date==min(date, na.rm=T)) %>% 
  filter(time==min(time, na.rm=T)) %>% 
  ungroup()

single_mat_lin_gh %>% distinct(site_name, mat_id) %>% count() == single_mat_lin_gh %>% count()


single_mat_lin_gh %>% 
  count(pop_ord, lin_pop_phenotype) %>% 
  write_csv('data_processed/pop_year_samples_gh.csv')

pops_in_gh<-c("BAC","FLO","PW","DC", "BMR", "BLOOM")

combined_dataset<-unadjusted_field_data %>% 
  select(source, join_id, name, pop_ord, year, linalool_phenotype, lin_pop_phenotype, species, id, all_of(scent_cols), tic_peak_area) %>% 
  filter(pop_ord %in% pops_in_gh) %>% 
  bind_rows(single_mat_lin_gh %>% 
              select(source, join_id, name, pop_ord, year, linalool_phenotype, lin_pop_phenotype, species, id, all_of(scent_cols), tic_peak_area)) %>%
  mutate(linalool_poly= ifelse(linalool>0, 'lin+', 'lin-')) %>% 
  na.omit()  
