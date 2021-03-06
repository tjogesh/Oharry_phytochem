# source('always_run/libraries.R')
# source('always_run/functions.R')
##---------------------------------------------
#greenhouse
##---------------------------------------------
GH_data<-read_excel("raw_data/O harry data for tania rar corr.xlsx", sheet="greenhouse 2008-09")

populations_gh<-GH_data%>% 
  slice((1)) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"Oharry")==T) %>% 
  select(name, pop=value) 

compounds_GH<-GH_data%>% 
  slice((2:42))%>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"Oharry")==T) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(compound, name) %>% 
  summarise(value_sum=sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=compound, 
              values_from=value_sum)  %>% 
  separate(name, into=c("Species","ID"), remove=F, sep=" ") %>% 
  rename_cols() %>% 
  left_join(populations_gh) %>% 
  mutate(source = "GH", year ="2008") %>% 
  select(-na)

##---------------------------------------------
#field 2008
##---------------------------------------------
Field_2008<-read_excel("raw_data/O harry data for tania rar corr.xlsx", sheet="CHALMAV2008")

compounds_2008<-Field_2008%>% 
  slice((2:38))%>% 
  select(-c("...16", "...17")) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(!is.na(compound)) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(compound, name) %>% 
  summarise(value_sum=sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=compound, 
              values_from=value_sum)  %>% 
  mutate(ID = str_remove_all(name, "[:alpha:]"),
         pop = str_remove_all(name, "[:digit:]")) %>% 
  rename_cols() %>% 
  mutate(source = "field",
         year = "2008") 
##---------------------------------------------
#field 2009
##---------------------------------------------
Field_2009<-read_excel("raw_data/O harry data for tania rar corr.xlsx", sheet="Oharry 2009")

populations_2009<-Field_2009%>% 
  slice((1)) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"oharry")==T) %>% 
  select(pop=value, name) 

# populations_2009 %>%  count()
# populations_2009 %>% distinct(name) %>% count()

compounds_2009<-Field_2009%>% 
  slice((2:38))%>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"oharry")==T) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(compound, name) %>% 
  summarise(value_sum=sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=compound, 
              values_from=value_sum)  %>% 
  separate(name, into=c("Species","ID"), remove=F, sep="-") %>% 
  mutate(Species="Oharry") %>% 
  rename_cols() %>% 
  left_join(populations_2009) %>% 
  mutate(source = "field", year = "2009") %>% 
  select(-na)
##---------------------------------------------
#field 2010
##---------------------------------------------
Field_2010<-read_excel("raw_data/O harry data for tania rar corr.xlsx", sheet="Oharry 2010")

populations_2010<-Field_2010%>% 
  slice((1)) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(!is.na(value)) %>% 
  select(pop=value, name) 

# populations_2010 %>% count()
# populations_2010 %>% distinct(name) %>% count()

compounds_2010<-Field_2010%>% 
  slice((2:38))%>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"^\\...")==F) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(compound, name) %>% 
  summarise(value_sum=sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=compound, 
              values_from=value_sum)  %>% 
  mutate(Species="Oharry", ID= name, year = "2010") %>% 
  rename_cols() %>% 
  left_join(populations_2010) %>% 
  mutate(source = "field") %>% 
  select(-na)

##---------------------------------------------
#field 2011
##---------------------------------------------
Field_2011<-read_excel("raw_data/O harry data for tania rar corr.xlsx", sheet="Oharry 2011")

populations_2011<-Field_2011%>% 
  slice((1)) %>% 
  mutate_all(~as.character(.)) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(!is.na(value)) %>% 
  select(pop=value, name) 

# populations_2011 %>% distinct(name) %>% count()
# populations_2011 %>% count()

compounds_2011<-Field_2011%>% 
  slice((2:38))%>% 
  mutate_all(~as.character(.)) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"^\\...")==F) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(compound, name) %>% 
  summarise(value_sum=sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=compound, 
              values_from=value_sum)  %>% 
  mutate(Species="Oharry", ID= name, year = "2011") %>% 
  rename_cols() %>%
  left_join(populations_2011) %>% 
  mutate(source = "field") %>% 
  select(-na)

##---------------------------------------------
#field 2012
##---------------------------------------------

Field_2012<-read_excel("raw_data/O harry data for tania rar corr.xlsx", sheet="corrected 2012")


compounds_2012<-Field_2012%>% 
  slice((2:38))%>% 
  mutate_all(~as.character(.)) %>% 
  pivot_longer(-c("compound", "ret time")) %>% 
  filter(str_detect(name,"^\\...")==F) %>% 
  mutate(value= as.numeric(value)) %>% 
  group_by(compound, name) %>% 
  summarise(value_sum=sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=compound, 
              values_from=value_sum)  %>% 
  mutate(ID = str_remove_all(name, "[:alpha:]"),
       pop = str_remove_all(name, "[:digit:]"),
       Species="Oharry",  year = "2012") %>% 
  rename_cols() %>% 
  mutate(source = "field") %>% 
  select(-na)

##----------------------------------------------
# combine all scent data together 
##----------------------------------------------

all_scent_data<-compounds_GH %>% 
  bind_rows(compounds_2008,
            compounds_2009,
            compounds_2010,
            compounds_2011,
            compounds_2012) %>% 
  #standardize population names
  mutate(pop=toupper(pop),
         pop_clean=case_when(
           pop=="BAC NE"~"BAC",
           pop=="CHAL"~"CHALMAV",
           pop=="CONE MT"~"CMT",
           pop=="HUERFANO"~"HUB",
           pop=="MAV"~"CHALMAV",
           pop=="PW MAU"~"PW",
           pop=="PW MCCULL"~"PW",
           pop=="PW PC"~"PW",
           pop=="PW QUINTO"~"PW",
           pop=="PW S"~"PW",
           pop=="PWPC"~"PW",
           str_detect(pop,"HEZRON")~"HZN",
           str_detect(pop,"ROUSE")~"ROU",
           str_detect(pop,"BERWIND")~"BER",
           TRUE~pop
         ))  %>% 
  mutate_all(~replace_na(., 0)) %>% 
  mutate(join_id = paste(source, year, pop_clean, trimws(id), sep="_")) %>% 
  rename(phenylethanol= `2_phenylethanol`)

##----------------------------------------------
# morphology data 
##----------------------------------------------
morph_cols<-c("floral_flare", 
              "herkogamy", 
              #"tube_length", 
              "tube_length1",
              "tube_length2",
              "nectar_length", 
              "corolla_diameter_1", 
              "corolla_diameter_2", 
              "sucrose_equivalents")

morph <- read_excel("raw_data/OharryFieldMorph20092012_KS_revised.xlsx", sheet="Sheet1") %>% 
  rename_cols() %>% 
  #standardize population names
  mutate(site_name=toupper(site_name),
         site_clean=case_when(
           str_detect(site_name,"HEZRON")~"HEZ",
           str_detect(site_name,"LUDLOW")~"LUD",
           str_detect(site_name,"TRINIDAD")~"TRIN",
           TRUE~site_name
         ))  %>% 
  mutate(join_id=paste("field", date, site_clean, vial_, sep="_")) %>% 
  #replce all handwritten nas
  mutate_at(c("nectar_length", "sucrose_equivalents"),~str_remove(.,"^\\.")) %>% 
  mutate_all(~na_if(.,"n/a")) %>% 
  mutate_all(~na_if(.,"na")) %>% 
  mutate_all(~na_if(.,"")) %>% 
  mutate(sucrose_equivalents = na_if(sucrose_equivalents, "Not enough nectar"),
         sucrose_equivalents = na_if(sucrose_equivalents, "too little")) %>% 
  #separate instances where two measurements for tueb length 
  separate(tube_length, into=c("tube_length1", "tube_length2"), sep="\\/") %>% 
  mutate_at(c("floral_flare", 
              "herkogamy", 
              "tube_length1", 
              "tube_length2", 
              "nectar_length",
              "corolla_diameter_1",
              "corolla_diameter_2",
              "sucrose_equivalents"), as.numeric) %>%  
  mutate(#use just one diameter where it is missing (id=158 MAV 2010)
    corolla_diam=(corolla_diameter_1+ corolla_diameter_2)/2,
    tube_length =ifelse(!is.na(tube_length2),  (tube_length1+ tube_length2)/2 , tube_length1)
  ) 

# all_scent_data%>% distinct(pop_clean) %>% arrange(pop_clean) %>% View("scent")
# morph %>% distinct(pop_clean) %>% arrange(pop_clean) %>% View('morph')

# all_scent_data %>% distinct(join_id)
# morph %>% distinct(join_id)

# read greenhouse morph data
greenhouse_morph<-read_excel("raw_data/OharryGreenhouseMorph20082009.xlsx") %>% 
  rename_cols() %>% 
  filter(!is.na(site_name)) %>% 
  mutate(vial_number=as.character(vial_number), 
         site_name=toupper(site_name),
         join_id=paste("GH", "2008", site_name, vial_number, sep="_"))  


##----------------------------------------------
# combine morph and scent
##----------------------------------------------
# all_scent_data %>% filter(join_id=="2009_MSR_71")
# scent_and_morph %>% filter(join_id=="2009_MSR_71")
# morph %>% filter(join_id=="2009_MSR_71")


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


