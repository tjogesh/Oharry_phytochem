# functions.R - Any functions made in the process of running script


# Function 1
# my_function <- function(x){
#   print('wowza')
# }

rename_cols<-function(df){
  df %>% 
    rename_all(function(.name){
      .name %>% 
        tolower %>% 
        str_replace_all(" ", "_") %>% 
        str_replace_all("-", "_") %>% 
        str_replace_all("[:punct:]","_")
    })
}



read_csvs_folder <- function(path, pattern = "*.csv") {
  list.files(path, pattern, full.names = TRUE) %>% 
    map_df(~read_csv(.))
}

#read in all as characters
read_csvs_folder_c <- function(path, pattern = "*.csv") {
  list.files(path, pattern, full.names = TRUE) %>% 
    map_df(~read_csv(. , col_types = cols(.default = "c")))
}

