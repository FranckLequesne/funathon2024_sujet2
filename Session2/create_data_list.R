# Ex 1
create_data_list <- function(source_file){
  catalogue <- yaml::read_yaml(source_file)
  return(catalogue)
}

urls <- create_data_list("sources.yml")

getwd()

# Ex 2

# Données aéroports

library(dplyr)
library(tidyr)
library(stringr)
tableau_airport <- readr::read_csv2(unlist(urls$airports))
tableau_airport_test <- tableau_airport %>% 
  mutate(
    an = str_sub(ANMOIS,1,4),
    mois = str_sub(ANMOIS,5,6),
    mois= str_remove(mois,"^0+")) %>% # recherche le(s) caractère(s) à supprimer
  rename_with(tolower)

# Fonction clean_data_frame

clean_dataframe <- function(df){
  data <- data %>% 
    mutate(
      an = str_sub(ANMOIS,1,4),
      mois = str_sub(ANMOIS,5,6),
      mois= str_remove(mois,"^0+")) %>% # recherche le(s) caractère(s) à supprimer
    rename_with(tolower)
  
  return(data)
         }
  

import_airport_data <- function(list_files){
  
  pax_apt_all <- readr::read_csv2(
    list_files, 
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_apt_all)
  
}

list_files <- unlist(urls$airport)

import_airport_data(list_files)
# Données compagnies aériennes

tableau_compagnies <- readr::read_csv2(unlist(urls$compagnies),
                                       col_types = cols(
                                         ANMOIS = col_character(),
                                         CIE = col_character(),
                                         CIE_NOM = col_character(),
                                         CIE_NAT = col_character(),
                                         CIE_PAYS = col_character(),
                                         .default = col_double()
                                       ))

import_compagnies_data <- function(list_files){
  
  pax_cie_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      CIE = col_character(),
      CIE_NOM = col_character(),
      CIE_NAT = col_character(),
      CIE_PAYS = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_cie_all)
  
  
}

import_airport_data <- function(list_files){
  
  pax_apt_all <- readr::read_csv2(
    list_files, 
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_apt_all)
  
}

# Données sur les liaisons

import_liaisons_data <- function(list_files){
  
  pax_lsn_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),
      .default = col_double()
    ) 
  ) %>% 
    clean_dataframe()
  
  return(pax_lsn_all)
  
  
}

# Construction de fichiers de fonctions

