
# loads the most recent IPC  data and cleans strings

path_to_folder <- here::here("data")
bases <- file.info(list.files(path_to_folder,full.names = TRUE))
path_to_base <- rownames(bases)[which.max(bases$mtime)]
data <- rio::import(path_to_base) %>% 
  linelist::clean_data(guess_dates = F)

data_c <- data %>% 
  fill(date_saisie1, csb_rattach,visite_zd,hameau,date_visite1,statut_mng1,
       consent_signe1, result_final1, statut_mng2, consent_signe2, result_final2, 
       statut_mng3, consent_signe3, result_final3, materiau_toit,
       .direction = "down")



hh_fact <- data_c %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  mutate(across(.cols = everything(), ~na_if(.x,""))) %>% 
  discard(~all(is.na(.))) %>% 
  select(where(is.numeric)) %>% 
  select(where(is_whole)) %>% 
  names() %>% 
  discard(function(x) str_detect(x, "(?<!men)age"))

hh <- data_c %>% 
  filter(is.na(redcap_repeat_instance)) %>% 
  mutate(across(.cols = everything(), ~na_if(.x,""))) %>% 
  discard(~all(is.na(.))) %>% 
  mutate(across(contains('date'), ~ lubridate::ymd(.)), 
         across(all_of(hh_fact), ~factor(.)), 
         across(contains('heure'), ~ lubridate::hm(str_replace(., "_", ":"))),
         gps_latitude = as.numeric(str_replace(gps_latitude, "_", ".")))


indiv_fact <- data_c %>% 
  filter(!is.na(redcap_repeat_instance)) %>% 
  mutate(across(.cols = everything(), ~na_if(.x,""))) %>% 
  discard(~all(is.na(.))) %>% 
  select(where(is.numeric)) %>% 
  select(where(is_whole)) %>% 
  names() %>% 
  discard(function(x) str_detect(x, "(?<!men)age"))

indiv <- data_c %>% 
  filter(!is.na(redcap_repeat_instance)) %>% 
  mutate(across(.cols = everything(), ~na_if(.x,""))) %>% 
  discard(~all(is.na(.))) %>% 
  mutate(across(contains('date'), ~ lubridate::ymd(.)), 
         across(all_of(indiv_fact), ~factor(.)), 
         across(contains('heure'), ~ lubridate::hm(str_replace(., "_", ":")))) %>% 
  select(-c(nom_indiv, prenom_indiv))


rm(hh_fact, indiv_fact)
