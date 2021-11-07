
# loads the most recent survey data and cleans strings

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

Hmisc::describe(indiv$csb_rattach)

fct_label <- function(x,
                      label) {
  
#  if (!is.factor(x))
#    stop("Not a factor variable")
  
  attr(x, "label") <- label
  
  x
  
}


code1 <- rio::import("exitinterview.xlsx", which = "choices") %>% 
  linelist::clean_variable_names()

codebook <- rio::import("codebook.xlsx")
rosetta_stone <- rio::import("exitinterview.xlsx", which = "survey") %>% 
  filter(str_detect(type, "select_one") | str_detect(type, "select_multiple")) %>% 
  select(name, type) %>% 
  mutate(type = str_remove(type, "select_one "), 
         type = str_remove(type, "select_multiple "))

dummy <- code1 %>% 
  right_join(rosetta_stone, by = c("list_name" = "type")) %>% 
  distinct(list_name, name.x, name.y, .keep_all = T) %>% 
  arrange(name.y) %>% 
  rename_with(~ "french", .cols = contains("fr")) %>% 
  rename_with(~ "english", .cols = contains("eng"))


facade <- function(df, langauge){
  df1 <- matchmaker::match_df(df,
             dictionary = dummy,
             from = "name.x",
             to = {{langauge}},
             by = "list_name")
  return(df1)
}

get_codes <- function(df, var, language){
  df1 <- df %>% 
    select(var) %>% 
    mutate(var1 = var) %>% 
    matchmaker::match_df(.,
                       dictionary = dummy,
                       from = "name.x",
                       to = {{langauge}},
                       by = "list_name") %>% 
    mutate(out = paste0(var1, " - ", var))
  return(df1$out)
}

trial <- rio::import("data_example.xlsx")

a <- names(indiv) %>% 
  as_tibble()

dummy <- indiv %>% 
  select(csb_rattach) %>% 
  fct_label(., codebook$label)



levels(dummy$csb_rattach)

dummy2 <- dummy %>% 
  mutate(csb_rattach = lfactor(csb_rattach, levels = 1:37, 
                               labels = codebook$label), 
         csb_rattach = fct_relevel(csb_rattach, "ee", "z"))

dummy3 <- dummy2 %>% 
  filter(csb_rattach == 26)

llevels(dummy2$csb_rattach)

attr(dummy2$csb_rattach, "label")

ggplot(dummy2, aes(x = csb_rattach)) + 
  geom_bar()


rm(hh_fact, indiv_fact)
