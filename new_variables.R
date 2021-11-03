

hh1 <- hh %>% 
  mutate(complete = if_else((statut_mng1==1 & consent_signe1==1 & result_final1 ==1)|
                               (statut_mng2==1 & consent_signe2==1 & result_final2 ==1)), 
                             1,0), 
          status = if_else(visite_du_menage_complete==2, "complete", 
                           "non_complete"), 
          )
  
  
names(hh)




indiv1 <- indiv %>% 
  mutate(compo_id = paste0(menage_id,"_",redcap_repeat_instance),
         age_yr = ifelse(!is.na(age_correct), age_correct, age_indiv),
         ageclass2 = cut(age_yr, breaks = c(0, 15, Inf),
                         labels = c("under_15", "15+"), 
                         right = F),
         ageclass3 = cut(age_yr, breaks = c(0, 5, 15, Inf),
                        labels = c("under_5", "5_14", "15+"), 
                        right = F),
         ageclass4 = cut(age_yr, breaks = c(0, 2, 5, 15, Inf),
                        labels = c("under_2", "2_4, 5_14", "15+"), 
                        right = F),
         ageclass5 = cut(age_yr, breaks = c(0, 2, 5, 10, 15, Inf),
                         labels = c("under_2", "2_4, 5_9", "10_14", "15+"), 
                         right = F),
         sexe_indiv = recode(sexe_indiv, '1' = "male", '2' = "female"), 
         



fever = factor(ifelse(
  malad_dern_sem ==1, "Febrile", 
  ifelse((malad_dern_sem==0|malad_dern_sem==""), "Non_febrile", 
         NA))),
respiratory_sym = factor(ifelse((malade_avc_toux==1)|(difficult_resp==1), 1, 
                                ifelse(is.na(malade_avc_toux) & 
                                         is.na(difficult_resp), NA, 0))),
respiratory_sym = factor(ifelse(malade_indiv==1 & Age>=60 & 
                                  is.na(respiratory_sym),98,respiratory_sym)),
diarrhea = factor(ifelse(diarh_dern_sem==1, 1,
                         ifelse(is.na(diarh_dern_sem), NA, 0))),
diarrhea = factor(ifelse(malade_indiv==1 & Age>=60 & is.na(diarrhea), 98, 
                         diarrhea)),
ill = factor(ifelse(
  malade_indiv ==1, "Ill", 
  ifelse(malade_indiv==0, "Well", 
         "NSP"))),
tdrpalu_realise_mod = tdrpalu_realise,
tdrpalu_realise = recode(tdrpalu_realise, '1' = "Tested", '0' = 
                           "Not Tested"), 
enf_scolarise = recode(enf_scolarise, '1' = "In School", '0'="Not"),
valeur_tdr1pos = ifelse(as.numeric(resultat_tdr1)==0, 0, valeur_tdr1pos),
any_malaria = ifelse(as.numeric(resultat_tdr1)==1, 1, 0),
falciparum = ifelse((as.numeric(resultat_tdr1)==1) &((valeur_tdr1pos==111)|
                                                       (valeur_tdr1pos==101)),
                    1, 0), 
falcip_only = ifelse((as.numeric(resultat_tdr1)==1) &(valeur_tdr1pos==101),
                     1, 0),
other_sp = ifelse((as.numeric(resultat_tdr1)==1) &((valeur_tdr1pos==111)|
                                                     (valeur_tdr1pos==110)),
                  1, 0), 
other_only = ifelse((as.numeric(resultat_tdr1)==1) &(valeur_tdr1pos==110),
                    1, 0), 
mix = ifelse((as.numeric(resultat_tdr1)==1) &(valeur_tdr1pos==111),
             1, 0),

)
  

  