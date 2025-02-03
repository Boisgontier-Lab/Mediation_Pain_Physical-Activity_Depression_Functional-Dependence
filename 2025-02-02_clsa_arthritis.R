### Load libraries 
# General utilities
library("tidyverse")  
library("here")      

# Data manipulation
library("lubridate")  
library("dplyr")  

# Statistical analysis
library("broom")    

# Visualization
library("ggplot2")    
library("viridis")  

# For the mediation models, load PROCESS script (https://www.processmacro.org/download.html) 
process_path <- here("scripts", "process.R")
system(paste("Rscript", process_path))

## Load and clean baseline data 
CLSA.Com.baseline <- read.csv(here("datasets", "2304007_UOttawa_MBoisgontier_BL","2304007_UOttawa_MBoisgontier_Baseline_CoPv7.csv"))
CLSA.Tra.baseline <- read.csv(here("datasets", "2304007_UOttawa_MBoisgontier_BL","2304007_UOttawa_MBoisgontier_Baseline_Trav4.csv"))
CLSA.baseline.raw <- merge(CLSA.Com.baseline, CLSA.Tra.baseline, all=TRUE)

CLSA.baseline <- CLSA.baseline.raw %>%
  dplyr::select(entity_id, startdate_COM, startdate_TRM, PA2_SIT_MCQ, PA2_WALK_MCQ, PA2_LSPRT_MCQ, PA2_MSPRT_MCQ, 
                PA2_SSPRT_MCQ, PA2_EXER_MCQ, PA2_LTHSWK_MCQ, PA2_HVYHSWK_MCQ, 
                PA2_HMREPAIR_MCQ, PA2_HVYODA_MCQ, PA2_LTODA_MCQ, PA2_CRPRSN_MCQ, 
                PA2_WRK_MCQ, HUP_FREE_MCQ,DEP_CESD10_COM, DEP_CESD10_TRM, ADL_DCLS_COM, ADL_DCLS_TRM, HUP_PRVACT_MCQ,
                CCC_RA_COM, CCT_RA_TRM, PA2_WALKHR_MCQ, PA2_LSPRTHR_MCQ, PA2_MSPRTHR_MCQ,
                PA2_SSPRTHR_MCQ, PA2_EXERHR_MCQ, GEN_DHDI_COM, GEN_DHDI_TRM,
                CCC_OAHAND_COM, CCT_OAHAND_TRM, CCC_OAHIP_COM, CCT_OAHIP_TRM, CCC_OAKNEE_COM,
                CCT_OAKNEE_TRM, CCC_ARTOT_COM, CCT_OTART_TRM, SEX_ASK_COM, SEX_ASK_TRM,
                AGE_GRP_COM, AGE_GRP_TRM, PA2_WRKHRS_NB_MCQ, 
                AGE_NMBR_COM, AGE_NMBR_TRM) %>%
  
  # coalesce columns from comprehensive (COM) and tracking (TRM) baseline surveys
  mutate(startdate_baseline = coalesce(startdate_COM, startdate_TRM), # to compute time difference between data collection waves
         DEP_CESD10 = coalesce(DEP_CESD10_COM, DEP_CESD10_TRM),
         ADL_DCLS = coalesce(ADL_DCLS_COM, ADL_DCLS_TRM),
         CCC_RA = coalesce(CCC_RA_COM, CCT_RA_TRM),
         CCC_OAHAND = coalesce(CCC_OAHAND_COM, CCT_OAHAND_TRM),
         CCC_OAHIP = coalesce(CCC_OAHIP_COM, CCT_OAHIP_TRM),
         CCC_OAKNEE = coalesce(CCC_OAKNEE_COM, CCT_OAKNEE_TRM),
         CCC_ARTOT = coalesce(CCC_ARTOT_COM, CCT_OTART_TRM),
         GEN_DHDI = coalesce(GEN_DHDI_COM, GEN_DHDI_TRM),
         SEX_ASK = coalesce(SEX_ASK_COM, SEX_ASK_TRM),
         AGE_GRP = coalesce(AGE_GRP_COM, AGE_GRP_TRM),
         AGE_NMBR = coalesce(AGE_NMBR_COM, AGE_NMBR_TRM)) %>%
  
  # replace non-response/I-dont-know values with NA
  mutate(PA2_SIT_MCQ = replace(PA2_SIT_MCQ, PA2_SIT_MCQ %in% c(8,9), NA),
         PA2_WALK_MCQ = replace(PA2_WALK_MCQ, PA2_WALK_MCQ %in% c(8,9), NA),
         PA2_LSPRT_MCQ = replace(PA2_LSPRT_MCQ, PA2_LSPRT_MCQ %in% c(8,9), NA),
         PA2_MSPRT_MCQ = replace(PA2_MSPRT_MCQ, PA2_MSPRT_MCQ %in% c(8,9), NA),
         PA2_SSPRT_MCQ  = replace(PA2_SSPRT_MCQ , PA2_SSPRT_MCQ  %in% c(8,9), NA),
         PA2_EXER_MCQ  = replace(PA2_EXER_MCQ , PA2_EXER_MCQ  %in% c(8,9), NA),
         PA2_LTHSWK_MCQ  = replace(PA2_LTHSWK_MCQ , PA2_LTHSWK_MCQ  %in% c(8,9), NA),
         PA2_HVYHSWK_MCQ  = replace(PA2_HVYHSWK_MCQ , PA2_HVYHSWK_MCQ  %in% c(8,9), NA),
         PA2_HMREPAIR_MCQ  = replace(PA2_HMREPAIR_MCQ , PA2_HMREPAIR_MCQ  %in% c(8,9), NA),
         PA2_HVYODA_MCQ  = replace(PA2_HVYODA_MCQ , PA2_HVYODA_MCQ  %in% c(8,9), NA),
         PA2_LTODA_MCQ  = replace(PA2_LTODA_MCQ , PA2_LTODA_MCQ  %in% c(8,9), NA),
         PA2_CRPRSN_MCQ  = replace(PA2_CRPRSN_MCQ , PA2_CRPRSN_MCQ  %in% c(8,9), NA),
         PA2_WRK_MCQ = replace(PA2_WRK_MCQ, PA2_WRK_MCQ %in% c(8,9), NA),
         HUP_FREE_MCQ = replace(HUP_FREE_MCQ, HUP_FREE_MCQ %in% c(8,9), NA),
         DEP_CESD10 = replace(DEP_CESD10, DEP_CESD10 %in% c(99,-88), NA),
         PA2_WALKHR_MCQ =  replace(PA2_WALKHR_MCQ , PA2_WALKHR_MCQ  %in% c(8,9), NA),
         PA2_LSPRTHR_MCQ =  replace(PA2_LSPRTHR_MCQ , PA2_LSPRTHR_MCQ  %in% c(8,9), NA),
         PA2_MSPRTHR_MCQ =  replace(PA2_MSPRTHR_MCQ , PA2_MSPRTHR_MCQ  %in% c(8,9), NA),
         PA2_SSPRTHR_MCQ =  replace(PA2_SSPRTHR_MCQ , PA2_SSPRTHR_MCQ  %in% c(8,9), NA),
         PA2_EXERHR_MCQ  =  replace(PA2_EXERHR_MCQ , PA2_EXERHR_MCQ  %in% c(8,9), NA),
         CCC_RA =  replace(CCC_RA , CCC_RA  %in% c(8,9), NA),
         CCC_OAHAND =  replace(CCC_OAHAND , CCC_OAHAND  %in% c(8,9), NA),
         CCC_OAHIP =  replace(CCC_OAHIP, CCC_OAHIP %in% c(8,9), NA),
         CCC_OAKNEE =  replace(CCC_OAKNEE , CCC_OAKNEE  %in% c(8,9), NA),
         CCC_ARTOT =  replace(CCC_ARTOT, CCC_ARTOT %in% c(8,9), NA),
         ADL_DCLS =  replace(ADL_DCLS, ADL_DCLS %in% c(9), NA),
         GEN_DHDI = replace(GEN_DHDI, GEN_DHDI %in% c(9), NA),
         HUP_PRVACT_MCQ =  replace(HUP_PRVACT_MCQ, HUP_PRVACT_MCQ %in% c(8,9), NA),
         PA2_WRKHRS_NB_MCQ =  replace(PA2_WRKHRS_NB_MCQ, PA2_WRKHRS_NB_MCQ %in% c(777, 998, 999), NA))

# recode PASE scale from 1-4 to 0-3 to align with scoring manual
# recode PASE Hrs/day to align with scoring (1/2 =1, 3 =2, 4 =3, 5 = 4)
CLSA.baseline <- CLSA.baseline %>%  mutate(
  PA2_SIT_MCQ = recode(PA2_SIT_MCQ, '1' = 0, '2' = 1, '3' = 2, '4' = 3),
  PA2_WALK_MCQ = recode(PA2_WALK_MCQ,'1' = 0, '2' = 1, '3' = 2, '4' = 3),
  PA2_LSPRT_MCQ = recode(PA2_LSPRT_MCQ ,'1' = 0, '2' = 1, '3' = 2, '4' = 3),
  PA2_MSPRT_MCQ = recode(PA2_MSPRT_MCQ,'1' = 0, '2' = 1, '3' = 2, '4' = 3),
  PA2_SSPRT_MCQ  = recode(PA2_SSPRT_MCQ,'1' = 0, '2' = 1, '3' = 2, '4' = 3),
  PA2_EXER_MCQ  = recode(PA2_EXER_MCQ,'1' = 0, '2' = 1, '3' = 2, '4' = 3),
  PA2_LTHSWK_MCQ  = recode(PA2_LTHSWK_MCQ ,'1' = 1, '2' = 0),
  PA2_HVYHSWK_MCQ  = recode(PA2_HVYHSWK_MCQ,'1' = 1, '2' = 0),
  PA2_HMREPAIR_MCQ  = recode(PA2_HMREPAIR_MCQ,'1' = 1, '2' = 0),
  PA2_HVYODA_MCQ  = recode(PA2_HVYODA_MCQ,'1' = 1, '2' = 0),
  PA2_LTODA_MCQ  = recode(PA2_LTODA_MCQ,'1' = 1, '2' = 0),
  PA2_CRPRSN_MCQ  = recode(PA2_CRPRSN_MCQ,'1' = 1, '2' = 0),
  PA2_WRK_MCQ = recode(PA2_WRK_MCQ,'1' = 1, '2' = 0),
  PA2_WALKHR_MCQ = recode(PA2_WALKHR_MCQ,'1' = 1, '2' = 1, '3' = 2, '4' = 3, '5' = 4),
  PA2_LSPRTHR_MCQ = recode(PA2_LSPRTHR_MCQ,'1' = 1, '2' = 1, '3' = 2, '4' = 3, '5' = 4),
  PA2_MSPRTHR_MCQ = recode(PA2_MSPRTHR_MCQ,'1' = 1, '2' = 1, '3' = 2, '4' = 3, '5' = 4),
  PA2_SSPRTHR_MCQ = recode(PA2_SSPRTHR_MCQ,'1' = 1, '2' = 1, '3' = 2, '4' = 3, '5' = 4),
  PA2_EXERHR_MCQ = recode(PA2_EXERHR_MCQ,'1' = 1, '2' = 1, '3' = 2, '4' = 3, '5' = 4),
  HUP_FREE_MCQ = recode (HUP_FREE_MCQ, '1' = 0, '2' = 1))

# create binary I/ADL classification for baseline and log transforming depression
# 0 is low I/ADL, 1 is high I/ADL 
# log transformation for depression 
CLSA.baseline <- CLSA.baseline %>% 
  mutate(Binary_IADL.ADL_baseline = if_else(ADL_DCLS %in% c(1, 2), 0, if_else(is.na(ADL_DCLS), NA_integer_, 1)),
         Binary_IADL.ADL2_baseline = if_else(ADL_DCLS %in% c(1), 0, if_else(is.na(ADL_DCLS), NA_integer_, 1)),
         log_trans_DEP_CESD10 = log(DEP_CESD10+1))

# Define a function to calculate weighted scores for different types of activities
calculate_weighted_score <- function(days, hours_per_day, weight) {
  # Conversion table based on days and hours per day
  conversion <- case_when(
    days == 0 ~ 0,
    days == 1 & hours_per_day == 1 ~ 0.11,
    days == 1 & hours_per_day == 2 ~ 0.32,
    days == 1 & hours_per_day == 3 ~ 0.64,
    days == 1 & hours_per_day == 4 ~ 1.07,
    days == 2 & hours_per_day == 1 ~ 0.25,
    days == 2 & hours_per_day == 2 ~ 0.75,
    days == 2 & hours_per_day == 3 ~ 1.5,
    days == 2 & hours_per_day == 4 ~ 2.5,
    days == 3 & hours_per_day == 1 ~ 0.43,
    days == 3 & hours_per_day == 2 ~ 1.29,
    days == 3 & hours_per_day == 3 ~ 2.57,
    days == 3 & hours_per_day == 4 ~ 4.29,
    TRUE ~ 0
  )
  return(conversion * weight)
}

# Calculate scores for each activity in clsa.baseline
CLSA.baseline <- CLSA.baseline %>%
  mutate(
    walk_score = calculate_weighted_score(PA2_WALK_MCQ, PA2_WALKHR_MCQ, 20),
    light_sport_score = calculate_weighted_score(PA2_LSPRT_MCQ, PA2_LSPRTHR_MCQ, 21),
    moderate_sport_score = calculate_weighted_score(PA2_MSPRT_MCQ, PA2_MSPRTHR_MCQ, 23),
    strenuous_sport_score = calculate_weighted_score(PA2_SSPRT_MCQ, PA2_SSPRTHR_MCQ, 23),
    muscle_strength_score = calculate_weighted_score(PA2_EXER_MCQ, PA2_EXERHR_MCQ, 30),
    light_housework_score = if_else(PA2_LTHSWK_MCQ == 1, 25, 0),
    heavy_housework_score = if_else(PA2_HVYHSWK_MCQ == 1, 25, 0),
    home_repairs_score = if_else(PA2_HMREPAIR_MCQ == 1, 30, 0),
    lawn_work_score = if_else(PA2_HVYODA_MCQ == 1, 36, 0),
    outdoor_gardening_score = if_else(PA2_LTODA_MCQ == 1, 20, 0),
    caring_for_another_score = if_else(PA2_CRPRSN_MCQ == 1, 35, 0),
    work_score = (PA2_WRKHRS_NB_MCQ / 7) * 21
  )

# Summing up all the scores to compute the total PASE score
CLSA.baseline <- CLSA.baseline %>%
  mutate(
    PASE_score = walk_score + light_sport_score + moderate_sport_score +
      strenuous_sport_score + muscle_strength_score + light_housework_score +
      heavy_housework_score + home_repairs_score + lawn_work_score +
      outdoor_gardening_score + caring_for_another_score + work_score
  )

## Load and clean follow-up2 data (I/ADL score)
CLSA.Com.FU2 <- read.csv(here("datasets", "2304007_UOttawa_MBoisgontier_FUP2","2304007_UOttawa_MBoisgontier_FUP2_CoPv1-1.csv"))
CLSA.Tra.FU2 <- read.csv(here("datasets", "2304007_UOttawa_MBoisgontier_FUP2","2304007_UOttawa_MBoisgontier_FUP2_Trav1-1.csv"))
CLSA.FU2 <- merge(CLSA.Com.FU2, CLSA.Tra.FU2, all=TRUE) 

# Select DVs (ADL/IADL) and ID ####
CLSA.FU2 <- CLSA.FU2 %>%
  select(entity_id, startdate_COF2, startdate_TRF2, ADL_ABLDR_COF2, ADL_HPDR_COF2, ADL_UNDR_COF2, ADL_ABLFD_COF2, 
         ADL_HPFD_COF2, ADL_UNFD_COF2, ADL_ABLAP_COF2, ADL_HPAP_COF2, ADL_UNAP_COF2,
         ADL_ABLWK_COF2, ADL_HPWK_COF2, ADL_UNWK_COF2, ADL_ABLBD_COF2, ADL_HPBD_COF2,
         ADL_UNBD_COF2, ADL_ABLBT_COF2, ADL_HPBT_COF2, ADL_UNBT_COF2, ADL_BATH_COF2,
         ADL_INCNT_COF2, IAL_ABLTEL_COF2, IAL_HPTEL_COF2, IAL_UNTEL_COF2, 
         IAL_ABLTRV_COF2, IAL_HPTRV_COF2, IAL_UNTRV_COF2, IAL_ABLGRO_COF2, 
         IAL_HPGRO_COF2, IAL_UNGRO_COF2, IAL_ABLML_COF2, IAL_HPML_COF2, IAL_UNML_COF2,
         IAL_ABLWRK_COF2, IAL_HPWRK_COF2, IAL_UNWRK_COF2, IAL_ABLMED_COF2, 
         IAL_HPMED_COF2, IAL_UNMED_COF2, IAL_ABLMO_COF2, IAL_HPMO_COF2, IAL_UNMO_COF2,
         ADL_ABLDR_TRF2, ADL_HPDR_TRF2, ADL_UNDR_TRF2, ADL_ABLFD_TRF2, ADL_HPFD_TRF2, 
         ADL_UNFD_TRF2, ADL_ABLAP_TRF2, ADL_HPAP_TRF2, ADL_UNAP_TRF2, ADL_ABLWK_TRF2, 
         ADL_HPWK_TRF2, ADL_UNWK_TRF2, ADL_ABLBD_TRF2, ADL_HPBD_TRF2, ADL_UNBD_TRF2, 
         ADL_ABLBT_TRF2, ADL_HPBT_TRF2, ADL_UNBT_TRF2, ADL_BATH_TRF2, ADL_INCNT_TRF2, 
         IAL_ABLTEL_TRF2, IAL_HPTEL_TRF2, IAL_UNTEL_TRF2, IAL_ABLTRV_TRF2, 
         IAL_HPTRV_TRF2, IAL_UNTRV_TRF2, IAL_ABLGRO_TRF2, IAL_HPGRO_TRF2, 
         IAL_UNGRO_TRF2, IAL_ABLML_TRF2, IAL_HPML_TRF2, IAL_UNML_TRF2, IAL_ABLWRK_TRF2, 
         IAL_HPWRK_TRF2, IAL_UNWRK_TRF2, IAL_ABLMED_TRF2, IAL_HPMED_TRF2, 
         IAL_UNMED_TRF2, IAL_ABLMO_TRF2, IAL_HPMO_TRF2, IAL_UNMO_TRF2)%>%
  
  # coalesce columns from comprehensive (COM) and tracking (TRM) follow-up surveys
  mutate(startdate_FU2 = coalesce(startdate_COF2, startdate_TRF2),
         ADL_ABLDR = coalesce(ADL_ABLDR_COF2, ADL_ABLDR_TRF2), 
         ADL_HPDR = coalesce(ADL_HPDR_COF2, ADL_HPDR_TRF2), 
         ADL_UNDR = coalesce(ADL_UNDR_COF2, ADL_UNDR_TRF2), 
         ADL_ABLFD = coalesce(ADL_ABLFD_COF2, ADL_ABLFD_TRF2), 
         ADL_HPFD = coalesce(ADL_HPFD_COF2, ADL_HPFD_TRF2), 
         ADL_UNFD = coalesce(ADL_UNFD_COF2, ADL_UNFD_TRF2), 
         ADL_ABLAP = coalesce(ADL_ABLAP_COF2, ADL_ABLAP_TRF2), 
         ADL_HPAP = coalesce(ADL_HPAP_COF2, ADL_HPAP_TRF2), 
         ADL_UNAP = coalesce(ADL_UNAP_COF2,ADL_UNAP_TRF2),
         ADL_ABLWK = coalesce(ADL_ABLWK_COF2, ADL_ABLWK_TRF2), 
         ADL_HPWK = coalesce(ADL_HPWK_COF2, ADL_HPWK_TRF2), 
         ADL_UNWK = coalesce(ADL_UNWK_COF2, ADL_UNWK_TRF2), 
         ADL_ABLBD = coalesce(ADL_ABLBD_COF2, ADL_ABLBD_TRF2), 
         ADL_HPBD = coalesce(ADL_HPBD_COF2,ADL_HPBD_TRF2),
         ADL_UNBD = coalesce(ADL_UNBD_COF2, ADL_UNBD_TRF2), 
         ADL_ABLBT = coalesce(ADL_ABLBT_COF2, ADL_ABLBT_TRF2), 
         ADL_HPBT = coalesce(ADL_HPBT_COF2, ADL_HPBT_TRF2), 
         ADL_UNBT = coalesce(ADL_UNBT_COF2, ADL_UNBT_TRF2), 
         ADL_BATH = coalesce(ADL_BATH_COF2,ADL_BATH_TRF2),
         ADL_INCNT = coalesce(ADL_INCNT_COF2, ADL_INCNT_TRF2), 
         IAL_ABLTEL = coalesce(IAL_ABLTEL_COF2,IAL_ABLTEL_TRF2), 
         IAL_HPTEL = coalesce(IAL_HPTEL_COF2, IAL_HPTEL_TRF2), 
         IAL_UNTEL = coalesce(IAL_UNTEL_COF2, IAL_UNTEL_TRF2), 
         IAL_ABLTRV = coalesce(IAL_ABLTRV_COF2,IAL_ABLTRV_TRF2), 
         IAL_HPTRV = coalesce(IAL_HPTRV_COF2, IAL_HPTRV_TRF2), 
         IAL_UNTRV = coalesce(IAL_UNTRV_COF2, IAL_UNTRV_TRF2), 
         IAL_ABLGRO = coalesce(IAL_ABLGRO_COF2,IAL_ABLGRO_TRF2), 
         IAL_HPGRO = coalesce(IAL_HPGRO_COF2, IAL_HPGRO_TRF2), 
         IAL_UNGRO = coalesce(IAL_UNGRO_COF2, IAL_UNGRO_TRF2), 
         IAL_ABLML = coalesce(IAL_ABLML_COF2, IAL_ABLML_TRF2), 
         IAL_HPML = coalesce(IAL_HPML_COF2, IAL_HPML_TRF2), 
         IAL_UNML = coalesce(IAL_UNML_COF2,IAL_UNML_TRF2),
         IAL_ABLWRK = coalesce(IAL_ABLWRK_COF2,IAL_ABLWRK_TRF2), 
         IAL_HPWRK = coalesce(IAL_HPWRK_COF2, IAL_HPWRK_TRF2), 
         IAL_UNWRK = coalesce(IAL_UNWRK_COF2, IAL_UNWRK_TRF2), 
         IAL_ABLMED = coalesce(IAL_ABLMED_COF2,IAL_ABLMED_TRF2), 
         IAL_HPMED = coalesce(IAL_HPMED_COF2, IAL_HPMED_TRF2), 
         IAL_UNMED = coalesce(IAL_UNMED_COF2, IAL_UNMED_TRF2), 
         IAL_ABLMO = coalesce(IAL_ABLMO_COF2, IAL_ABLMO_TRF2), 
         IAL_HPMO = coalesce(IAL_HPMO_COF2, IAL_HPMO_TRF2), 
         IAL_UNMO = coalesce(IAL_UNMO_COF2, IAL_UNMO_TRF2)) %>%
  
  select(entity_id, startdate_FU2, ADL_ABLDR ,ADL_HPDR  ,ADL_UNDR  ,ADL_ABLFD  ,ADL_HPFD  ,
         ADL_UNFD  ,ADL_ABLAP  ,ADL_HPAP  ,ADL_UNAP  ,ADL_ABLWK  ,ADL_HPWK  ,
         ADL_UNWK  ,ADL_ABLBD  ,ADL_HPBD  ,ADL_UNBD  ,ADL_ABLBT  ,ADL_HPBT  ,
         ADL_UNBT  ,ADL_BATH  ,ADL_INCNT  ,IAL_ABLTEL  ,IAL_HPTEL  ,IAL_UNTEL  ,
         IAL_ABLTRV ,IAL_HPTRV  ,IAL_UNTRV  ,IAL_ABLGRO  ,IAL_HPGRO  ,IAL_UNGRO  ,
         IAL_ABLML  ,IAL_HPML  ,IAL_UNML  ,IAL_ABLWRK  ,IAL_HPWRK  ,IAL_UNWRK  ,
         IAL_ABLMED ,IAL_HPMED  ,IAL_UNMED  ,IAL_ABLMO  ,IAL_HPMO  ,IAL_UNMO)  


# remove placeholder values (e.g., -9999), and create summary items 
# format missing variables
CLSA.FU2  <- CLSA.FU2  %>%
  mutate(across(where(is.numeric), ~na_if(., -99999))) %>% 
  mutate(across(where(is.character), ~na_if(., "-99999")))

# OARS Scale: Number of Missing Items (Excluding Meal Preparation) Variable Name: ADL_NBRMIS
# Description: This variable counts the total number of daily activities, both basic and instrumental,
# except for meal preparation, for which the respondent did not provide an answer of their ability to 
# perform the task.

# ADL_NBRMIS == Number of missing items, excluding meal preparation.
CLSA.FU2 <- CLSA.FU2 %>%
  mutate(ADL_IMDR = if_else(ADL_ABLDR == 1 | ADL_HPDR == 1 | ADL_UNDR == 1, 0, 1),
         ADL_IMFD = if_else(ADL_ABLFD == 1 | ADL_HPFD == 1 | ADL_UNFD == 1, 0, 1),
         ADL_IMAP = if_else(ADL_ABLAP == 1 | ADL_HPAP == 1 | ADL_UNAP == 1, 0, 1),
         ADL_IMWK = if_else(ADL_ABLWK== 1 | ADL_HPWK == 1 | ADL_UNWK == 1, 0, 1),
         ADL_IMBD = if_else(ADL_ABLBD == 1 | ADL_HPBD == 1 | ADL_UNBD == 1, 0, 1),
         ADL_IMBT = if_else(ADL_ABLBT == 1 | ADL_HPBT == 1 | ADL_UNBT == 1, 0, 1),
         ADL_IMBATH = if_else(
           (ADL_BATH != 1 & ADL_BATH != 2) | (ADL_BATH == 1 & !ADL_INCNT %in% c(1, 2, 3)), 1,
           if_else(ADL_BATH == 1 & ADL_INCNT %in% c(1, 2, 3) | ADL_BATH == 2, 0, NA_real_)),
         ADL_IMTEL = if_else(IAL_ABLTEL == 1 | IAL_HPTEL == 1 | IAL_UNTEL == 1, 0, 1),
         ADL_IMTRV = if_else(IAL_ABLTRV == 1 | IAL_HPTRV == 1 | IAL_UNTRV == 1, 0, 1),
         ADL_IMGRO = if_else(IAL_ABLGRO == 1 | IAL_HPGRO == 1 | IAL_UNGRO == 1, 0, 1),
         ADL_IMWRK = if_else(IAL_ABLWRK == 1 | IAL_HPWRK == 1 | IAL_UNWRK == 1, 0, 1),
         ADL_IMMED = if_else(IAL_ABLMED == 1 | IAL_HPMED == 1 | IAL_UNMED == 1, 0, 1),
         ADL_IMMO = if_else(IAL_ABLMO == 1 | IAL_HPMO == 1 | IAL_UNMO == 1, 0, 1))

# List of variables to sum
variables_to_sum <- c("ADL_IMDR", "ADL_IMFD", "ADL_IMAP", "ADL_IMWK",
                      "ADL_IMBD", "ADL_IMBT", "ADL_IMBATH", "ADL_IMTEL", "ADL_IMTRV",
                      "ADL_IMGRO", "ADL_IMWRK", "ADL_IMMED", "ADL_IMMO")

# Filter the list to include only those variables present in the dataset
existing_vars_to_sum <- variables_to_sum[variables_to_sum %in% names(CLSA.FU2)]

# Create the new variable 'ADL_NBRMIS'
CLSA.FU2 <- CLSA.FU2 %>%
  mutate(ADL_NBRMIS = rowSums(select(., all_of(existing_vars_to_sum)), na.rm = TRUE))

# OARS Scale: Some or Complete Dependence for Meal Preparation - Intermediate Derived Variable
# Derived Variable Name: ADL_DMEA_TRM
# Description: This variable indicates whether a respondent is able to prepare their own meals. The 
# authors of the OARS instrument determined that the inability to prepare one’s own meals without help 
# is more detrimental to independent living than all other activities of daily living and hence, the meal 
# preparation component of the questionnaire should be considered separately when determining 
# functional capacity (2). 

# Create the new variable 'ADL_DMEA'
CLSA.FU2 <- CLSA.FU2 %>%
  mutate(ADL_DMEA = case_when(
    IAL_ABLML == 1 ~ 0,
    IAL_HPML == 1 | IAL_UNML == 1 ~ 1,
    TRUE ~ 9
  ))

CLSA.FU2 %>% group_by(ADL_DMEA) %>% summarise(count=n())

# OARS scale: Sum of Some Dependence and Complete Dependence (Excluding Meal                                                                Preparation) – Temporary Variable
# Derived Variable Name: ADL_TDSUM_TRM
# Description: This variable calculates the total number of times the respondent indicated that they need 
# help with an activity or that they are completely unable to do an activity in the ADL and IAL modules, 
# excluding the questions regarding meal preparation ability (IAL_ABLML_TRM, IAL_HPML_TRM, and 
# IAL_UNML_TRM). It ignores missing values and cannot be interpreted without considering the number 
# of missing items, ADL_NBRMIS_TRM. This variable in not provided in the CLSA dataset

CLSA.FU2 <- CLSA.FU2 %>%
  mutate(
    ADL_THPDR = case_when(ADL_HPDR == 1 ~ 1,TRUE ~ 0),
    ADL_TUNDR = case_when(ADL_UNDR == 1 ~ 1,TRUE ~ 0),
    ADL_THPFD = case_when(ADL_HPFD == 1 ~ 1,TRUE ~ 0),
    ADL_TUNFD = case_when(ADL_UNFD == 1 ~ 1,TRUE ~ 0),
    ADL_THPAP = case_when(ADL_HPAP == 1 ~ 1,TRUE ~ 0),
    ADL_TUNAP = case_when(ADL_UNAP == 1 ~ 1,TRUE ~ 0),
    ADL_THPWK = case_when(ADL_HPWK == 1 ~ 1,TRUE ~ 0),
    ADL_TUNWK = case_when(ADL_UNWK == 1 ~ 1,TRUE ~ 0),
    ADL_THPBD = case_when(ADL_HPBD == 1 ~ 1,TRUE ~ 0),
    ADL_TUNBD = case_when(ADL_UNBD == 1 ~ 1,TRUE ~ 0),
    ADL_THPBT = case_when(ADL_HPBT == 1 ~ 1,TRUE ~ 0),
    ADL_TUNBT = case_when(ADL_UNBT == 1 ~ 1,TRUE ~ 0),
    ADL_TINCNT = case_when(ADL_INCNT %in% c(2, 3) ~ 1,TRUE ~ 0),
    IAL_THPTEL = case_when(IAL_HPTEL == 1 ~ 1,TRUE ~ 0),
    IAL_TUNTEL = case_when(IAL_UNTEL == 1 ~ 1,TRUE ~ 0),
    IAL_THPTRV = case_when(IAL_HPTRV == 1 ~ 1,TRUE ~ 0),
    IAL_TUNTRV = case_when(IAL_UNTRV == 1 ~ 1,TRUE ~ 0),
    IAL_THPGRO = case_when(IAL_HPGRO == 1 ~ 1,TRUE ~ 0),
    IAL_TUNGRO = case_when(IAL_UNGRO == 1 ~ 1,TRUE ~ 0),
    IAL_THPWRK = case_when(IAL_HPWRK == 1 ~ 1,TRUE ~ 0),
    IAL_TUNWRK = case_when(IAL_UNWRK == 1 ~ 1,TRUE ~ 0),
    IAL_THPMED = case_when(IAL_HPMED == 1 ~ 1,TRUE ~ 0),
    IAL_TUNMED = case_when(IAL_UNMED == 1 ~ 1,TRUE ~ 0),
    IAL_THPMO = case_when(IAL_HPMO == 1 ~ 1,TRUE ~ 0),
    IAL_TUNMO = case_when(IAL_UNMO == 1 ~ 1,TRUE ~ 0))

# List of variables to sum
variables_to_sum <- c("ADL_THPDR", "ADL_TUNDR", "ADL_THPFD", "ADL_TUNFD", "ADL_THPAP", 
                      "ADL_TUNAP", "ADL_THPWK", "ADL_TUNWK", "ADL_THPBD", "ADL_TUNBD",
                      "ADL_THPBT", "ADL_TUNBT", "ADL_TINCNT", "IAL_THPTEL", "IAL_TUNTEL",
                      "IAL_THPTRV", "IAL_TUNTRV", "IAL_THPGRO", "IAL_TUNGRO",
                      "IAL_THPWRK", "IAL_TUNWRK", "IAL_THPMED", "IAL_TUNMED", "IAL_THPMO",
                      "IAL_TUNMO") 

# Filter the list to include only those variables present in the dataset
existing_vars_to_sum <- variables_to_sum[variables_to_sum %in% names(CLSA.FU2)]

# Create the new variable 'ADL_TDSUM_TRM'
CLSA.FU2 <- CLSA.FU2%>%
  mutate(ADL_TDSUM = rowSums(select(., all_of(existing_vars_to_sum)), na.rm = TRUE))

# OARS scale: Basic and Instrumental Activities of Daily Living Classification (Excluding Meal Preparation) – Intermediate Derived Variable
# Derived Variable Name: ADL_DCLST_TRM
# Description: This variable categorizes the respondent’s ability to perform activities of daily living based 
# on the number of times they indicated that they need help with an activity or that they are completely 
# unable to do an activity in the ADL and IAL modules, excluding the questions regarding meal preparation 
# ability (IAL_ABLML_TRM, IAL_HPML_TRM, and IAL_UNML_TRM). The classification is done according to 
# the instructions provided in Table 11 of the OARS manual (2). However, the CLSA has augmented the 
# classification conditions to also take account of the number of missing items. If the possible values of 
# those missing items would not change the results of a classification, then the classification can be made. 
# Hence, some participants with missing items who would have otherwise had a missing classification can 
# be classified. The classification values range from 0 (no problems performing activities of daily living) to 
# 4 (complete inability in performing daily activities).

CLSA.FU2 <- CLSA.FU2 %>%
  mutate(
    ADL_DCLST = case_when(
      ADL_TDSUM == 0 & ADL_NBRMIS == 0 ~ 0,
      ADL_TDSUM %in% 1:3 & ADL_NBRMIS == 0 | ADL_TDSUM %in% 1:2 & ADL_NBRMIS == 1 | ADL_TDSUM == 1 & ADL_NBRMIS == 2 ~ 1,
      ADL_TDSUM %in% 4:5 & ADL_NBRMIS == 0 | ADL_TDSUM == 4 & ADL_NBRMIS == 1 ~ 2,
      ADL_TDSUM %in% 6:7 & ADL_NBRMIS == 0 | ADL_TDSUM == 6 & ADL_NBRMIS == 1 ~ 3,
      ADL_TDSUM %in% 8:13 ~ 4,
      TRUE ~ 9
    )
  )

# OARS scale: Basic and Instrumental Activities of Daily Living Classification
# Derived Variable Name: ADL_DCLS_TRM
# Description: This variable is an overall classification of a respondent’s capacity to perform activities of 
# daily living. It is based on the 5-point scale described in Table 11 of the OARS manual (2), which ranges 
# from 2 (Excellent/Good) to 6 (Total Impairment). The CLSA has modified this scale so that the range is 
# now 1 to 5, and the first category “Excellent/Good” has been renamed “No functional impairment”. 
# Higher values indicate greater impairment. This variable combines the functional status classification 
# determined from all activities of daily living excluding meal preparation (ADL_DCLST_TRM) with the 
# meal preparation indicator (ADL_DMEA_TRM), creating an overall classification of functional capacity 
# that assigns extra weight to the meal preparation component.

CLSA.FU2 <- CLSA.FU2 %>%
  mutate(
    ADL_DCLS_F2 = case_when(
      ADL_DMEA == 0 & ADL_DCLST == 0 ~ 1,
      ADL_DMEA == 0 & ADL_DCLST == 1 ~ 2,
      (ADL_DMEA == 1 & ADL_DCLST %in% c(0, 1)) | ADL_DCLST == 2 ~ 3,
      ADL_DCLST == 3 ~ 4,
      ADL_DCLST == 4 ~ 5,
      TRUE ~ NA_real_
    )
  )

# OARS scale: Sum of Some Dependence and Complete Dependence (Excluding Meal Preparation)
# Derived Variable Name: ADL_DSUM_TRM
# Description: This variable calculates the total number of times the respondent indicated that they need 
# help with an activity or that they are completely unable to do an activity in the ADL and IAL modules, 
# excluding the questions regarding meal preparation ability (IAL_ABLML_TRM, IAL_HPML_TRM, and 
# IAL_UNML_TRM).If there are any missing items, it too is missing. However, it distinguishes between 
# participants with missing items who have provided sufficient information for a Basic and Instrumental 
# Activities of Daily Living Classification and those who have not.

CLSA.FU2 <- CLSA.FU2 %>%
  mutate(
    ADL_DSUM_F2 = case_when(
      ADL_NBRMIS == 0 ~ ADL_TDSUM,
      ADL_NBRMIS > 0 & ADL_DCLST == 9 ~ NA_real_,
      ADL_NBRMIS > 0 & ADL_DCLST != 9 ~ NA_real_
    )
  )

list (CLSA.FU2$startdate_FU2) 

# selecting only three specific columns of follow-up dataset
CLSA.FU2 <- CLSA.FU2 %>%
  dplyr::select(entity_id, ADL_DCLS_F2, startdate_FU2) 

# merge FU2 variables to baseline dataset 
CLSA.baseline.FU2 <- full_join(CLSA.baseline, CLSA.FU2, by = "entity_id")

# create binary I/ADL classification
# 0 is low I/ADL, 1 is high I/ADL 
# Binary_FU2_IADL.1 is 1-2 vs 3-5
# Binary_FU2_IADL.2 is 1 vs 2-5
# log transformation for depression 
CLSA.baseline.FU2 <- CLSA.baseline.FU2 %>% 
  mutate(Binary_FU2_IADL.1 = if_else(ADL_DCLS_F2 %in% c(1, 2), 0, if_else(is.na(ADL_DCLS_F2), NA_integer_, 1)),
         Binary_FU2_IADL.2 = if_else(ADL_DCLS_F2 %in% c(1), 0, if_else(is.na(ADL_DCLS_F2), NA_integer_, 1)))

# Convert Factor Sex (F/M) to Numeric (0/1)
str(CLSA.baseline.FU2$SEX_ASK)
CLSA.baseline.FU2$sex_numeric <- ifelse(CLSA.baseline.FU2$SEX_ASK == "M", 1, 0) #  0 = Female, 1 = Male

# Remove NAs
CLSA.baseline.FU2.subset <- subset(CLSA.baseline.FU2, !is.na(Binary_FU2_IADL.1) & !is.na(HUP_FREE_MCQ)
                     & !is.na(log_trans_DEP_CESD10) & !is.na(PASE_score)  & !is.na(AGE_NMBR) & !is.na(sex_numeric) & !is.na(Binary_IADL.ADL_baseline))
nrow(CLSA.baseline.FU2) # n = 51338

### filter out by arthritis type 
# Any arthritis
CLSA.baseline.arthritis <- CLSA.baseline.FU2.subset %>% filter(CCC_RA == 1 | CCC_OAHAND == 1 |
                                                          CCC_OAHIP == 1 | CCC_OAKNEE == 1 |
                                                          CCC_ARTOT == 1)
nrow(CLSA.baseline.arthritis) # n = 6972

# Osteoarthritis
CLSA.baseline.osteo <- CLSA.baseline.FU2.subset %>% filter(CCC_OAHAND == 1 |CCC_OAHIP == 1 | CCC_OAKNEE == 1)
nrow(CLSA.baseline.osteo) # n = 4930

# Rheumatoid Arthritis
CLSA.baseline.ra <- CLSA.baseline.FU2.subset %>% filter(CCC_RA ==1)
nrow(CLSA.baseline.ra) # n = 694

### Time difference between baseline and follow-up
# Convert baseline and follow-up dates to POSIXct format
CLSA.baseline.arthritis <- CLSA.baseline.arthritis %>%
  mutate(
    startdate_baseline = ymd_hms(startdate_baseline, tz = "UTC"),
    startdate_FU2 = ymd_hms(startdate_FU2, tz = "UTC"),
    time_diff_days = as.numeric(difftime(startdate_FU2, startdate_baseline, units = "days")),
    time_diff_years = time_diff_days / 365.25  # Convert days to years
  )

# Compute summary statistics
summary_stats <- CLSA.baseline.arthritis %>%
  summarise(
    Mean_Days = mean(time_diff_days, na.rm = TRUE),
    SD_Days = sd(time_diff_days, na.rm = TRUE),
    Min_Days = min(time_diff_days, na.rm = TRUE),
    Max_Days = max(time_diff_days, na.rm = TRUE),
    Mean_Years = mean(time_diff_years, na.rm = TRUE),
    SD_Years = sd(time_diff_years, na.rm = TRUE),
    Min_Years = min(time_diff_years, na.rm = TRUE),
    Max_Years = max(time_diff_years, na.rm = TRUE)
  )

print(summary_stats)

### Standardization
CLSA.baseline.arthritis$AGE_NMBR_c <- scale (CLSA.baseline.arthritis$AGE_NMBR, center = TRUE, scale = TRUE)
CLSA.baseline.arthritis$PASE_score_c <- scale (CLSA.baseline.arthritis$PASE_score, center = TRUE, scale = TRUE)
CLSA.baseline.arthritis$log_trans_DEP_CESD10_c <- scale (CLSA.baseline.arthritis$log_trans_DEP_CESD10, center = TRUE, scale = TRUE)

CLSA.baseline.osteo$AGE_NMBR_c <- scale (CLSA.baseline.osteo$AGE_NMBR, center = TRUE, scale = TRUE)
CLSA.baseline.osteo$PASE_score_c <- scale (CLSA.baseline.osteo$PASE_score, center = TRUE, scale = TRUE)
CLSA.baseline.osteo$log_trans_DEP_CESD10_c <- scale (CLSA.baseline.osteo$log_trans_DEP_CESD10, center = TRUE, scale = TRUE)

CLSA.baseline.ra $AGE_NMBR_c <- scale (CLSA.baseline.ra $AGE_NMBR, center = TRUE, scale = TRUE)
CLSA.baseline.ra $PASE_score_c <- scale (CLSA.baseline.ra $PASE_score, center = TRUE, scale = TRUE)
CLSA.baseline.ra $log_trans_DEP_CESD10_c <- scale (CLSA.baseline.ra $log_trans_DEP_CESD10, center = TRUE, scale = TRUE)

### Descriptive statistics


#arthritis
hist(CLSA.baseline.arthritis$PASE_score)
hist(CLSA.baseline.arthritis$DEP_CESD10)
hist(CLSA.baseline.arthritis$HUP_FREE_MCQ)
hist(CLSA.baseline.arthritis$ADL_DCLS)
hist(CLSA.baseline.arthritis$Binary_FU2_IADL.1)
hist(CLSA.baseline.arthritis$Binary_FU2_IADL.2)

CLSA.baseline.arthritis %>% group_by(SEX_ASK) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.arthritis %>% group_by(AGE_GRP) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.arthritis %>% group_by(ADL_DCLS) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.arthritis %>% group_by(HUP_FREE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

mean(CLSA.baseline.arthritis$PASE_score, na.rm=T)
sd(CLSA.baseline.arthritis$PASE_score, na.rm=T)
mean(CLSA.baseline.arthritis$DEP_CESD10, na.rm=T)
sd(CLSA.baseline.arthritis$DEP_CESD10, na.rm=T)

#osteoarthritis
hist(CLSA.baseline.osteo$PASE_score)
hist(CLSA.baseline.osteo$DEP_CESD10)
hist(CLSA.baseline.osteo$HUP_FREE_MCQ)
hist(CLSA.baseline.osteo$ADL_DCLS)
hist(CLSA.baseline.osteo$Binary_FU2_IADL.1) # 1-2 vs 3-5
hist(CLSA.baseline.osteo$Binary_FU2_IADL.2) # 

CLSA.baseline.osteo %>% group_by(SEX_ASK) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.osteo %>% group_by(AGE_GRP) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.osteo %>% group_by(ADL_DCLS) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.osteo %>% group_by(HUP_FREE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

mean(CLSA.baseline.osteo$PASE_score, na.rm=T)
sd(CLSA.baseline.osteo$PASE_score, na.rm=T)
mean(CLSA.baseline.osteo$DEP_CESD10, na.rm=T)
sd(CLSA.baseline.osteo$DEP_CESD10, na.rm=T)

#rheumatoid arthritis
hist(CLSA.baseline.ra$PASE_score)
hist(CLSA.baseline.ra$DEP_CESD10)
hist(CLSA.baseline.ra$HUP_FREE_MCQ)
hist(CLSA.baseline.ra$ADL_DCLS)
hist(CLSA.baseline.ra$Binary_FU2_IADL.1) # 1-2 vs 3-5
hist(CLSA.baseline.ra$Binary_FU2_IADL.2) # 1 vs 2-5

CLSA.baseline.ra %>% group_by(SEX_ASK) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.ra %>% group_by(AGE_GRP) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.ra %>% group_by(ADL_DCLS) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
CLSA.baseline.ra %>% group_by(HUP_FREE_MCQ) %>% summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

mean(CLSA.baseline.ra$PASE_score, na.rm=T)
sd(CLSA.baseline.ra$PASE_score, na.rm=T)
mean(CLSA.baseline.ra$DEP_CESD10, na.rm=T)
sd(CLSA.baseline.ra$DEP_CESD10, na.rm=T)






####  Statistical Analyses  ####
### Main Analyses
# Serial Mediation
m1.1 <- process(data = CLSA.baseline.arthritis,
        y = "Binary_FU2_IADL.1", #functional_limitations
        x = "HUP_FREE_MCQ", #pain
        m = c("log_trans_DEP_CESD10_c","PASE_score_c"), #depressive_symptoms
        cov = c("sex_numeric", "AGE_NMBR_c", "Binary_IADL.ADL_baseline"), 
        model = 6)

# Depression 
m1.2.1 <- lm(log_trans_DEP_CESD10_c ~ HUP_FREE_MCQ + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.arthritis)
summary(m1.2.1)
tidy(m1.2.1, conf.int = TRUE, conf.level = 0.95)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 4, nsmall = 4)))

# Physical activity
m1.2.2 <- lm(PASE_score_c ~ HUP_FREE_MCQ + log_trans_DEP_CESD10_c + AGE_NMBR_c + sex_numeric
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.arthritis)
summary(m1.2.2)
tidy(m1.2.2, conf.int = TRUE, conf.level = 0.95)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 4, nsmall = 4)))

# Functional status (without mediators)
m1.3.1 <- glm(Binary_FU2_IADL.1 ~ HUP_FREE_MCQ + AGE_NMBR_c + sex_numeric 
            + Binary_IADL.ADL_baseline, family = "binomial", data = CLSA.baseline.arthritis)
summary(m1.3.1)
tidy(m1.3.1, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

# Functional status (with mediators)
m1.3.2 <- glm(Binary_FU2_IADL.1 ~ HUP_FREE_MCQ + log_trans_DEP_CESD10_c + PASE_score_c + AGE_NMBR_c + sex_numeric 
            + Binary_IADL.ADL_baseline, family = "binomial", data = CLSA.baseline.arthritis)
summary(m1.3.2)
tidy(m1.3.2, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

### Sensitivity Analyses
## 1) all  arthritis - no vs mild/moderate/sever (Binary_FU2_IADL.2)
# Serial Mediation
m2.1 <- process(data = CLSA.baseline.arthritis,
        y = "Binary_FU2_IADL.2", #functional_limitations
        x = "HUP_FREE_MCQ", #pain
        m = c("log_trans_DEP_CESD10_c","PASE_score_c"), #depressive_symptoms
        cov = c("sex_numeric", "AGE_NMBR_c", "Binary_IADL.ADL_baseline"), 
        model = 6)

# lm to obtain p-value in scientific notation
m2.2.1 <- lm(log_trans_DEP_CESD10_c ~ HUP_FREE_MCQ + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.arthritis)
summary(m2.2.1)
tidy(m2.2.1, conf.int = TRUE, conf.level = 0.95)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

m2.2.2 <- lm(PASE_score_c ~ HUP_FREE_MCQ + log_trans_DEP_CESD10_c + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.arthritis)
summary(m2.2.2)
tidy(m2.2.2, conf.int = TRUE, conf.level = 0.95)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

# glm to obtain exponentiated log odds and p-value in scientific notation
m2.3 <- glm(Binary_FU2_IADL.2 ~  HUP_FREE_MCQ + log_trans_DEP_CESD10_c + PASE_score_c + AGE_NMBR_c 
            + sex_numeric + Binary_IADL.ADL_baseline, family = "binomial", data = CLSA.baseline.arthritis)
tidy(m2.3, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

## 2) osteoarthritis - no/mild vs moderate/sever (Binary_FU2_IADL.1)
# Serial Mediation
m3.1 <- process(data = CLSA.baseline.osteo,
                y = "Binary_FU2_IADL.1", #functional_limitations
                x = "HUP_FREE_MCQ", #pain
                m = c("log_trans_DEP_CESD10_c","PASE_score_c"), #depressive_symptoms
                cov = c("sex_numeric", "AGE_NMBR_c", "Binary_IADL.ADL_baseline"), 
                model = 6)

# lm to obtain p-value in scientific notation
m3.2.1 <- lm(log_trans_DEP_CESD10_c ~ HUP_FREE_MCQ + PASE_score_c + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.osteo)
summary(m3.2.1)
tidy(m3.2.1, conf.int = TRUE, conf.level = 0.95)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

m3.2.2 <- lm(PASE_score_c ~ HUP_FREE_MCQ + log_trans_DEP_CESD10_c + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.osteo)
summary(m3.2.2)
tidy(m3.2.2, conf.int = TRUE, conf.level = 0.95)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

# glm to obtain exponentiated log odds and p-value in scientific notation
m3.3 <- glm(Binary_FU2_IADL.1 ~  HUP_FREE_MCQ + log_trans_DEP_CESD10_c + PASE_score_c + AGE_NMBR_c 
            + sex_numeric + Binary_IADL.ADL_baseline, family = "binomial", data = CLSA.baseline.osteo)
summary(m3.3)
tidy(m3.3, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)%>%
  mutate(across(where(is.numeric), ~ format(.x, digits = 3, nsmall = 3)))

## 3) rheumatoid arthritis no/mild vs moderate/sever (Binary_FU2_IADL.1)
# Serial Mediation
m4.1 <- process(data = CLSA.baseline.ra,
        y = "Binary_FU2_IADL.1", #functional_limitations,FUL_TOTAL
        x = "HUP_FREE_MCQ", #pain
        m = c("log_trans_DEP_CESD10_c","PASE_score_c"), #depressive_symptoms
        cov = c("sex_numeric", "AGE_NMBR_c"), 
        model = 6)

# lm to obtain p-value in scientific notation
m4.2.1 <- lm(log_trans_DEP_CESD10_c ~  HUP_FREE_MCQ + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.ra)
summary(m4.2.1)
tidy(m4.2.1, conf.int = TRUE, conf.level = 0.95)

m4.2.2 <- lm(PASE_score_c ~ HUP_FREE_MCQ + log_trans_DEP_CESD10_c + AGE_NMBR_c + sex_numeric 
             + Binary_IADL.ADL_baseline, data = CLSA.baseline.ra)
summary(m4.2.2)
tidy(m4.2.2, conf.int = TRUE, conf.level = 0.95)

# glm to obtain exponentiated log odds and p-value in scientific notation
m4.3 <- glm(Binary_FU2_IADL.1 ~ HUP_FREE_MCQ + log_trans_DEP_CESD10_c + PASE_score_c + AGE_NMBR_c 
            + sex_numeric + Binary_IADL.ADL_baseline, family = "binomial", data = CLSA.baseline.ra)
summary(m4.3)
tidy(m4.3, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)

### Figure
# Create data frame for indirect effects
data <- data.frame(
  Group = rep(c("Total", "Depressive Symptoms", "Physical Activity", "Serial Pathway"), each = 4),
  Condition = rep(c("All Arthritis 1", "All Arthritis 2", "Osteoarthritis", "Rheumatoid Arthritis"), times = 4),
  Estimate = c(0.184, 0.152, 0.203, 0.250,  # Total indirect effect
               0.128, 0.120, 0.139, 0.185,  # Depressive symptoms
               0.050, 0.028, 0.055, 0.055,  # Physical activity
               0.006, 0.004, 0.009, 0.010), # Serial pathway
  Lower_CI = c(0.110, 0.117, 0.113, 0.049, 
               0.066, 0.089, 0.062, 0.013, 
               0.019, 0.013, 0.016, -0.089, 
               0.001, 0.001, 0.002, -0.013),
  Upper_CI = c(0.267, 0.189, 0.307, 0.574, 
               0.200, 0.154, 0.227, 0.453, 
               0.090, 0.046, 0.109, 0.226, 
               0.013, 0.007, 0.019, 0.038)
)

# Plot using ggplot2 with Viridis color scale and setting factor order in scale_x_discrete
ggplot(data, aes(x = Group, y = Estimate, ymin = Lower_CI, ymax = Upper_CI, color = Condition)) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.2) +  # Smaller points
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +  # Add reference line at 0
  labs(title = "Indirect Effects of Baseline Pain on Functional Status",
       y = "Estimate (95% CI)", x = "Indirect Pathway", color = "Condition") +
  scale_color_viridis(discrete = TRUE) +  # Apply Viridis color palette
  scale_x_discrete(limits = c("Total", "Depressive Symptoms", "Physical Activity", "Serial Pathway")) +  # Control order
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank()) # Remove vertical grid lines for clarity
