
# 1) load libraries ####
library(tidyverse)  # general
#note: download and run process script (https://www.processmacro.org/download.html) 

# 2) load data ####
CLSA.Com.baseline <-read_csv("2304007_UOttawa_MBoisgontier_Baseline_CoPv7.CSV")
CLSA.Tra.baseline <-read_csv("2304007_UOttawa_MBoisgontier_Baseline_Trav4.CSV")
CLSA.baseline.raw <- merge(CLSA.Com.baseline, CLSA.Tra.baseline, all=TRUE)

CLSA.baseline <- CLSA.baseline.raw %>%
  dplyr::select(entity_id, PA2_SIT_MCQ, PA2_WALK_MCQ, PA2_LSPRT_MCQ, PA2_MSPRT_MCQ, 
         PA2_SSPRT_MCQ, PA2_EXER_MCQ, PA2_LTHSWK_MCQ, PA2_HVYHSWK_MCQ, 
         PA2_HMREPAIR_MCQ, PA2_HVYODA_MCQ, PA2_LTODA_MCQ, PA2_CRPRSN_MCQ, 
         PA2_WRK_MCQ, HUP_FREE_MCQ,DEP_CESD10_COM, DEP_CESD10_TRM,  FUL_SHLD_COM,
         FUL_STOOP_COM, FUL_PUSH_COM, FUL_LFT10_COM, FUL_HDLG_COM, FUL_ST15_COM,
         FUL_SIT1H_COM, FUL_STDUP_COM, FUL_FSTR_COM, FUL_WK23B_COM, FUL_MKBED_COM,
         FUL_WSHBK_COM, FUL_KNCUT_COM, FUL_FORC_COM, FUL_SHLD_TRM, FUL_STOOP_TRM,
         FUL_PUSH_TRM, FUL_LFT10_TRM, FUL_HDLG_TRM, FUL_ST15_TRM, FUL_SIT1H_TRM,
         FUL_STDUP_TRM, FUL_FSTR_TRM, FUL_WK23B_TRM, FUL_MKBED_TRM, FUL_WSHBK_TRM,
         FUL_KNCUT_TRM, FUL_FORC_TRM, ADL_DCLS_COM, ADL_DCLS_TRM, HUP_PRVACT_MCQ,
         CCC_RA_COM, CCT_RA_TRM, PA2_WALKHR_MCQ, PA2_LSPRTHR_MCQ, PA2_MSPRTHR_MCQ,
         PA2_SSPRTHR_MCQ, PA2_EXERHR_MCQ, GEN_DHDI_COM, GEN_DHDI_TRM,
         CCC_OAHAND_COM, CCT_OAHAND_TRM, CCC_OAHIP_COM, CCT_OAHIP_TRM, CCC_OAKNEE_COM,
         CCT_OAKNEE_TRM, CCC_ARTOT_COM, CCT_OTART_TRM) %>%
  # coalesce columns from comprehensive (COM) and tracking (TRM) surveys
  mutate(DEP_CESD10 = coalesce(DEP_CESD10_COM, DEP_CESD10_TRM),
         FUL_SHLD = coalesce(FUL_SHLD_COM, FUL_SHLD_TRM),
         FUL_STOOP = coalesce(FUL_STOOP_COM, FUL_STOOP_TRM),
         FUL_PUSH = coalesce(FUL_PUSH_COM, FUL_PUSH_TRM),
         FUL_LFT10 = coalesce(FUL_LFT10_COM, FUL_LFT10_TRM),
         FUL_HDLG = coalesce(FUL_HDLG_COM, FUL_HDLG_TRM),
         FUL_ST15 = coalesce(FUL_ST15_COM, FUL_ST15_TRM),
         FUL_SIT1H = coalesce(FUL_SIT1H_COM, FUL_SIT1H_TRM),
         FUL_STDUP = coalesce(FUL_STDUP_COM, FUL_STDUP_TRM),
         FUL_FSTR = coalesce(FUL_FSTR_COM, FUL_FSTR_TRM),
         FUL_WK23B = coalesce(FUL_WK23B_COM, FUL_WK23B_TRM),
         FUL_MKBED = coalesce(FUL_MKBED_COM, FUL_MKBED_TRM),
         FUL_WSHBK = coalesce(FUL_WSHBK_COM, FUL_WSHBK_TRM),
         FUL_KNCUT = coalesce(FUL_KNCUT_COM, FUL_KNCUT_TRM),
         FUL_FORC = coalesce(FUL_FORC_COM, FUL_FORC_TRM),
         ADL_DCLS = coalesce(ADL_DCLS_COM, ADL_DCLS_TRM),
         CCC_RA = coalesce(CCC_RA_COM, CCT_RA_TRM),
         CCC_OAHAND = coalesce(CCC_OAHAND_COM, CCT_OAHAND_TRM),
         CCC_OAHIP = coalesce(CCC_OAHIP_COM, CCT_OAHIP_TRM),
         CCC_OAKNEE = coalesce(CCC_OAKNEE_COM, CCT_OAKNEE_TRM),
         CCC_ARTOT = coalesce(CCC_ARTOT_COM, CCT_OTART_TRM),
         GEN_DHDI = coalesce(GEN_DHDI_COM, GEN_DHDI_TRM)) %>%
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
         FUL_SHLD =  replace(FUL_SHLD , FUL_SHLD  %in% c(8,9), NA), 
         FUL_STOOP = replace(FUL_STOOP, FUL_STOOP %in% c(8,9), NA),
         FUL_PUSH =  replace(FUL_PUSH , FUL_PUSH  %in% c(8,9), NA),
         FUL_LFT10 = replace(FUL_LFT10, FUL_LFT10 %in% c(8,9), NA),
         FUL_HDLG =  replace(FUL_HDLG , FUL_HDLG  %in% c(8,9), NA),
         FUL_ST15 =  replace(FUL_ST15 , FUL_ST15  %in% c(8,9), NA),
         FUL_SIT1H = replace(FUL_SIT1H, FUL_SIT1H %in% c(8,9), NA),
         FUL_STDUP = replace(FUL_STDUP, FUL_STDUP %in% c(8,9), NA),
         FUL_FSTR =  replace(FUL_FSTR , FUL_FSTR  %in% c(8,9), NA),
         FUL_WK23B = replace(FUL_WK23B, FUL_WK23B %in% c(8,9), NA),
         FUL_MKBED = replace(FUL_MKBED, FUL_MKBED %in% c(8,9), NA),
         FUL_WSHBK = replace(FUL_WSHBK, FUL_WSHBK %in% c(8,9), NA),
         FUL_KNCUT = replace(FUL_KNCUT, FUL_KNCUT %in% c(8,9), NA),
         FUL_FORC =  replace(FUL_FORC , FUL_FORC  %in% c(8,9), NA),
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
         HUP_PRVACT_MCQ =  replace(HUP_PRVACT_MCQ, HUP_PRVACT_MCQ %in% c(8,9), NA)) %>%
  # recode values for functional status 1 = yes, 2 = no / unable to do / don't do on doctors orders
  mutate(FUL_SHLD = if_else(FUL_SHLD %in% c(1), 1, if_else(is.na(FUL_SHLD), NA_integer_, 0)),
         FUL_STOOP = if_else(FUL_STOOP %in% c(1), 1, if_else(is.na(FUL_STOOP), NA_integer_, 0)),
         FUL_PUSH = if_else(FUL_PUSH %in% c(1), 1, if_else(is.na(FUL_PUSH), NA_integer_, 0)),
         FUL_LFT10 = if_else(FUL_LFT10 %in% c(1), 1, if_else(is.na(FUL_LFT10), NA_integer_, 0)),
         FUL_HDLG = if_else(FUL_HDLG %in% c(1), 1, if_else(is.na(FUL_HDLG), NA_integer_, 0)),
         FUL_ST15 = if_else(FUL_ST15 %in% c(1), 1, if_else(is.na(FUL_ST15), NA_integer_, 0)),
         FUL_SIT1H = if_else(FUL_SIT1H %in% c(1), 1, if_else(is.na(FUL_SIT1H), NA_integer_, 0)),
         FUL_STDUP = if_else(FUL_STDUP %in% c(1), 1, if_else(is.na(FUL_STDUP), NA_integer_, 0)),
         FUL_FSTR  = if_else(FUL_FSTR %in% c(1), 1, if_else(is.na(FUL_FSTR), NA_integer_, 0)),
         FUL_WK23B = if_else(FUL_WK23B %in% c(1), 1, if_else(is.na(FUL_WK23B), NA_integer_, 0)),
         FUL_MKBED = if_else(FUL_MKBED %in% c(1), 1, if_else(is.na(FUL_MKBED), NA_integer_, 0)),
         FUL_KNCUT = if_else(FUL_KNCUT %in% c(1), 1, if_else(is.na(FUL_KNCUT), NA_integer_, 0)),
         FUL_FORC  = if_else(FUL_FORC %in% c(1), 1, if_else(is.na(FUL_FORC), NA_integer_, 0))) %>%
  # create a sum score for functional status (total number of functional limitations)
  mutate(FUL_TOTAL = rowSums(dplyr::select(.,FUL_SHLD, FUL_STOOP, FUL_PUSH, FUL_LFT10, FUL_HDLG, 
                                    FUL_ST15, FUL_SIT1H, FUL_STDUP, FUL_FSTR, FUL_WK23B, 
                                    FUL_MKBED, FUL_KNCUT, FUL_FORC, FUL_WSHBK), 
                             na.rm = TRUE))


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
         #recode pain free so 1 is yes-pain free
         HUP_FREE_MCQ = recode (HUP_FREE_MCQ, '1' = 1, '2' = 0))


# create binary I/ADL classification
# 0 is low I/ADL, 1 is high I/ADL 
# log transformation for depression 
CLSA.baseline <- CLSA.baseline %>% 
  mutate(Binary_IADL.ADL = if_else(ADL_DCLS %in% c(1, 2), 0, if_else(is.na(ADL_DCLS), NA_integer_, 1)),
         Binary_IADL.ADL2 = if_else(ADL_DCLS %in% c(1), 0, if_else(is.na(ADL_DCLS), NA_integer_, 1)),
         log_trans_DEP_CESD10 = log(DEP_CESD10+1))


# 3) create a total physical activity score (0-400) ####
# scoring reference: https://meetinstrumentenzorg.nl/wp-content/uploads/instrumenten/PASE-handl.pdf 
# note: max should be 400 but is 485?

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
    #need to adjust this as the value should be divided by 7
    work_score = if_else(PA2_WRK_MCQ == 1, 21, 0)
  )

# Summing up all the scores to compute the total PASE score
CLSA.baseline <- CLSA.baseline %>%
  mutate(
    PASE_score = walk_score + light_sport_score + moderate_sport_score +
      strenuous_sport_score + muscle_strength_score + light_housework_score +
      heavy_housework_score + home_repairs_score + lawn_work_score +
      outdoor_gardening_score + caring_for_another_score + work_score
  )


# 4) filter out only people living with arthritis #### 
# n = 2058
CLSA.baseline.ra <- CLSA.baseline %>% filter(CCC_RA ==1)
# n = 18,681
CLSA.baseline.arthritis <- CLSA.baseline %>% filter(CCC_RA == 1 | CCC_OAHAND == 1 |
                                                    CCC_OAHIP == 1 | CCC_OAKNEE == 1 |
                                                    CCC_ARTOT == 1)


# 5) descriptive statistics ####
hist(CLSA.baseline.ra$FUL_TOTAL)
hist(CLSA.baseline.ra$PASE_score)
hist(CLSA.baseline.ra$DEP_CESD10)
hist(CLSA.baseline.ra$HUP_FREE_MCQ)
hist(CLSA.baseline.ra$ADL_DCLS)
hist(CLSA.baseline.ra$Binary_IADL.ADL) # 1-2 vs 3-5
hist(CLSA.baseline.ra$Binary_IADL.ADL2) # 1 vs 2-5


hist(CLSA.baseline.arthritis$PASE_score)
hist(CLSA.baseline.arthritis$DEP_CESD10)
hist(CLSA.baseline.arthritis$HUP_FREE_MCQ)
hist(CLSA.baseline.arthritis$ADL_DCLS)
hist(CLSA.baseline.arthritis$Binary_IADL.ADL)
hist(CLSA.baseline.arthritis$Binary_IADL.ADL2)

# 6) mediation analysis - rheumatoid arthritis ####

# X -> Y is significant (pain-free predicts I/ADL)
model3 <- glm(Binary_IADL.ADL2 ~  HUP_FREE_MCQ, family = "binomial", data = CLSA.baseline.ra)
summary(model3)
# Less pronounced effect for original I/ADL score 
model4 <- glm(Binary_IADL.ADL ~  HUP_FREE_MCQ, family = "binomial", data = CLSA.baseline.ra)
summary(model4)

# Now check simple mediation without moderation (all significant)
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        #w = "PASE_score", # "physical_activity",
        model = 4) # simple mediation

# checking simple moderating effect of PA on Pain-free > I/ADL (interaction not significant)
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        #m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 1) # moderation of direct effect only

# checking simple moderating effect of PA on Pain-free > depression (significant)
process(data = CLSA.baseline.ra,
        y = "log_trans_DEP_CESD10", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        #m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 1) # moderation of direct effect only

# checking simple moderating effect of PA on depression > I/ADL (significant)
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL_Log
        x = "log_trans_DEP_CESD10", # "pain",
        #m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 1) # moderation of direct effect only

# back to simple mediation model, now adding moderator for pain-free to depression (interaction significant)
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 7) # simple mediation with 1 moderator on a path

# mediation model, b path moderator(interaction significant)
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 14) # simple mediation with 1 moderator on b path


# mediation model, with 2 moderators on a and b but not c [direct] (a sig; b not)
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 58) # simple mediation with 1 moderator on a and b path


# mediation with moderation an a,b,c paths
process(data = CLSA.baseline.ra,
        y = "Binary_IADL.ADL2", # "functional_limitations",FUL_TOTAL
        x = "HUP_FREE_MCQ", # "pain",
        m = "DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 59)

# 7) mediation analysis - all  arthritis ####

# X -> Y is significant (pain-free predicts I/ADL)
model1 <- glm(Binary_IADL.ADL2 ~  HUP_FREE_MCQ, family = "binomial", data = CLSA.baseline.arthritis)
summary(model1)
# Less pronounced effect for original I/ADL score 
model2 <- glm(Binary_IADL.ADL ~  HUP_FREE_MCQ, family = "binomial", data = CLSA.baseline.arthritis)
summary(model2)

# Now check simple mediation without moderation (all significant)
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        #w = "PASE_score", # "physical_activity",
        model = 4) # simple mediation

# checking simple moderating effect of PA on Pain-free > I/ADL (interaction not significant)
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        #m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 1) # moderation of direct effect only

# checking simple moderating effect of PA on Pain-free > depression (significant)
process(data = CLSA.baseline.arthritis,
        y = "log_trans_DEP_CESD10", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        #m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 1) # moderation of direct effect only

# checking simple moderating effect of PA on depression > I/ADL (significant)
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL_Log
        x = "log_trans_DEP_CESD10", # "pain",
        #m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 1) # moderation of direct effect only

# back to simple mediation model, now adding moderator for pain-free to depression (interaction significant)
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 7) # simple mediation with 1 moderator on a path

# mediation model, b path moderator(interaction significant)
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 14) # simple mediation with 1 moderator on b path


# mediation model, with 2 moderators on a and b but not c [direct] (a sig; b not)
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL_Log
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 58) # simple mediation with 1 moderator on a and b path


# mediation with moderation an a,b,c paths
process(data = CLSA.baseline.arthritis,
        y = "Binary_IADL.ADL", # "functional_limitations",FUL_TOTAL
        x = "HUP_FREE_MCQ", # "pain",
        m = "log_trans_DEP_CESD10", # "depressive_symptoms",
        w = "PASE_score", # "physical_activity",
        model = 59)



# 7) mixed graphical model ####
library(qgraph)
library(mgm)
install.packages("bootnet")
library(bootnet)

CLSA.ra.network <- CLSA.baseline.ra3 %>% 
  dplyr::select(PASE_score, FUL_TOTAL, DEP_CESD10, log_trans_DEP_CESD10, HUP_FREE_MCQ,Binary_IADL.ADL, Binary_IADL.ADL2) %>%
  mutate(Binary_IADL.ADL = as.factor(Binary_IADL.ADL),
         Binary_IADL.ADL2 = as.factor(Binary_IADL.ADL2))


str(CLSA.ra.network)


model <- glm(Binary_IADL.ADL ~ PASE_score + log_trans_DEP_CESD10 + HUP_FREE_MCQ, family = "binomial", data = CLSA.baseline.ra3)
summary(model)

model2 <- glm(Binary_IADL.ADL2 ~ PASE_score + log_trans_DEP_CESD10 + HUP_FREE_MCQ, family = "binomial", data = CLSA.baseline.ra3)
summary(model2)



#this gives partial polychoric correlation
install.packages("correlation")
library(correlation) # correlation analysis
as_tibble(cor_to_pcor(cor_auto(CLSA.ra.network))) %>% round(2)
as_tibble(cor_auto(CLSA.ra.network)) %>% round(2)

# mixed graphic model
# PASE            g 0-400 continuous
# FUL_TOTAL       g 0-15 continuous
# DEP_CESD10      g 0-30 continuous
# HUP_FREE_MCQ    c 0-1 categorical
# Binary_IADL.ADL   c 0-1 categorical

#identifies caregiving and social media as categorical (c) and all others and gaussian (g)
type<-c("g","g","g", "c", "c")

#identifies categorical vars as having 2 levels and all continuous variables as 1
level<-c(1,1,1,2,2)

network.2<-estimateNetwork(CLSA.ra.network, default="mgm", type=type, level=level)

pdf("CLSA Miriams Networks - Full Sample.pdf",width = 5, height = 4)
layout(t(1:1))
plot(network.2, edge.labels = T, palette = 'pastel', layout="circle")
dev.off()

test<- CLSA.baseline %>% dplyr::select(PASE_score, GEN_DHDI, PA2_SIT_MCQ, PA2_WALK_MCQ, PA2_LSPRT_MCQ, PA2_MSPRT_MCQ, 
                                PA2_SSPRT_MCQ, PA2_EXER_MCQ, DEP_CESD10, Binary_IADL.ADL)

cor_auto(test)


# 8) misc/junk ####

# Display the modified dataset with the PASE scores
# When PASE items recoded 0-3 per manual range of scores seems low?
# When not recoded range seems more expected (e.g., max near 400)
summary(CLSA.baseline$PASE_SCORE, na.rm = T)
summary(CLSA.baseline.ra$PASE_SCORE, na.rm = T)

#there are 2058 people with arthritis in the same and 48,764 without
CLSA.baseline %>% group_by(CCC_RA) %>% summarise(count = n())
# no missing for functional limitations for those with arthritis
CLSA.baseline.ra %>% group_by(FUL_TOTAL) %>% summarise(count = n())
# 14 missing depression
summary(CLSA.baseline.ra$DEP_CESD10)
# 175 missing for PASE
summary(CLSA.baseline.ra$PASE_SCORE)
# 172 missing for pain free
summary(CLSA.baseline.ra$HUP_FREE_MCQ)

# to do:
# double-check PASE scoring with a couple manual spot checks
