#### R script that formats lake data for R Shiny
#
# Ben.Block@tetratech.com, Date completed: 2022-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R version 4.1.2 (2021-11-01) -- "Bird Hippie"

#clear global environment
rm(list=ls())

# load libraries
library(tidyverse)

# Declare directories ####
wd <- getwd()
input.dir <- "inst/shiny/MA_NBC/WQ_Data"
fn.data1 <- "MA_WQ_Lakes_Compiled_20220525.csv"
myDate <- format(Sys.Date(), "%Y%m%d")

# Read data files ####
df_lakes <- read_csv(file.path(wd,input.dir, fn.data1)
                           , na = c("NA",""), trim_ws = TRUE, skip = 0
                           , col_names = TRUE, guess_max = 100000)

# Trim dataset ####
vars_keep <- c("WaterbodyID"
               ,"DistCat"
               ,"Alkalinity_uEqL"
               ,"Chla_ugL"
               ,"Chloride_mgL"
               ,"Conductivity_uScm"
               ,"DO_bttm_mgL"
               ,"DO_mean_1mBlwThrmclin_mgL"
               ,"DO_mean_hypo_mgL"
               ,"DO_mean_surf_mgL"
               ,"DO_mgL"
               ,"DO_SAT_pct"
               ,"DOSAT_bttm_pct"
               ,"DOSAT_mean_1mBlwThrmclin_pct"
               ,"DOSAT_mean_hypo_pct"
               ,"DOSAT_mean_surf_pct"
               ,"NO3NO2_mgL"
               ,"pH_bttm"
               ,"pH_mean_1mBlwThrmclin"
               ,"pH_mean_hypo"
               ,"pH_mean_surf"
               ,"pH_None"
               ,"Temp_bttm_C"
               ,"Temp_min_C"
               ,"Temperature_Water_DegC"
               ,"TN_mgL"
               ,"TP_mgL"
               ,"WsAreaSqKm"
               ,"BFICat"
               ,"ElevCat"
               ,"Al2O3Cat"
               ,"K2OCat"
               ,"Na2OCat"
               ,"PctImp2019Cat"
               ,"KffactCat"
               ,"InorgNWetDep_2008Cat"
               ,"Tmean8110Cat"
               ,"RunoffCat"
               ,"SlopeCat"
               ,"ClayCat"
               ,"SandCat"
               ,"OmCat"
               ,"WetIndexCat"
               ,"WetIndexWs"
               ,"PctUrb2019Ws"
               ,"PctFor2019Ws"
               ,"PctWet2019Ws")

df_trim <- df_lakes %>% 
  select(one_of(vars_keep)) %>% 
  mutate(DistCat_trim = case_when((DistCat == "BestRef"
                                   |DistCat == "Ref"
                                   |DistCat == "SubRef") ~ "Ref",
                                  (DistCat == "HighStrs"
                                   |DistCat == "Strs"
                                   |DistCat == "SomeStrs") ~ "Strs",
                                  (DistCat == "Other") ~ "Other",
                                  (is.na(DistCat)) ~ "Unknown")) %>%
  mutate(DistCat2 = case_when((DistCat_trim == "Ref") ~ "Ref"
                              , (DistCat_trim != "Ref") ~ "Non_Ref")) %>% 
  group_by(WaterbodyID, DistCat, DistCat_trim, DistCat2) %>% 
  summarize(across(Alkalinity_uEqL:PctWet2019Ws, mean, na.rm = TRUE)) %>% 
  mutate_all(~ifelse(is.nan(.),NA,.))

# Export data ####
write.table(df_trim, file.path(wd, input.dir, paste0("MA_WQ_Lakes_4RShiny_"
                                                     , myDate,".csv"))
            , sep = ",", row.names = FALSE, col.names = TRUE, na = "")
