# Title: Wrangle provisional RoL dataset output
# Author: Mary Lofton
# Date: 04MAR24

# Purpose: collate model output files of GLM-AED runs

# Load packages
library(tidyverse)
library(lubridate)

# repackage GLM-AED model output

n_model_runs = 1000

for(i in 1:n_model_runs){
  
  filename <- paste0("./Surrogate_dataset/Deepm2_exm_weight2_EXO/output/model_scenario_eco-KGML_",i,".csv")
  temp_model <- read_csv(filename) %>%
    filter(Depth_m == 1.6) %>%
    select(DateTime, Chla_ugL)
  
  filename_param <- paste0("./Surrogate_dataset/Deepm2_exm_weight2_EXO/output/param_scenario_eco-KGML_",i,".csv")
  temp_param <- read_csv(filename_param) 
  
  temp <- data.frame(R_growth_cyano = temp_param$R_growth_cyano,
                     R_growth_green = temp_param$R_growth_green,
                     R_growth_diatom = temp_param$R_growth_diatom,
                     w_p_cyano = temp_param$w_p_cyano,
                     w_p_green = temp_param$w_p_green,
                     w_p_diatom = temp_param$w_p_diatom,
                     deviation = 0,
                     datetime = temp_model$DateTime,
                     variable = "PHY_tchla_1.6",
                     prediction = temp_model$Chla_ugL)
  
  if(i == 1){
    final <- temp
  } else {
    final <- bind_rows(final, temp)
  }
}


# now for observations
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "Depth_m",     
                 "Temp_C",     
                 "DO_mgL",     
                 "DOsat_percent",     
                 "Cond_uScm",     
                 "SpCond_uScm",     
                 "Chla_ugL",     
                 "Turbidity_NTU",     
                 "pH",     
                 "ORP_mV",     
                 "PAR_umolm2s",     
                 "DescRate_ms",     
                 "Flag_DateTime",     
                 "Flag_Temp_C",     
                 "Flag_DO_mgL",     
                 "Flag_DOsat_percent",     
                 "Flag_Cond_uScm",     
                 "Flag_SpCond_uScm",     
                 "Flag_Chla_ugL",     
                 "Flag_Turbidity_NTU",     
                 "Flag_pH",     
                 "Flag_ORP_mV",     
                 "Flag_PAR_umolm2s",     
                 "Flag_DescRate_ms"    ), check.names=TRUE)

unlink(infile1)

dt2 <- dt1 %>%
  filter(Reservoir == "FCR" & Site == 50) %>%
  mutate(Date = date(DateTime)) %>%
  filter(DateTime >= "2016-12-01" & DateTime <= "2020-12-31" & !is.na(Chla_ugL)) %>%
  group_by(Date) %>%
  slice(which.min(abs(as.numeric(Depth_m) - 1.6))) %>%
  select(Date, Chla_ugL) %>%
  rename(datetime = Date,
         observation = Chla_ugL) %>%
  add_column(variable = "PHY_tchla_1.6") %>%
  arrange(datetime, variable, observation)

final1 <- final %>%
  filter(datetime %in% dt2$datetime)

write.csv(final1, "./Surrogate_model_runs/surrogate_model_runs_04MAR24.csv", row.names = FALSE)
write.csv(dt2, "./Surrogate_model_runs/surrogate_observations_04MAR24.csv", row.names = FALSE)

