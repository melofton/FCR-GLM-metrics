# Title: Wrangle eco-KGML dataset output
# Author: Mary Lofton
# Date: 28FEB24

# Purpose: collate model output files of GLM-AED runs

# Load packages
library(tidyverse)
library(lubridate)

# Assemble dataframe of model drivers (airtemp, shortwave, inflow)

met <- read_csv('./Surrogate_dataset/Deepm2_exm_weight2_EXO/inputs/met_avg_filtered.csv') %>%
  mutate(DateTime = date(time)) %>%
  select(DateTime, AirTemp, ShortWave) %>%
  group_by(DateTime) %>%
  summarize(AirTemp_C = mean(AirTemp, na.rm = TRUE),
            Shortwave_Wm2 = mean(ShortWave, na.rm = TRUE)) %>%
  filter(DateTime >= "2016-12-01" & DateTime <= "2020-12-31")

inf <- read_csv('./Surrogate_dataset/Deepm2_exm_weight2_EXO/inputs/FCR_weir_inflow_2013_2020_20220411_allfractions_2poolsDOC_1dot5xDOCr.csv') %>%
  mutate(DateTime = date(time)) %>%
  select(DateTime, FLOW) %>%
  rename(Inflow_cms = FLOW) %>%
  filter(DateTime >= "2016-12-01" & DateTime <= "2020-12-31") 

driv <- left_join(met, inf, by = "DateTime") %>%
  add_column(Lake = "FCR",
             Site = 50,
             DataType = "modeled")

write.csv(driv, "./Eco-KGML_model_runs/model_runs_driver_data.csv", row.names = FALSE)

n_model_runs = 1000

for(i in 1:n_model_runs){
  
  filename <- paste0("./Surrogate_dataset/Deepm2_exm_weight2_EXO/output/model_scenario_eco-KGML_",i,".csv")
  temp <- read_csv(filename)
  
  filename_param <- paste0("./Surrogate_dataset/Deepm2_exm_weight2_EXO/output/param_scenario_eco-KGML_",i,".csv")
  temp_param <- read_csv(filename_param) %>%
    add_column(DataType = "modeled")
  
  if(i == 1){
    final <- temp
    final_param <- temp_param
  } else {
    final <- bind_rows(final, temp)
    final_param <- bind_rows(final_param, temp_param)
  }
}

write.csv(final, "./Eco-KGML_model_runs/eco-KGML_model_runs.csv", row.names = FALSE)
write.csv(final_param, "./Eco-KGML_model_runs/eco-KGML_parameter_scenarios.csv", row.names = FALSE)

one_six <- final %>%
  filter(Depth_m == 1.6)

# Plots
ggplot(data = one_six, aes(x = DateTime, y = LightAttenuation_Kd, group = ModelRunType))+
  geom_line()+
  theme_bw()
