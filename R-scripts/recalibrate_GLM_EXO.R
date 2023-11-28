# Title: Run GLM-AED in Docker
# Author: Mary Lofton
# Date: 06NOV23

# Purpose: Verify that you can run GLM-AED in Linux environment in Docker
# and access and manipulate output

# load packages
library(tidyverse)
library(lubridate)

#### run model ----

# set working directory; you can change this to be any calibration folder
setwd("./Calibrated_models/Deepm2_exm_weight2_EXO") 

# run GLM-AED using GLM3r
# an output folder with lake.csv and output.nc files will automatically be created 
# in the working directory
GLM3r::run_glm()

#### grab output using glmtools ----

# set file location of output
nc_file <- file.path('output/output.nc') 
glmtools::sim_vars(file = nc_file)
var <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6) %>%
  rename(modeled = PHY_tchla_1.6)
green <- glmtools::get_var(nc_file, var_name = "PHY_green", reference="surface", z_out=1.6)
cyano <- glmtools::get_var(nc_file, var_name = "PHY_cyano", reference="surface", z_out=1.6)
diatom <- glmtools::get_var(nc_file, var_name = "PHY_diatom", reference="surface", z_out=1.6)
obs <- read_csv("/home/rstudio/RProjects/FCR-GLM-metrics/observations/EXOChla.csv") %>%
  filter(Depth == 1.6) %>%
  rename(observed = PHY_TCHLA) %>%
  select(-Depth)

chla <- inner_join(obs, var, by = "DateTime") %>%
  pivot_longer(cols = modeled:observed, names_to = "data_type", values_to = "chla")

phytos <- left_join(green,cyano, by = "DateTime") %>%
  left_join(diatom, by = "DateTime") %>%
  pivot_longer(cols = starts_with("PHY"), names_to = "group", values_to = "biomass")

#### visualize output ----

# plot output
ggplot(data = var, aes(x = DateTime, y = PHY_tchla_1.6))+
  geom_point()+
  theme_classic()

ggplot(data = phytos, aes(x = DateTime, y = biomass, group = group, color = group))+
  geom_point()+
  theme_classic()

ggplot(data = chla, aes(x = DateTime, y = chla, group = data_type, color = data_type))+
  geom_point()+
  theme_classic()

#### pull parameter values from nml ----
nml_file <- file.path('aed/aed2_phyto_pars_27NOV23_MEL.nml')
nml <- glmtools::read_nml(nml_file = nml_file)
glmtools::get_nml_value(nml, arg_name = 'pd%Xcc')
new_nml <- glmtools::set_nml(nml, arg_list = list('pd%Xcc' = c(60, 10, 40)))

#create path to write permuted nml to file
write_path <- nml_file
#write permuted nml to file
glmtools::write_nml(new_nml, file = write_path)
