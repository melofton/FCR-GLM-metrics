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
setwd("./Calibrated_models/Deepm2_exm_weight2") 

# run GLM-AED using GLM3r
# an output folder with lake.csv and output.nc files will automatically be created 
# in the working directory
GLM3r::run_glm()

#### grab output using glmtools ----

# set folder location of output
sim_folder <- "./Calibrated_models/Deepm2_exm_weight2"

# set file location of output
nc_file <- file.path(sim_folder, 'output/output.nc') 
glmtools::sim_vars(file = nc_file)
var <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6)

#### visualize output ----

# plot output
ggplot(data = var, aes(x = DateTime, y = PHY_tchla_1.6))+
  geom_point()+
  theme_classic()

#### pull parameter values from nml ----
nml_file <- file.path(sim_folder, 'aed/aed2_phyto_pars_6NOV23_MEL.nml')
nml <- glmtools::read_nml(nml_file = nml_file)
glmtools::get_nml_value(nml, arg_name = 'pd%w_p')
glmtools::get_nml_value(nml, arg_name = 'pd%R_growth')
new_nml <- glmtools::set_nml(nml, arg_list = list('pd%w_p' = c(-0.12,-0.12,-0.14)))

#create path to write permuted nml to file
write_path <- nml_file
#write permuted nml to file
glmtools::write_nml(new_nml, file = write_path)
