# Title: Assemble Toy Surrogate Dataset
# Author: Mary Lofton
# Date: 06NOV23

# Purpose: Create a toy dataset for Bobby's students to run with for surrogate development

# Notes:

# Data frame format: Parameter 1, parameter 2, temp/weather deviation, datetime, 
# variable, prediction, observation

# load packages
library(tidyverse)
library(lubridate)

# set working directory; you can change this to be any calibration folder
setwd("./Surrogate_dataset/Deepm2_exm_weight2") 

# create lists of parameter values
phyto_groups <- c("cyano","green","diatom")
param_names <- c("pd%R_growth", "pd%w_p")
param_values_list <- list(R_growth = seq(0.1, 5, by = 0.4),
                          w_p = seq(-1,1, by = 0.2))

# set nml filepath
nml_file <- file.path('./aed/aed2_phyto_pars_6NOV23_MEL.nml')

# set file location of output
nc_file <- file.path('./output/output.nc') 

# save starting version of nml in environment so you can reset after
start_nml <- glmtools::read_nml(nml_file = nml_file)

# for-loop to run GLM using different parameter values
for(i in 1:length(phyto_groups)){
  
  for(j in 1:length(param_names)){
    
    for(k in 1:length(param_values_list[[j]])){
  
  # read in nml
  nml <- glmtools::read_nml(nml_file = nml_file)
  
  # get current parameter values
  curr_param <- nml$phyto_data[[param_names[j]]]
  
  # replace parameter value as desired
  curr_param[i] <- param_values_list[[j]][k]

  # set nml parameter values
  new_nml <- glmtools::set_nml(nml, arg_name = param_names[j], arg_val = curr_param)
  
  # create path to write permuted nml to file
  write_path <- nml_file
  
  # write permuted nml to file
  glmtools::write_nml(new_nml, file = write_path)
  
  # run GLM-AED using GLM3r
  GLM3r::run_glm()

  # pull variable of interest from model output
  var <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6)
  
  # pull parameters from model output
  R_growth <- new_nml$phyto_data$`pd%R_growth`
  w_p <- new_nml$phyto_data$`pd%w_p`
  
  # assemble dataframe for that model run
  temp <- data.frame(R_growth_cyano = R_growth[1],
                     R_growth_green = R_growth[2],
                     R_growth_diatom = R_growth[3],
                     w_p_cyano = w_p[1],
                     w_p_green = w_p[2],
                     w_p_diatom = w_p[3],
                     deviation = 0,
                     datetime = var$DateTime,
                     variable = "PHY_tchla_1.6",
                     prediction = var$PHY_tchla_1.6)
  
  # bind to other model runs
  if(i == 1){
    final <- temp
  } else {
    final <- bind_rows(temp, final)
  }

    }
  }
}

# make sure you reset nml
glmtools::write_nml(start_nml, file = nml_file)

# write final dataset to file
write.csv(final, file = "./collated_model_scenarios.csv",row.names = FALSE)

# plot output
p <- ggplot(data = final, aes(x = datetime, y = prediction, group = as.factor(R_growth_cyano), color = as.factor(R_growth_cyano)))+
  geom_point()+
  xlab("")+
  ylab("chlorophyll-a (ug/L) at 1.6 m")+
  labs(color = "Model parameter scenario")+
  scale_color_manual(values = as.factor(c(0.5, 1, 1.5)), labels = c("Low growth rates","Original calibrated growth rates","High growth rates"))+
  theme_classic()+
  theme(legend.position = "bottom")
p
ggsave(p, filename = "./figures/example_param_scenarios.png", units = "in",
       dev = "png", height = 3, width = 7)
