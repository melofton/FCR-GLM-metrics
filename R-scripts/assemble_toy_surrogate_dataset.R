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
param_list <- list(set1 = c(0.5, 1.5, 2.5),
                   set2 = c(1, 2, 3),
                   set3 = c(1.5, 2.5, 3.5))

# set nml filepath
nml_file <- file.path('./aed/aed2_phyto_pars_6NOV23_MEL.nml')

# set file location of output
nc_file <- file.path('./output/output.nc') 

# for-loop to run GLM using different parameter values
for(i in 1:length(param_list)){
  
  # read in nml
  nml <- glmtools::read_nml(nml_file = nml_file)

  # set nml parameter values
  new_nml <- glmtools::set_nml(nml, arg_list = list('pd%R_growth' = param_list[[i]]))
  
  # create path to write permuted nml to file
  write_path <- nml_file
  
  # write permuted nml to file
  glmtools::write_nml(new_nml, file = write_path)
  
  # run GLM-AED using GLM3r
  GLM3r::run_glm()

  # pull variable of interest from model output
  var <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6)
  
  # assemble dataframe for that model run
  temp <- data.frame(R_growth_cyano = param_list[[i]][1],
                     R_growth_green = param_list[[i]][2],
                     R_growth_diatom = param_list[[i]][3],
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
