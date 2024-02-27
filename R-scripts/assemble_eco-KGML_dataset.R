# Title: Assemble eco-KGML Dataset
# Author: Mary Lofton
# Date: 27FEB24

# Purpose: Create a dataset for Abhilash for foundation model

# Notes:

# Data frame format: Parameter 1, parameter 2, temp/weather deviation, datetime, 
# variable, prediction, observation

# load packages ----
source("./R-scripts/install.R")

library(tidyverse)
library(lubridate)
library(lhs)
library(glmtools)
library(GLM3r)

# set working directory; you can change this to be any calibration folder ----
setwd("./Surrogate_dataset/Deepm2_exm_weight2_EXO") 

# create matrix of parameter values using maximin space-filling design ----
phyto_groups <- c("cyano","green","diatom")
param_names <- c("pd%R_growth", "pd%w_p")

# Latin hypercube function
# n runs for m factors
# so start with 1000 runs for 6 factors (parameters)

mylhs <- function(n, m)
{
  ## generate the Latin hypercube 
  l <- (-(n - 1)/2):((n - 1)/2)
  L <- matrix(NA, nrow=n, ncol=m)
  for(j in 1:m) L[,j] <- sample(l, n)
  
  ## draw the random uniforms and turn the hypercube into a sample
  U <- matrix(runif(n*m), ncol=m)
  X <- (L + (n - 1)/2 + U)/n
  colnames(X) <- paste0("x", 1:m)
  
  ## return the design and the grid it lives on for visualization
  return(list(X=X, g=c((l + (n - 1)/2)/n,1)))
}

Dlist <- mylhs(n = 1000, m = 6)

# data wrangling to get parameter values in correct range
scale_R_growth <- function(x, na.rm = FALSE) x*3 + 0.5
scale_w_p <- function(x, na.rm = FALSE){
  
  for(i in 1:length(x)){
  if(x[i] == 0.5){x[i] <- 0} else if(x[i] < 0.5){x[i] <- x[i]*-2} else {x[i] <- (x[i]-0.5)*2}
  }
  
  return(x)
  
}

param_values <- tibble(data.frame(Dlist$X)) %>%
  mutate_at(c("x1","x2","x3"), scale_R_growth) %>%
  mutate_at(c("x4","x5","x6"), scale_w_p)
colnames(param_values) <- c("R_growth_cyano","R_growth_green","R_growth_diatom","w_p_cyano","w_p_green","w_p_diatom")

# set nml filepath
nml_file <- file.path('./aed/aed2_phyto_pars_27NOV23_MEL.nml')

# set file location of output
nc_file <- file.path('./output/output.nc') 

# check variable names if needed
simvars <- glmtools::sim_vars(file = nc_file)

# save starting version of nml in environment so you can reset after
start_nml <- glmtools::read_nml(nml_file = nml_file)

# set variables you want to save
vars <- c("PHY_tchla","PHS_frp","NIT_amm","NIT_nit","temp","TOT_extc")

# set depths you want to pull
depths <- c(0.1, 1.6, 3.8, 5, 6.2, 8, 9)

# for-loop to run GLM using different parameter values
  for(j in 1:length(unlist(param_values[,1]))){
    
  # read in nml
  nml <- glmtools::read_nml(nml_file = nml_file)
  
  # get current parameter values
  curr_R_growth <- nml$phyto_data[[param_names[1]]]
  curr_w_p <- nml$phyto_data[[param_names[2]]]
  
  # replace parameter value as desired
  curr_R_growth <- unname(unlist(param_values[j,c(1:3)]))
  curr_w_p <- unname(unlist(param_values[j,c(4:6)]))
  
  # set nml parameter values
  new_nml <- glmtools::set_nml(nml, arg_name = param_names[1], arg_val = curr_R_growth)
  new_nml1 <- glmtools::set_nml(new_nml, arg_name = param_names[2], arg_val = curr_w_p)
  
  # create path to write permuted nml to file
  write_path <- nml_file
  
  # write permuted nml to file
  glmtools::write_nml(new_nml1, file = write_path)
  
  # run GLM-AED using GLM3r
  GLM3r::run_glm()
  
  # pull variables and depths of interest
  for(k in 1:length(vars)){
  
    var <- glmtools::get_var(nc_file, var_name = vars[k], reference="surface", z_out=depths) 
    
    if(k == 1){
      var_df <- var
    } else {
      var_df <- left_join(var_df,var,by = "DateTime")
      }
    
  }
  
  # wrangle model output variables
  temp <- var_df %>%
    pivot_longer(-DateTime, names_to = "variable", values_to = "observation") %>%
    separate_wider_delim(variable, delim = "_", too_few = "align_end", names = c("trash","varname","Depth_m")) %>%
    select(-trash) %>%
    mutate(varname = ifelse(varname == "tchla","Chla_ugL",
                            ifelse(varname == "frp","SRP_ugL",
                                   ifelse(varname == "nit","NO3NO2_ugL",
                                          ifelse(varname == "amm","NH4_ugL",
                                                 ifelse(varname == "temp","WaterTemp_C","LightAttenuation_Kd"))))),
           Depth_m = as.numeric(Depth_m)) %>%
    pivot_wider(names_from = varname, values_from = observation) %>%
    mutate(NH4_ugL = NH4_ugL*18.04,
           NO3NO2_ugL = NO3NO2_ugL*62.0049,
           SRP_ugL = SRP_ugL*94.9714,
           DIN_ugL = NH4_ugL + NO3NO2_ugL) %>%
    add_column(Lake = "FCR",
               Site = 50,
               DataType = "modeled",
               ModelRunType = j,
               Flag_WaterTemp_C = 0,
               Flag_SRP_ugL = 0,
               Flag_DIN_ugL = 0,
               Flag_LightAttenuation_Kd = 0,
               Flag_Chla_ugL = 0) %>%
    select(Lake, DateTime, Site, Depth_m, DataType, ModelRunType, WaterTemp_C, SRP_ugL, DIN_ugL, LightAttenuation_Kd, Chla_ugL,
           Flag_WaterTemp_C, Flag_SRP_ugL, Flag_DIN_ugL, Flag_LightAttenuation_Kd, Flag_Chla_ugL)

  # pull parameters from model output
  R_growth <- new_nml1$phyto_data$`pd%R_growth`
  w_p <- new_nml1$phyto_data$`pd%w_p`
  
  # assemble dataframe for that model run
  # remember to have a unique identifier for model run that can be mapped to parameters
  temp_param <- data.frame(R_growth_cyano = R_growth[1],
                     R_growth_green = R_growth[2],
                     R_growth_diatom = R_growth[3],
                     w_p_cyano = w_p[1],
                     w_p_green = w_p[2],
                     w_p_diatom = w_p[3],
                     ModelRunType = j,
                     Lake = "FCR",
                     Site = 50)

  # make sure you reset nml
  glmtools::write_nml(start_nml, file = nml_file)
  
  # bind to other model runs
  if(j == 1){
    final <- temp
    final_param <- temp_param
  } else {
    final <- bind_rows(final, temp)
    final_param <- bind_rows(final_param, temp_param)
  }

    }

write.csv(final, file = "./model_scenarios_eco-KGML.csv",row.names = FALSE)
