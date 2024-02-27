# Title: Assemble Surrogate Dataset
# Author: Mary Lofton
# Date: 06NOV23

# Purpose: Create a dataset for Bobby's students to run with for surrogate development

# Notes:

# Data frame format: Parameter 1, parameter 2, temp/weather deviation, datetime, 
# variable, prediction, observation

# load packages ----
source("./R-scripts/install.R")

library(tidyverse)
library(lubridate)
library(lhs)

# set working directory; you can change this to be any calibration folder ----
setwd("./Surrogate_dataset/Deepm2_exm_weight2_EXO") 

# create matrix of parameter values using maximin space-filling design ----
phyto_groups <- c("cyano","green","diatom")
param_names <- c("pd%R_growth", "pd%w_p")
param_values_list <- list(R_growth = c(0.5, 1, 2, 3, 3.5),
                          w_p = c(-1, -0.5, 0, 0.5, 1))

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

plot(Dlist$X[,1:2], xlim=c(0,1), ylim=c(0,1), xlab="x1", ylab="x2")

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

ggplot(data = param_values, aes(x = R_growth_cyano, y = w_p_cyano))+
  geom_point()+
  theme_bw()
ggplot(data = param_values, aes(x = R_growth_green, y = w_p_green))+
  geom_point()+
  theme_bw()
ggplot(data = param_values, aes(x = R_growth_diatom, y = w_p_diatom))+
  geom_point()+
  theme_bw()

# set nml filepath
nml_file <- file.path('./aed/aed2_phyto_pars_27NOV23_MEL.nml')

# set file location of output
nc_file <- file.path('./output/output.nc') 

# save starting version of nml in environment so you can reset after
start_nml <- glmtools::read_nml(nml_file = nml_file)

# for-loop to run GLM using different parameter values
for(i in 1:length(phyto_groups)){
  
  for(j in 1:length(param_values_list[[1]])){
    
    for(k in 1:length(param_values_list[[2]])){
  
  # read in nml
  nml <- glmtools::read_nml(nml_file = nml_file)
  
  # get current parameter values
  curr_R_growth <- nml$phyto_data[[param_names[1]]]
  curr_w_p <- nml$phyto_data[[param_names[2]]]
  
  # replace parameter value as desired
  curr_R_growth[i] <- param_values_list[[1]][j]
  curr_w_p[i] <- param_values_list[[2]][k]
  
  # set nml parameter values
  new_nml <- glmtools::set_nml(nml, arg_name = param_names[1], arg_val = curr_R_growth)
  new_nml1 <- glmtools::set_nml(new_nml, arg_name = param_names[2], arg_val = curr_w_p)
  
  # create path to write permuted nml to file
  write_path <- nml_file
  
  # write permuted nml to file
  glmtools::write_nml(new_nml1, file = write_path)
  
  # run GLM-AED using GLM3r
  GLM3r::run_glm()

  # pull variable of interest from model output
  var <- glmtools::get_var(nc_file, var_name = "PHY_tchla", reference="surface", z_out=1.6)
  
  # pull parameters from model output
  R_growth <- new_nml1$phyto_data$`pd%R_growth`
  w_p <- new_nml1$phyto_data$`pd%w_p`
  
  # pull f factors
  nc_file <- file.path('output/output.nc') 
  
  for(f in 1:length(f_factor_names)){
    
    f_factor <- glmtools::get_var(nc_file, var_name = f_factor_names[f], reference="surface", z_out=1.6) 
    
    if(f == 1){
      f_factors <- f_factor
    } else {
      f_factors <- left_join(f_factors, f_factor, by = c("DateTime"))
    }
  }
  
  final_factors <- f_factors %>% rename(datetime = DateTime)
  
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
  temp1 <- left_join(temp, final_factors, by = "datetime")
  
  # make sure you reset nml
  glmtools::write_nml(start_nml, file = nml_file)
  
  # bind to other model runs
  if(i == 1 & j == 1 & k == 1){
    final <- temp
  } else {
    final <- bind_rows(final, temp)
  }

    }
  }
}

# read in observation dataset
obs <- read_csv("/home/rstudio/RProjects/FCR-GLM-metrics/observations/EXOChla.csv") %>%
  filter(Depth == 1.6) %>%
  rename(observed = PHY_TCHLA) %>%
  select(-Depth)

# write final dataset to file
final1 <- final %>%
  filter(datetime %in% obs$DateTime)
write.csv(final1, file = "./collated_model_scenarios_EXOdates.csv",row.names = FALSE)

# plot parameter space
parms <- final1 %>%
  select(R_growth_cyano, w_p_cyano) %>%
  distinct()

ggplot(data = parms, aes(x = R_growth_cyano, y = w_p_cyano))+
  geom_point(size = 3)+
  theme_bw()

# plot output
plot_data <- final1 %>%
  group_by(R_growth_cyano, w_p_cyano) %>%
  mutate(model_run = cur_group_id()) %>%
  rename(DateTime = datetime) %>%
  left_join(., obs, by = "DateTime")

p <- ggplot(data = plot_data)+
  geom_line(aes(x = DateTime, y = prediction, group = as.factor(model_run), color = as.factor(R_growth_cyano), linetype = as.factor(w_p_cyano)))+
  geom_point(aes(x = DateTime, y = observed), size = 0.5)+
  xlab("")+
  ylab("chlorophyll-a (ug/L) at 1.6 m")+
  labs(color = "Cyano growth rate", linetype = "Cyano sinking rate")+
  theme_classic()+
  theme(legend.position = "bottom")
p
ggsave(p, filename = "./figures/example_param_scenarios.png", units = "in",
       dev = "png", height = 3, width = 7)

# look at f factors
glmtools::sim_vars(file = nc_file)

f_factor_names <- c("PHY_green_fI","PHY_green_fNit","PHY_green_fPho","PHY_green_fSal",
               "PHY_green_fSil","PHY_green_fT","PHY_cyano_fI","PHY_cyano_fNit","PHY_cyano_fPho","PHY_cyano_fSal",
               "PHY_cyano_fSil","PHY_cyano_fT","PHY_diatom_fI","PHY_diatom_fNit","PHY_diatom_fPho","PHY_diatom_fSal",
               "PHY_diatom_fSil","PHY_diatom_fT")



plot_factors <- f_factors %>%
  pivot_longer(PHY_green_fI_1.6:PHY_diatom_fT_1.6, names_to = "var_name", values_to = "f_factor") %>%
  separate(var_name, c("PHY","group","factor_name","depth1","depth2")) %>%
  select(-c("PHY","depth1","depth2")) %>%
  filter(DateTime >= "2018-08-06") 

factor_plot <- ggplot(data = plot_factors, aes(x = DateTime, y = f_factor, group = factor_name, color = factor_name))+
  geom_line()+
  facet_grid(rows = vars(group))+
  theme_bw()
factor_plot